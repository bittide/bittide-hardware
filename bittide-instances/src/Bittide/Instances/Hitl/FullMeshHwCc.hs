-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

{- | Test whether clock boards are configurable and transceiver links come
online. If they do, run clock control and wait for the clocks to stabilize.
This assumes to run on a fully connected mesh of 8 FPGAs. Also see
'c_CHANNEL_NAMES' and 'c_CLOCK_PATHS'. It has two tricks up its sleeve:

  1. It uses @SYNC_IN@/@SYNC_OUT@ to make sure each board starts programming
     its clock boards at the same time.

  2. It keeps track of how many times the GTH's reset manager had to reset
     the connection and how often it lost connections after establishing
     them.

This test will succeed if all clocks have been stable for 5 seconds. Note:
this doesn't test reframing yet.
-}
module Bittide.Instances.Hitl.FullMeshHwCc (
  fullMeshHwCcWithRiscvTest,
  fullMeshHwCcTest,
  clockControlConfig,
  fullMeshHwCcWithRiscvTest',
  fullMeshHwCcTest',
) where

import Clash.Explicit.Prelude hiding (PeriodToCycles)
import qualified Clash.Explicit.Prelude as E
import Clash.Prelude (withClockResetEnable)

import Data.Proxy
import Language.Haskell.TH (runIO)
import LiftType (liftTypeQ)
import System.FilePath

import Bittide.Arithmetic.Time
import Bittide.ClockControl
import Bittide.ClockControl.Callisto
import Bittide.ClockControl.DebugRegister (DebugRegisterCfg (..), debugRegisterWb)
import Bittide.ClockControl.Registers (ClockControlData (clockMod), clockControlWb)
import Bittide.ClockControl.Si539xSpi (ConfigState (Error, Finished), si539xSpi)
import Bittide.Counter
import Bittide.DoubleBufferedRam (
  ContentType (Blob),
  InitialContent (Reloadable),
  RegisterWritePriority (CircuitPriority),
  registerWb,
 )
import Bittide.ElasticBuffer (sticky)
import Bittide.Hitl
import Bittide.Instances.Domains
import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Bittide.ProcessingElement.Util (memBlobsFromElf)
import Bittide.SharedTypes (ByteOrder (BigEndian), Bytes)
import Bittide.Simulate.Config (CcConf (..))
import Bittide.Topology (TopologyType (..))
import Bittide.Transceiver (transceiverPrbsN)

import Bittide.Instances.Hitl.HwCcTopologies (cSigMap, commonSpiConfig, csDupe)
import Bittide.Instances.Hitl.IlaPlot
import Bittide.Instances.Hitl.Setup
import Project.FilePath

import Clash.Annotations.TH (makeTopEntity)
import Clash.Class.Counter
import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.Ila (Depth (..), IlaConfig (..), ila, ilaConfig)
import Clash.Sized.Extra (unsignedToSigned)
import Clash.Xilinx.ClockGen

import Protocols
import Protocols.Wishbone
import VexRiscv

import qualified Bittide.Transceiver as Transceiver
import qualified Bittide.Transceiver.ResetManager as ResetManager

clockControlConfig ::
  $(case (instancesClockConfig (Proxy @Basic125)) of (_ :: t) -> liftTypeQ @t)
clockControlConfig =
  $(lift (instancesClockConfig (Proxy @Basic125)))

debugRegisterConfig :: DebugRegisterCfg
debugRegisterConfig =
  DebugRegisterCfg
    { reframingEnabled = False
    }

{- | Instantiates a RiscV core that copies instructions coming from a hardware
implementation of Callisto (see 'fullMeshHwTest') and copies it to a register
tied to FINC/FDEC.
-}
fullMeshRiscvCopyTest ::
  forall dom.
  (KnownDomain dom, 1 <= DomainPeriod dom) =>
  Clock dom ->
  Reset dom ->
  Signal dom (CallistoResult LinkCount) ->
  Vec LinkCount (Signal dom (RelDataCount 32)) ->
  -- Freq increase / freq decrease request to clock board
  ( "FINC" ::: Signal dom Bool
  , "FDEC" ::: Signal dom Bool
  )
fullMeshRiscvCopyTest clk rst callistoResult dataCounts = unbundle fIncDec
 where
  (_, ccData) =
    toSignals
      ( circuit $ \jtag -> do
          [wbFincFdec, wbClockControl, wbDebug] <-
            withClockResetEnable clk rst enableGen $ processingElement @dom peConfig -< jtag
          fIncDecCallisto -< wbFincFdec
          [ccd0, ccd1] <-
            csDupe
              <| withClockResetEnable
                clk
                rst
                enableGen
                (clockControlWb margin framesize (pure $ complement 0) dataCounts)
              -< wbClockControl
          cm <- cSigMap clockMod -< ccd0
          _debugData <-
            withClockResetEnable clk rst enableGen
              $ debugRegisterWb (pure debugRegisterConfig)
              -< (wbDebug, cm)
          idC -< ccd1
      )
      (pure $ JtagIn low low low, pure ())
  fIncDec = speedChangeToStickyPins clk rst enableGen (SNat @Si539xHoldTime) ccData.clockMod

  fIncDecCallisto ::
    forall aw nBytes.
    (KnownNat aw, nBytes ~ 4) =>
    Circuit
      (Wishbone dom 'Standard aw (Bytes nBytes))
      ()
  fIncDecCallisto = Circuit goFIncDecCallisto
   where
    goFIncDecCallisto (wbM2S, _) = (wbS2M, ())
     where
      (_, wbS2M) =
        withClockResetEnable clk rst enableGen
          $ registerWb
            CircuitPriority
            (0 :: Bytes nBytes, 0 :: Bytes nBytes)
            wbM2S
            (fmap (fmap ((,0) . extend . pack)) fincfdec)

      fincfdec :: Signal dom (Maybe SpeedChange)
      fincfdec =
        clearOnAck
          <$> fmap acknowledge wbS2M
          <*> fmap maybeSpeedChange callistoResult

      -- Clear register if register is read from (or written to, but we assume
      -- this doesn't happen). This makes sure the RiscV doesn't read the same
      -- result from the hardware clock control twice.
      clearOnAck :: ("ACK" ::: Bool) -> Maybe SpeedChange -> Maybe SpeedChange
      clearOnAck False maybeSpeedChange = maybeSpeedChange
      clearOnAck True (Just speedChange) = Just speedChange
      clearOnAck True Nothing = Just NoChange

  margin = d2

  framesize = SNat @(PeriodToCycles dom (Seconds 1))

  (iMem, dMem) =
    $( do
        root <- runIO $ findParentContaining "cabal.project"
        let
          elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" Release
          elfPath = elfDir </> "clock-control-reg-cpy"
          iSize = 64 * 1024 -- 64 KB
          dSize = 64 * 1024 -- 64 KB
        memBlobsFromElf BigEndian (Just iSize, Just dSize) elfPath Nothing
     )

  {-
    0b100xxxxx_xxxxxxxx 0b100 0x8x instruction memory
    0b010xxxxx_xxxxxxxx 0b010 0x4x data memory
    0b000xxxxx_xxxxxxxx 0b000 0x0x FINC/FDEC register
    0b110xxxxx_xxxxxxxx 0b110 0xCx memory mapped hardware clock control
    0b111xxxxx_xxxxxxxx 0b111 0xEx memory mapped debugging register
  -}
  peConfig =
    PeConfig
      (0b100 :> 0b010 :> 0b000 :> 0b110 :> 0b111 :> Nil)
      (Reloadable $ Blob iMem)
      (Reloadable $ Blob dMem)

{- | Instantiates a hardware implementation of Callisto and exports its results. Can
be used to drive FINC/FDEC directly (see @FINC_FDEC@ result) or to tie the
results to a RiscV core (see 'fullMeshRiscvCopyTest')
-}
fullMeshHwTest ::
  "SMA_MGT_REFCLK_C" ::: Clock Ext200 ->
  "SYSCLK" ::: Clock Basic125 ->
  "ILA_CTRL" ::: IlaControl Basic125 ->
  "GTH_RX_NS" ::: TransceiverWires GthRxS LinkCount ->
  "GTH_RX_PS" ::: TransceiverWires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTxS LinkCount
  , "GTH_TX_PS" ::: TransceiverWires GthTxS LinkCount
  , "FINC_FDEC" ::: Signal Basic125 (FINC, FDEC)
  , "CALLISTO_RESULT" ::: Signal Basic125 (CallistoResult LinkCount)
  , "CALLISTO_RESET" ::: Reset Basic125
  , "DATA_COUNTERS" ::: Vec LinkCount (Signal Basic125 (RelDataCount 32))
  , "stats" ::: Vec LinkCount (Signal Basic125 ResetManager.Statistics)
  , "spiDone" ::: Signal Basic125 Bool
  , ""
      ::: ( "SCLK" ::: Signal Basic125 Bool
          , "MOSI" ::: Signal Basic125 Bit
          , "CSB" ::: Signal Basic125 Bool
          )
  , "transceiversFailedAfterUp" ::: Signal Basic125 Bool
  , "ALL_READY" ::: Signal Basic125 Bool
  , "ALL_STABLE" ::: Signal Basic125 Bool
  )
fullMeshHwTest refClk sysClk IlaControl{syncRst = rst, ..} rxNs rxPs miso =
  fincFdecIla
    `hwSeqX` ( transceivers.txNs
             , transceivers.txPs
             , frequencyAdjustments
             , callistoResult
             , clockControlReset
             , domainDiffs
             , transceivers.stats
             , spiDone
             , spiOut
             , transceiversFailedAfterUp
             , allReady
             , allStable0
             )
 where
  syncRst = rst `orReset` (unsafeFromActiveLow (fmap not spiErr))

  -- Clock programming
  spiDone = E.dflipflop sysClk $ (== Finished) <$> spiState
  spiErr = E.dflipflop sysClk $ isErr <$> spiState

  isErr (Error _) = True
  isErr _ = False

  (_, _, spiState, spiOut) =
    withClockResetEnable sysClk syncRst enableGen
      $ si539xSpi commonSpiConfig (SNat @(Microseconds 10)) (pure Nothing) miso

  -- Transceiver setup
  gthAllReset = unsafeFromActiveLow spiDone

  transceivers =
    transceiverPrbsN
      @GthTx
      @GthRx
      @Ext200
      @Basic125
      @GthTxS
      @GthRxS
      Transceiver.defConfig
      Transceiver.Inputs
        { clock = sysClk
        , reset = gthAllReset
        , refClock = refClk
        , channelNames
        , clockPaths
        , rxNs
        , rxPs
        , txDatas = repeat (pure 0)
        , txReadys = repeat (pure False)
        , rxReadys = repeat (pure True)
        }

  allReady =
    trueFor (SNat @(Milliseconds 500)) sysClk syncRst (and <$> bundle transceivers.linkReadys)
  transceiversFailedAfterUp =
    sticky sysClk syncRst (isFalling sysClk syncRst enableGen False allReady)

  timeSucc = countSucc @(Unsigned 16, Index (PeriodToCycles Basic125 (Milliseconds 1)))
  timer = register sysClk syncRst enableGen (0, 0) (timeSucc <$> timer)
  milliseconds1 = fst <$> timer

  -- Clock control
  clockControlReset =
    orReset (unsafeFromActiveLow allReady)
      $ orReset
        (unsafeFromActiveHigh transceiversFailedAfterUp)
        (unsafeFromActiveLow syncStart)

  availableLinkMask = pure maxBound

  (clockMod, _stabilities, allStable0, _allCentered) =
    unbundle
      $ fmap
        (\CallistoResult{..} -> (maybeSpeedChange, stability, allStable, allSettled))
        callistoResult

  callistoResult =
    callistoClockControlWithIla @LinkCount @CccBufferSize
      (head transceivers.txClocks)
      sysClk
      clockControlReset
      clockControlConfig
      IlaControl{..}
      availableLinkMask
      (fmap (fmap resize) domainDiffs)

  -- Capture every 100 microseconds - this should give us a window of about 5
  -- seconds. Or: when we're in reset. If we don't do the latter, the VCDs get
  -- very confusing.
  capture = (captureFlag .&&. allReady) .||. unsafeToActiveHigh syncRst

  fincFdecIla :: Signal Basic125 ()
  fincFdecIla =
    setName @"fincFdecIla"
      $ ila
        ( ilaConfig
            $ "trigger_0"
            :> "capture_0"
            :> "probe_milliseconds"
            :> "probe_allStable0"
            :> "probe_transceiversFailedAfterUp"
            :> "probe_nFincs"
            :> "probe_nFdecs"
            :> "probe_net_nFincs"
            :> Nil
        )
          { depth = D16384
          }
        sysClk
        -- Trigger as soon as we come out of reset
        (unsafeToActiveLow syncRst)
        capture
        -- Debug probes
        milliseconds1
        allStable0
        transceiversFailedAfterUp
        nFincs
        nFdecs
        (fmap unsignedToSigned nFincs - fmap unsignedToSigned nFdecs)

  captureFlag =
    riseEvery
      sysClk
      syncRst
      enableGen
      (SNat @(PeriodToCycles Basic125 (Milliseconds 1)))

  nFincs =
    regEn
      sysClk
      clockControlReset
      enableGen
      (0 :: Unsigned 32)
      ((== Just SpeedUp) <$> clockMod)
      (satSucc SatBound <$> nFincs)

  nFdecs =
    regEn
      sysClk
      clockControlReset
      enableGen
      (0 :: Unsigned 32)
      ((== Just SlowDown) <$> clockMod)
      (satSucc SatBound <$> nFdecs)

  frequencyAdjustments :: Signal Basic125 (FINC, FDEC)
  frequencyAdjustments =
    E.delay sysClk enableGen minBound {- glitch filter -}
      $ speedChangeToStickyPins
        sysClk
        clockControlReset
        enableGen
        (SNat @Si539xHoldTime)
        clockMod

  domainDiffs =
    domainDiffCounterExt sysClk clockControlReset
      <$> transceivers.rxClocks
      <*> transceivers.txClocks

-- | Top entity for this test. See module documentation for more information.
fullMeshHwCcWithRiscvTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "SYNC_IN" ::: Signal Basic125 Bool ->
  "GTH_RX_NS" ::: TransceiverWires GthRxS LinkCount ->
  "GTH_RX_PS" ::: TransceiverWires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTxS LinkCount
  , "GTH_TX_PS" ::: TransceiverWires GthTxS LinkCount
  , ""
      ::: ( "FINC" ::: Signal Basic125 Bool
          , "FDEC" ::: Signal Basic125 Bool
          )
  , "SYNC_OUT" ::: Signal Basic125 Bool
  , "spiDone" ::: Signal Basic125 Bool
  , ""
      ::: ( "SCLK" ::: Signal Basic125 Bool
          , "MOSI" ::: Signal Basic125 Bit
          , "CSB" ::: Signal Basic125 Bool
          )
  )
fullMeshHwCcWithRiscvTest refClkDiff sysClkDiff syncIn rxns rxps miso =
  (txns, txps, (riscvFinc, riscvFdec), syncOut, spiDone, spiOut)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Ext200

  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset

  ilaControl@IlaControl{..} = ilaPlotSetup IlaPlotSetup{..}

  ( txns
    , txps
    , _hwFincFdecs
    , callistoResult
    , callistoReset
    , dataCounts
    , _stats
    , spiDone
    , spiOut
    , transceiversFailedAfterUp
    , allReady
    , allStable
    ) = fullMeshHwTest refClk sysClk ilaControl rxns rxps miso

  (riscvFinc, riscvFdec) =
    fullMeshRiscvCopyTest sysClk callistoReset callistoResult dataCounts

  -- check that tests are not synchronously start before all
  -- transceivers are up
  startBeforeAllReady =
    sticky
      sysClk
      syncRst
      (startTest .&&. syncStart .&&. ((not <$> allReady) .||. transceiversFailedAfterUp))

  endSuccess :: Signal Basic125 Bool
  endSuccess = trueFor (SNat @(Seconds 5)) sysClk syncRst allStable

  done = endSuccess .||. transceiversFailedAfterUp .||. startBeforeAllReady
  success = not <$> (transceiversFailedAfterUp .||. startBeforeAllReady)

  startTest :: Signal Basic125 Bool
  startTest = hitlVioBool sysClk done success

makeTopEntity 'fullMeshHwCcWithRiscvTest

-- | Top entity for this test. See module documentation for more information.
fullMeshHwCcTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "SYNC_IN" ::: Signal Basic125 Bool ->
  "GTH_RX_NS" ::: TransceiverWires GthRxS LinkCount ->
  "GTH_RX_PS" ::: TransceiverWires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTxS LinkCount
  , "GTH_TX_PS" ::: TransceiverWires GthTxS LinkCount
  , ""
      ::: ( "FINC" ::: Signal Basic125 Bool
          , "FDEC" ::: Signal Basic125 Bool
          )
  , "SYNC_OUT" ::: Signal Basic125 Bool
  , "spiDone" ::: Signal Basic125 Bool
  , ""
      ::: ( "SCLK" ::: Signal Basic125 Bool
          , "MOSI" ::: Signal Basic125 Bit
          , "CSB" ::: Signal Basic125 Bool
          )
  )
fullMeshHwCcTest refClkDiff sysClkDiff syncIn rxns rxps miso =
  (txns, txps, unbundle hwFincFdecs, syncOut, spiDone, spiOut)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Ext200
  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset
  ilaControl@IlaControl{..} = ilaPlotSetup IlaPlotSetup{..}

  ( txns
    , txps
    , hwFincFdecs
    , _callistoResult
    , _callistoReset
    , _dataCounts
    , _stats
    , spiDone
    , spiOut
    , transceiversFailedAfterUp
    , allReady
    , allStable
    ) = fullMeshHwTest refClk sysClk ilaControl rxns rxps miso

  -- check that tests are not synchronously start before all
  -- transceivers are up
  startBeforeAllReady =
    sticky
      sysClk
      syncRst
      (syncStart .&&. ((not <$> allReady) .||. transceiversFailedAfterUp))

  endSuccess :: Signal Basic125 Bool
  endSuccess = trueFor (SNat @(Seconds 5)) sysClk syncRst allStable

  startTest :: Signal Basic125 Bool
  startTest =
    hitlVioBool
      sysClk
      -- done
      (endSuccess .||. transceiversFailedAfterUp .||. startBeforeAllReady)
      -- success
      (not <$> (transceiversFailedAfterUp .||. startBeforeAllReady))

makeTopEntity 'fullMeshHwCcTest

mkTest :: ClashTargetName -> HitlTestGroup
mkTest topEntity =
  HitlTestGroup
    { topEntity = topEntity
    , extraXdcFiles = []
    , externalHdl = []
    , testCases =
        [ HitlTestCase
            { name = "CC"
            , parameters = paramForHwTargets allHwTargets ()
            , postProcData =
                def
                  { ccTopologyType = Complete (natToInteger @FpgaCount)
                  , samples = 1000
                  , duration = natToNum @(PeriodToCycles Basic125 (Seconds 60))
                  , stabilityMargin = snatToNum cccStabilityCheckerMargin
                  , stabilityFrameSize = snatToNum cccStabilityCheckerFramesize
                  , reframe = cccEnableReframing
                  , waitTime = fromEnum cccReframingWaitTime
                  , clockOffsets = Nothing
                  , startupDelays = toList $ repeat @FpgaCount 0
                  }
            }
        ]
    , mPostProc = Nothing
    }
 where
  ClockControlConfig{..} = clockControlConfig

fullMeshHwCcWithRiscvTest' :: HitlTestGroup
fullMeshHwCcWithRiscvTest' = mkTest 'fullMeshHwCcWithRiscvTest

fullMeshHwCcTest' :: HitlTestGroup
fullMeshHwCcTest' = mkTest 'fullMeshHwCcTest
