-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}

-- | Test whether clock boards are configurable and transceiver links come
-- online. If they do, run clock control and wait for the clocks to stabilize.
-- This assumes to run on a fully connected mesh of 8 FPGAs. Also see
-- 'c_CHANNEL_NAMES' and 'c_CLOCK_PATHS'. It has two tricks up its sleeve:
--
--   1. It uses @SYNC_IN@/@SYNC_OUT@ to make sure each board starts programming
--      its clock boards at the same time.
--
--   2. It keeps track of how many times the GTH's reset manager had to reset
--      the connection and how often it lost connections after establishing
--      them.
--
-- This test will succeed if all clocks have been stable for 5 seconds. Note:
-- this doesn't test reframing yet.
--
module Bittide.Instances.Hitl.FullMeshHwCc
  ( fullMeshHwCcWithRiscvTest
  , fullMeshHwCcTest
  , clockControlConfig
  , tests
  ) where

import Clash.Prelude (withClockResetEnable)
import Clash.Explicit.Prelude
import qualified Clash.Explicit.Prelude as E

import Data.Maybe (fromMaybe)
import Data.Proxy
import Language.Haskell.TH (runIO)
import LiftType (liftTypeQ)
import System.FilePath

import Bittide.Arithmetic.Time
import Bittide.ClockControl
import Bittide.ClockControl.Callisto
import Bittide.ClockControl.Callisto.Util (FINC, FDEC, stickyBits, speedChangeToPins)
import Bittide.ClockControl.Registers (clockControlWb)
import Bittide.ClockControl.Si5395J
import Bittide.ClockControl.Si539xSpi (ConfigState(Error, Finished), si539xSpi)
import Bittide.DoubleBufferedRam
  ( InitialContent(Reloadable), ContentType(Blob)
  , RegisterWritePriority(CircuitPriority)
  , registerWb
  )
import Bittide.Counter
import Bittide.ElasticBuffer (sticky)
import Bittide.Hitl (HitlTestsWithPostProcData, hitlVioBool, allFpgas)
import Bittide.Instances.Domains
import Bittide.ProcessingElement (PeConfig(..), processingElement)
import Bittide.ProcessingElement.Util (memBlobsFromElf)
import Bittide.Simulate.Config (SimConf(..))
import Bittide.SharedTypes (Bytes, ByteOrder(BigEndian))
import Bittide.Topology (TopologyType(..))
import Bittide.Transceiver (transceiverPrbsN)

import Bittide.Instances.Hitl.IlaPlot
import Bittide.Instances.Hitl.Setup
import Project.FilePath

import Clash.Annotations.TH (makeTopEntity)
import Clash.Class.Counter
import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.Ila (IlaConfig(..), Depth(..), ila, ilaConfig)
import Clash.Sized.Extra (unsignedToSigned)
import Clash.Xilinx.ClockGen

import Protocols
import Protocols.Wishbone
import VexRiscv

import qualified Bittide.Transceiver as Transceiver
import qualified Bittide.Transceiver.ResetManager as ResetManager
import qualified Data.Map as Map (singleton)

clockControlConfig ::
  $(case (instancesClockConfig (Proxy @Basic125)) of { (_ :: t) -> liftTypeQ @t })
clockControlConfig =
  $(lift (instancesClockConfig (Proxy @Basic125)))

-- | Instantiates a RiscV core that copies instructions coming from a hardware
-- implementation of Callisto (see 'fullMeshHwTest') and copies it to a register
-- tied to FINC/FDEC.
fullMeshRiscvCopyTest ::
  forall dom .
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  Signal dom (CallistoResult (FpgaCount - 1)) ->
  Vec (FpgaCount - 1) (Signal dom (DataCount 32)) ->
  -- Freq increase / freq decrease request to clock board
  ( "FINC" ::: Signal dom Bool
  , "FDEC" ::: Signal dom Bool
  )
fullMeshRiscvCopyTest clk rst callistoResult dataCounts = unbundle fIncDec
 where
  (_, fIncDec) = toSignals
    ( circuit $ \jtag -> do
      [wbA, wbB] <- withClockResetEnable clk rst enableGen $ processingElement @dom peConfig -< jtag
      fIncDecCallisto -< wbA
      (fIncDec, _allStable) <- withClockResetEnable clk rst enableGen $
        clockControlWb margin framesize (pure $ complement 0) dataCounts -< wbB
      idC -< fIncDec
    ) (pure $ JtagIn low low low, pure ())

  fIncDecCallisto ::
    forall aw nBytes .
    (KnownNat aw, 2 <= aw, nBytes ~ 4) =>
    Circuit
      (Wishbone dom 'Standard aw (Bytes nBytes))
      ()
  fIncDecCallisto = Circuit goFIncDecCallisto
   where
    goFIncDecCallisto (wbM2S, _) = (wbS2M, ())
     where
      (_, wbS2M) = withClockResetEnable clk rst enableGen $
        registerWb
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
      clearOnAck False maybeSpeedChange   = maybeSpeedChange
      clearOnAck True  (Just speedChange) = Just speedChange
      clearOnAck True  Nothing            = Just NoChange

  margin = d2

  framesize = SNat @(PeriodToCycles dom (Seconds 1))

  (iMem, dMem) = $(do
    root <- runIO $ findParentContaining "cabal.project"
    let
      elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" Release
      elfPath = elfDir </> "clock-control-reg-cpy"
      iSize = 64 * 1024 -- 64 KB
      dSize = 64 * 1024 -- 64 KB
    memBlobsFromElf BigEndian (Just iSize, Just dSize) elfPath Nothing)

  {-
    0b10xxxxx_xxxxxxxx 0b10 0x8x instruction memory
    0b01xxxxx_xxxxxxxx 0b01 0x4x data memory
    0b00xxxxx_xxxxxxxx 0b00 0x0x FINC/FDEC register
    0b11xxxxx_xxxxxxxx 0b11 0xCx memory mapped hardware clock control
  -}
  peConfig =
    PeConfig
      (0b10 :> 0b01 :> 0b00 :> 0b11 :> Nil)
      (Reloadable $ Blob iMem)
      (Reloadable $ Blob dMem)

-- | Instantiates a hardware implementation of Callisto and exports its results. Can
-- be used to drive FINC/FDEC directly (see @FINC_FDEC@ result) or to tie the
-- results to a RiscV core (see 'fullMeshRiscvCopyTest')
fullMeshHwTest ::
  "SMA_MGT_REFCLK_C" ::: Clock Ext200 ->
  "SYSCLK" ::: Clock Basic125 ->
  "ILA_CTRL" ::: IlaControl Basic125 ->
  "GTH_RX_NS" ::: TransceiverWires GthRx ->
  "GTH_RX_PS" ::: TransceiverWires GthRx ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTx
  , "GTH_TX_PS" ::: TransceiverWires GthTx
  , "FINC_FDEC" ::: Signal Basic125 (FINC, FDEC)
  , "CALLISTO_RESULT" ::: Signal Basic125 (CallistoResult (FpgaCount - 1))
  , "CALLISTO_RESET" ::: Reset Basic125
  , "DATA_COUNTERS" ::: Vec (FpgaCount - 1) (Signal Basic125 (DataCount 32))
  , "stats" ::: Vec (FpgaCount - 1) (Signal Basic125 ResetManager.Statistics)
  , "spiDone" ::: Signal Basic125 Bool
  , "" :::
      ( "SCLK" ::: Signal Basic125 Bool
      , "MOSI" ::: Signal Basic125 Bit
      , "CSB"  ::: Signal Basic125 Bool
      )
  , "transceiversFailedAfterUp" ::: Signal Basic125 Bool
  , "ALL_READY" ::: Signal Basic125 Bool
  , "ALL_STABLE"   ::: Signal Basic125 Bool
  )
fullMeshHwTest refClk sysClk IlaControl{syncRst = rst, ..} rxNs rxPs miso =
  fincFdecIla `hwSeqX`
  ( transceivers.txNs
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
  spiDone = E.dflipflop sysClk $ (==Finished) <$> spiState
  spiErr = E.dflipflop sysClk $ isErr <$> spiState

  isErr (Error _) = True
  isErr _         = False

  (_, _, spiState, spiOut) =
    withClockResetEnable sysClk syncRst enableGen $
      si539xSpi testConfig6_200_on_0a_10ppb (SNat @(Microseconds 10)) (pure Nothing) miso

  -- Transceiver setup
  gthAllReset = unsafeFromActiveLow spiDone

  transceivers =
    transceiverPrbsN
      @GthTx @GthRx @Ext200 @Basic125 @GthTx @GthRx
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

  allReady = trueFor (SNat @(Milliseconds 500)) sysClk syncRst (and <$> bundle transceivers.linkReadys)
  transceiversFailedAfterUp =
    sticky sysClk syncRst (isFalling sysClk syncRst enableGen False allReady)

  timeSucc = countSucc @(Unsigned 16, Index (PeriodToCycles Basic125 (Milliseconds 1)))
  timer = register sysClk syncRst enableGen (0, 0) (timeSucc <$> timer)
  milliseconds1 = fst <$> timer

  -- Clock control
  clockControlReset =
      orReset (unsafeFromActiveLow allReady)
    $ orReset (unsafeFromActiveHigh transceiversFailedAfterUp)
              (unsafeFromActiveLow syncStart)

  availableLinkMask = pure maxBound

  (clockMod, _stabilities, allStable0, _allCentered) = unbundle $
    fmap
      (\CallistoResult{..} -> (maybeSpeedChange, stability, allStable, allSettled))
      callistoResult

  callistoResult =
    callistoClockControlWithIla @(FpgaCount - 1) @CccBufferSize
      (head transceivers.txClocks) sysClk clockControlReset clockControlConfig
      IlaControl{..} availableLinkMask (fmap (fmap resize) domainDiffs)

  -- Capture every 100 microseconds - this should give us a window of about 5
  -- seconds. Or: when we're in reset. If we don't do the latter, the VCDs get
  -- very confusing.
  capture = (captureFlag .&&. allReady) .||. unsafeToActiveHigh syncRst

  fincFdecIla :: Signal Basic125 ()
  fincFdecIla = setName @"fincFdecIla" $ ila
    (ilaConfig $
         "trigger_0"
      :> "capture_0"
      :> "probe_milliseconds"
      :> "probe_allStable0"
      :> "probe_transceiversFailedAfterUp"
      :> "probe_nFincs"
      :> "probe_nFdecs"
      :> "probe_net_nFincs"
      :> Nil
    ){depth = D16384}
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

  captureFlag = riseEvery sysClk syncRst enableGen
    (SNat @(PeriodToCycles Basic125 (Milliseconds 1)))

  nFincs = regEn sysClk clockControlReset enableGen
    (0 :: Unsigned 32)
    ((== Just SpeedUp) <$> clockMod)
    (satSucc SatBound <$> nFincs)

  nFdecs = regEn sysClk clockControlReset enableGen
    (0 :: Unsigned 32)
    ((== Just SlowDown) <$> clockMod)
    (satSucc SatBound <$> nFdecs)

  frequencyAdjustments :: Signal Basic125 (FINC, FDEC)
  frequencyAdjustments =
    E.delay sysClk enableGen minBound {- glitch filter -} $
      withClockResetEnable sysClk clockControlReset enableGen $
        stickyBits @Basic125 d20 (speedChangeToPins . fromMaybe NoChange <$> clockMod)

  domainDiffs =
    domainDiffCounterExt sysClk clockControlReset
      <$> transceivers.rxClocks
      <*> transceivers.txClocks

-- | Top entity for this test. See module documentation for more information.
fullMeshHwCcWithRiscvTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_300" ::: DiffClock Ext300 ->
  "SYNC_IN" ::: Signal Basic125 Bool ->
  "GTH_RX_NS" ::: TransceiverWires GthRx ->
  "GTH_RX_PS" ::: TransceiverWires GthRx ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTx
  , "GTH_TX_PS" ::: TransceiverWires GthTx
  , "" :::
      ( "FINC"      ::: Signal Basic125 Bool
      , "FDEC"      ::: Signal Basic125 Bool
      )
  , "SYNC_OUT" ::: Signal Basic125 Bool
  , "spiDone" ::: Signal Basic125 Bool
  , "" :::
      ( "SCLK"      ::: Signal Basic125 Bool
      , "MOSI"      ::: Signal Basic125 Bit
      , "CSB"       ::: Signal Basic125 Bool
      )
  )
fullMeshHwCcWithRiscvTest refClkDiff sysClkDiff syncIn rxns rxps miso =
  (txns, txps, (riscvFinc, riscvFdec), syncOut, spiDone, spiOut)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Ext200

  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset

  ilaControl@IlaControl{..} = ilaPlotSetup IlaPlotSetup{..}

  (   txns, txps, _hwFincFdecs, callistoResult, callistoReset
    , dataCounts, _stats, spiDone, spiOut, transceiversFailedAfterUp, allReady
    , allStable ) = fullMeshHwTest refClk sysClk ilaControl rxns rxps miso

  (riscvFinc, riscvFdec) =
    fullMeshRiscvCopyTest sysClk callistoReset callistoResult dataCounts

  -- check that tests are not synchronously start before all
  -- transceivers are up
  startBeforeAllReady = sticky sysClk syncRst
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
  "SYSCLK_300" ::: DiffClock Ext300 ->
  "SYNC_IN" ::: Signal Basic125 Bool ->
  "GTH_RX_NS" ::: TransceiverWires GthRx ->
  "GTH_RX_PS" ::: TransceiverWires GthRx ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTx
  , "GTH_TX_PS" ::: TransceiverWires GthTx
  , "" :::
      ( "FINC"      ::: Signal Basic125 Bool
      , "FDEC"      ::: Signal Basic125 Bool
      )
  , "SYNC_OUT" ::: Signal Basic125 Bool
  , "spiDone" ::: Signal Basic125 Bool
  , "" :::
      ( "SCLK"      ::: Signal Basic125 Bool
      , "MOSI"      ::: Signal Basic125 Bit
      , "CSB"       ::: Signal Basic125 Bool
      )
  )
fullMeshHwCcTest refClkDiff sysClkDiff syncIn rxns rxps miso =
  (txns, txps, unbundle hwFincFdecs, syncOut, spiDone, spiOut)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Ext200
  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset
  ilaControl@IlaControl{..} = ilaPlotSetup IlaPlotSetup{..}

  (   txns, txps, hwFincFdecs, _callistoResult, _callistoReset
    , _dataCounts, _stats, spiDone, spiOut, transceiversFailedAfterUp, allReady
    , _allStable ) = fullMeshHwTest refClk sysClk ilaControl rxns rxps miso

  -- check that tests are not synchronously start before all
  -- transceivers are up
  startBeforeAllReady = sticky sysClk syncRst
    (syncStart .&&. ((not <$> allReady) .||. transceiversFailedAfterUp))

  testDone :: Signal Basic125 Bool
  testDone = sticky sysClk syncRst driftTestEnd
   where
    driftTestEnd = (fst <$> ilaControl.globalTimestamp) .==. (natToNum @DriftTestEndCount)

  startTest :: Signal Basic125 Bool
  startTest =
    hitlVioBool
      sysClk

      -- done
      (testDone .||. transceiversFailedAfterUp .||. startBeforeAllReady)

      -- success
      (not <$> (transceiversFailedAfterUp .||. startBeforeAllReady))
makeTopEntity 'fullMeshHwCcTest

tests :: HitlTestsWithPostProcData () SimConf
tests = Map.singleton "CC" $
  ( allFpgas ()
  , def { mTopologyType      = Just $ Complete (natToInteger @FpgaCount)
        , samples            = 1000
        , duration           = natToNum @(PeriodToCycles Basic125 (Seconds 60))
        , stabilityMargin    = snatToNum cccStabilityCheckerMargin
        , stabilityFrameSize = snatToNum cccStabilityCheckerFramesize
        , reframe            = cccEnableReframing
        , rusty              = cccEnableRustySimulation
        , waitTime           = fromEnum cccReframingWaitTime
        , clockOffsets       = toList $ repeat @FpgaCount 0
        , startupDelays      = toList $ repeat @FpgaCount 0
        }
  )
 where
  ClockControlConfig{..} = clockControlConfig
