-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}

-- | Test whether clock boards are configurable and transceiver links come
-- online. If they do, run clock control and wait for the clocks to stabilize.
-- Also see  'Bittide.Instances.Hitl.Setup'. It has two tricks up its
-- sleeve:
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
module Bittide.Instances.Hitl.HwCcTopologies
  ( hwCcTopologyWithRiscvTest
  , hwCcTopologyTest
  , clockControlConfig
  , tests
  ) where

import Clash.Prelude (withClockResetEnable)
import Clash.Explicit.Prelude
import qualified Clash.Explicit.Prelude as E

import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import Data.String (fromString)
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
  ( InitialContent(Reloadable), ContentType(Blob), RegisterWritePriority(CircuitPriority)
  , registerWb
  )
import Bittide.Counter
import Bittide.ElasticBuffer (sticky)
import Bittide.Instances.Domains
import Bittide.ProcessingElement (PeConfig(..), processingElement)
import Bittide.ProcessingElement.Util (memBlobsFromElf)
import Bittide.Simulate.Config (SimConf(..))
import Bittide.SharedTypes (Bytes, ByteOrder(BigEndian))
import Bittide.Transceiver
import Bittide.Topology

import Bittide.Hitl (HitlTestsWithPostProcData, TestName, Probes, hitlVio)

import Bittide.Instances.Hitl.IlaPlot
import Bittide.Instances.Hitl.Setup
import Project.FilePath

import Clash.Class.Counter
import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.Ila (IlaConfig(..), Depth(..), ila, ilaConfig)
import Clash.Cores.Xilinx.Xpm.Cdc.Single
import Clash.Sized.Extra (unsignedToSigned)
import Clash.Xilinx.ClockGen

import Protocols hiding (SimulationConfig)
import Protocols.Wishbone
import VexRiscv

import qualified Data.Map.Strict as Map (fromList)

type AllStablePeriod = Seconds 5

data TestConfig =
  TestConfig
    { fpgaEnabled :: Bool
    , mask :: BitVector (FpgaCount - 1)
    }
  deriving (Generic, NFDataX, BitPack)

clockControlConfig ::
  $(case (instancesClockConfig (Proxy @Basic125)) of { (_ :: t) -> liftTypeQ @t })
clockControlConfig =
  $(lift (instancesClockConfig (Proxy @Basic125)))

-- | Instantiates a RiscV core that copies instructions coming from a hardware
-- implementation of Callisto (see 'topologyTest') and copies it to a register
-- tied to FINC/FDEC.
riscvCopyTest ::
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
riscvCopyTest clk rst callistoResult dataCounts = unbundle fIncDec
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
    memBlobsFromElf BigEndian (Nothing, Nothing) elfPath Nothing)

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
-- results to a RiscV core (see 'riscvCopyTest')
topologyTest ::
  "SMA_MGT_REFCLK_C" ::: Clock Ext200 ->
  "SYSCLK" ::: Clock Basic125 ->
  "ILA_CTRL" ::: IlaControl Basic125 ->
  "GTH_RX_NS" ::: TransceiverWires GthRx ->
  "GTH_RX_PS" ::: TransceiverWires GthRx ->
  "MISO" ::: Signal Basic125 Bit ->
  "LINKS" ::: Signal Basic125 (BitVector (FpgaCount - 1)) ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTx
  , "GTH_TX_PS" ::: TransceiverWires GthTx
  , "FINC_FDEC" ::: Signal Basic125 (FINC, FDEC)
  , "CALLISTO_RESULT" ::: Signal Basic125 (CallistoResult (FpgaCount - 1))
  , "CALLISTO_RESET" ::: Reset Basic125
  , "DATA_COUNTERS" ::: Vec (FpgaCount - 1) (Signal Basic125 (DataCount 32))
  , "stats" ::: Vec (FpgaCount - 1) (Signal Basic125 GthResetStats)
  , "spiDone" ::: Signal Basic125 Bool
  , "" :::
      ( "SCLK" ::: Signal Basic125 Bool
      , "MOSI" ::: Signal Basic125 Bit
      , "CSB"  ::: Signal Basic125 Bool
      )
  , "transceiversFailedAfterUp" ::: Signal Basic125 Bool
  , "ALL_UP" ::: Signal Basic125 Bool
  , "ALL_STABLE"   ::: Signal Basic125 Bool
  )
topologyTest refClk sysClk IlaControl{syncRst = rst, ..} rxns rxps miso mask =
  fincFdecIla `hwSeqX`
  ( txns
  , txps
  , frequencyAdjustments
  , callistoResult
  , clockControlReset
  , domainDiffs
  , stats
  , spiDone
  , spiOut
  , transceiversFailedAfterUp
  , allUp
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
      si539xSpi testConfig6_200_on_0a_10ppb
        (SNat @(Microseconds 10)) (pure Nothing) miso

  -- Transceiver setup
  gthAllReset = unsafeFromActiveLow spiDone

  (txClocks, rxClocks, txns, txps, linkUpsRx, stats) = unzip6 $
    transceiverPrbsN
      @GthTx @GthRx @Ext200 @Basic125 @GthTx @GthRx
      refClk sysClk gthAllReset
      channelNames clockPaths rxns rxps

  syncLink rxClock linkUp = xpmCdcSingle rxClock sysClk linkUp
  linkUps = zipWith syncLink rxClocks linkUpsRx
  allUp = trueFor (SNat @(Milliseconds 500)) sysClk syncRst (and <$> bundle linkUps)
  transceiversFailedAfterUp =
    sticky sysClk syncRst (isFalling sysClk syncRst enableGen False allUp)

  timeSucc = countSucc @(Unsigned 16, Index (PeriodToCycles Basic125 (Milliseconds 1)))
  timer = register sysClk syncRst enableGen (0, 0) (timeSucc <$> timer)
  milliseconds1 = fst <$> timer

  -- Clock control
  clockControlReset =
      orReset (unsafeFromActiveLow allUp)
    $ orReset (unsafeFromActiveHigh transceiversFailedAfterUp)
              (unsafeFromActiveLow syncStart)

  (clockMod, _stabilities, allStable0, _allCentered) = unbundle $
    fmap
      (\CallistoResult{..} -> (maybeSpeedChange, stability, allStable, allSettled))
      callistoResult

  callistoResult =
    callistoClockControlWithIla @(FpgaCount - 1) @CccBufferSize
      (head txClocks) sysClk clockControlReset clockControlConfig
      IlaControl{..} mask (fmap (fmap resize) domainDiffs)

  -- Capture every 100 microseconds - this should give us a window of about 5
  -- seconds. Or: when we're in reset. If we don't do the latter, the VCDs get
  -- very confusing.
  capture = (captureFlag .&&. allUp) .||. unsafeToActiveHigh syncRst

  fincFdecIla :: Signal Basic125 ()
  fincFdecIla = ila
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
      <$> rxClocks
      <*> txClocks

-- | Top entity for this test. See module documentation for more information.
hwCcTopologyWithRiscvTest ::
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
hwCcTopologyWithRiscvTest refClkDiff sysClkDiff syncIn rxns rxps miso =
  (txns, txps, (riscvFinc, riscvFdec), syncOut, spiDone, spiOut)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Ext200

  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset

  ilaControl@IlaControl{..} = ilaPlotSetup IlaPlotSetup{..}
  startTest = isJust <$> testConfig

  (   txns, txps, _hwFincFdecs, callistoResult, callistoReset
    , dataCounts, _stats, spiDone, spiOut, transceiversFailedAfterUp, allUp
    , allStable ) = topologyTest refClk sysClk ilaControl { skipTest = skip }
                      rxns rxps miso (maybe 0 mask <$> testConfig)

  (riscvFinc, riscvFdec) =
    riscvCopyTest sysClk callistoReset callistoResult dataCounts

  -- check that tests are not synchronously start before all
  -- transceivers are up
  startBeforeAllUp = sticky sysClk syncRst
    (startTest .&&. syncStart .&&. ((not <$> allUp) .||. transceiversFailedAfterUp))

  endSuccess :: Signal Basic125 Bool
  endSuccess = trueFor (SNat @AllStablePeriod) sysClk syncRst allStable

  done = endSuccess .||. transceiversFailedAfterUp .||. startBeforeAllUp
  success = not <$> (transceiversFailedAfterUp .||. startBeforeAllUp)

  skip =
    register sysClk sysRst enableGen False
      (maybe False (not . fpgaEnabled) <$> testConfig)

  testConfig :: Signal Basic125 (Maybe TestConfig)
  testConfig = hitlVio disabled sysClk done success

-- XXX: We use an explicit top entity annotation here, as 'makeTopEntity'
--      generates warnings in combination with 'Vec'.
{-# ANN hwCcTopologyWithRiscvTest Synthesize
  { t_name = "hwCcTopologyWithRiscvTest"
  , t_inputs =
    [ (PortProduct "SMA_MGT_REFCLK_C") [PortName "p", PortName "n"]
    , (PortProduct "SYSCLK_300") [PortName "p", PortName "n"]
    , PortName "SYNC_IN"
    , PortName "GTH_RX_NS"
    , PortName "GTH_RX_PS"
    , PortName "MISO"
    ]
  , t_output =
    (PortProduct "")
      [ PortName "GTH_TX_NS"
      , PortName "GTH_TX_PS"
      , PortProduct "" [PortName "FINC", PortName "FDEC"]
      , PortName "SYNC_OUT"
      , PortName "spiDone"
      , (PortProduct "") [PortName "SCLK", PortName "MOSI", PortName "CSB"]
      ]
  } #-}

-- | Top entity for this test. See module documentation for more information.
hwCcTopologyTest ::
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
hwCcTopologyTest refClkDiff sysClkDiff syncIn rxns rxps miso =
  (txns, txps, unbundle hwFincFdecs, syncOut, spiDone, spiOut)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Ext200
  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset
  ilaControl@IlaControl{..} = ilaPlotSetup IlaPlotSetup{..}
  startTest = isJust <$> testConfig

  (   txns, txps, hwFincFdecs, _callistoResult, _callistoReset
    , _dataCounts, _stats, spiDone, spiOut, transceiversFailedAfterUp, allUp
    , allStable ) = topologyTest refClk sysClk ilaControl { skipTest = skip }
                      rxns rxps miso (maybe 0 mask <$> testConfig)

  -- check that tests are not synchronously start before all
  -- transceivers are up
  startBeforeAllUp = sticky sysClk syncRst
    (syncStart .&&. ((not <$> allUp) .||. transceiversFailedAfterUp))

  endSuccess :: Signal Basic125 Bool
  endSuccess = trueFor (SNat @(Seconds 5)) sysClk syncRst allStable

  skip = maybe False (not . fpgaEnabled) <$> testConfig

  testConfig :: Signal Basic125 (Maybe TestConfig)
  testConfig =
    hitlVio
      disabled
      sysClk
      -- done
      (startTest .&&.
         (skip .||. endSuccess .||. transceiversFailedAfterUp .||. startBeforeAllUp))
      -- success
      (skip .||.
        (allStable .&&. (not <$> (transceiversFailedAfterUp .||. startBeforeAllUp))))

-- XXX: We use an explicit top entity annotation here, as 'makeTopEntity'
--      generates warnings in combination with 'Vec'.
{-# ANN hwCcTopologyTest Synthesize
  { t_name = "hwCcTopologyTest"
  , t_inputs =
    [ (PortProduct "SMA_MGT_REFCLK_C") [PortName "p", PortName "n"]
    , (PortProduct "SYSCLK_300") [PortName "p", PortName "n"]
    , PortName "SYNC_IN"
    , PortName "GTH_RX_NS"
    , PortName "GTH_RX_PS"
    , PortName "MISO"
    ]
  , t_output =
    (PortProduct "")
      [ PortName "GTH_TX_NS"
      , PortName "GTH_TX_PS"
      , PortProduct "" [PortName "FINC", PortName "FDEC"]
      , PortName "SYNC_OUT"
      , PortName "spiDone"
      , (PortProduct "") [PortName "SCLK", PortName "MOSI", PortName "CSB"]
      ]
  } #-}

tests :: HitlTestsWithPostProcData TestConfig SimConf
tests = Map.fromList
  [ testTopology   diamond
  , testTopology $ complete d3
  , testTopology $ cyclic d5
  , testTopology $ torus2d d2 d3
  , testTopology $ star d7
  , testTopology $ line d4
  , testTopology $ hourglass d3
  ]
 where
  ClockControlConfig{..} = clockControlConfig

  testTopology ::
    forall n.
    (KnownNat n, n <= FpgaCount) =>
    Topology n -> (TestName, (Probes TestConfig, SimConf))
  testTopology t =
    ( fromString $ topologyName t
    , ( toList (imap testData $ linkMasks @n t)
         <> [ (fromInteger i, disabled)
            | i <- [natToNum @n, natToNum @n + 1 .. natToNum @(FpgaCount - 1)]
            ]
      , def { mTopologyType = Just $ topologyType t
            , simulationSamples = 1000
            , simulationSteps = natToNum @(PeriodToCycles Basic125 (Seconds 60))
            , stabilityMargin = snatToNum cccStabilityCheckerMargin
            , stabilityFrameSize = snatToNum cccStabilityCheckerFramesize
            , disableReframing = not $ cccEnableReframing
            , rusty = cccEnableRustySimulation
            , waitTime = fromEnum cccReframingWaitTime
            , clockOffsets = toList $ repeat @n 0
            , startupOffsets = toList $ repeat @n 0
            , stopAfterStable =
                Just $ natToNum @(PeriodToCycles Basic125 AllStablePeriod)
            }
      )
    )

  testData ::
    forall n.
    (KnownNat n, n <= FpgaCount) =>
    Index n -> BitVector (FpgaCount - 1) -> (Index FpgaCount, TestConfig)
  testData i mask =
    ( zeroExtend @Index @n @(FpgaCount - n) i
    , TestConfig{ fpgaEnabled = True, .. }
    )

disabled :: TestConfig
disabled = TestConfig
  { fpgaEnabled = False
  , mask = 0
  }