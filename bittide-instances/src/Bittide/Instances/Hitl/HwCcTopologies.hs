-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

{- | Test whether clock boards are configurable and transceiver links come
online. If they do, run clock control and wait for the clocks to stabilize.
Also see  'Bittide.Instances.Hitl.Setup'. It has two tricks up its
sleeve:

  1. It uses @SYNC_IN@/@SYNC_OUT@ to make sure each board starts programming
     its clock boards at the same time.

  2. It keeps track of how many times the GTH's reset manager had to reset
     the connection and how often it lost connections after establishing
     them.

This test will succeed if all clocks have been stable for 5 seconds. Note:
this doesn't test reframing yet.
-}
module Bittide.Instances.Hitl.HwCcTopologies (
  hwCcTopologyWithRiscvTest,
  hwCcTopologyTest,
  clockControlConfig,
  tests,
) where

import Clash.Explicit.Prelude hiding (PeriodToCycles)
import qualified Clash.Explicit.Prelude as E
import Clash.Prelude (withClockResetEnable)

import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import Data.String (fromString)
import Language.Haskell.TH (runIO)
import LiftType (liftTypeQ)
import System.FilePath

import Bittide.Arithmetic.Time
import Bittide.ClockControl
import Bittide.ClockControl.Callisto
import Bittide.ClockControl.Callisto.Util (FDEC, FINC, speedChangeToPins, stickyBits)
import Bittide.ClockControl.Registers (clockControlWb)
import Bittide.ClockControl.Si5395J
import Bittide.ClockControl.Si539xSpi (ConfigState (Error, Finished), si539xSpi)
import Bittide.Counter
import Bittide.DoubleBufferedRam (
  ContentType (Blob),
  InitialContent (Reloadable),
  RegisterWritePriority (CircuitPriority),
  registerWb,
 )
import Bittide.ElasticBuffer (sticky)
import Bittide.Instances.Domains
import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Bittide.ProcessingElement.Util (memBlobsFromElf)
import Bittide.SharedTypes (ByteOrder (BigEndian), Bytes)
import Bittide.Simulate.Config (SimConf (..))
import Bittide.Topology
import Bittide.Transceiver (transceiverPrbsN)

import Bittide.Hitl (HitlTestsWithPostProcData, Probes, TestName, hitlVio)

import Bittide.Instances.Hitl.IlaPlot
import Bittide.Instances.Hitl.Setup
import Project.FilePath

import Clash.Annotations.TH (makeTopEntity)
import Clash.Class.Counter
import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.Ila (Depth (..), IlaConfig (..), ila, ilaConfig)
import Clash.Sized.Extra (unsignedToSigned)
import Clash.Xilinx.ClockGen

import Protocols hiding (SimulationConfig)
import Protocols.Wishbone
import VexRiscv

import qualified Bittide.Transceiver as Transceiver
import qualified Bittide.Transceiver.ResetManager as ResetManager
import qualified Data.Map.Strict as Map (fromList)

type AllStablePeriod = Seconds 5

{- | The number of FINCs (if positive) or FDECs (if negative) applied
prior to the test start leading to some desired initial clock
offset.

Note that the value is limited to fit within 16 bits right now to
avoid the generation of YAML files with integers that are larger
than 63 bits.
-}
type InitialClockShift = Signed 32

{- | The number of clock cycles to wait before starting clock control
according to the local, but stable system clock of a node.
-}
type StartupDelay = Unsigned 32

-- | Availabe step size configurations.
data StepSizeSelect
  = PPB_1
  | PPB_10
  | PPB_100
  | PPM_1
  deriving (Generic, NFDataX, BitPack, Eq, Enum, Bounded)

-- | Calibration stages
data CCCalibrationStage
  = NoCCCalibration
  | CCCalibrate
  | CCCalibrationValidation
  deriving (Generic, NFDataX, BitPack, Eq, Enum, Bounded)

{- | The step size, as it is used by all tests. Note that changing the
step size for individual tests requires recalibration of the clock
offsets, which is why we fix it to a single and common value here.
-}
commonStepSizeSelect :: StepSizeSelect
commonStepSizeSelect = PPB_10

{- | Accepted noise between the inital clock control calibration run
and the last calibration verifiction run.
-}
acceptableNoiseLevel :: InitialClockShift
acceptableNoiseLevel = 6

disabled :: TestConfig
disabled =
  TestConfig
    { fpgaEnabled = False
    , calibrate = NoCCCalibration
    , stepSizeSelect = commonStepSizeSelect
    , initialClockShift = 0
    , startupDelay = 0
    , mask = 0
    }

-- | The test configuration.
data TestConfig = TestConfig
  { fpgaEnabled :: Bool
  -- ^ Enables or disables an FPGA depending on the selected
  -- topology. Disabled FPGAs immediediatly succeed after the test
  -- start.
  --
  -- Also note that the flag only disables clock control, while
  -- other functionality, as for example SYNC_IN/SYNC_OUT time
  -- synchronization, needs to stay alive.
  , calibrate :: CCCalibrationStage
  -- ^ Indicates the selected calibration stage.
  , stepSizeSelect :: StepSizeSelect
  -- ^ The selected step size of the test. Note that changing the
  -- step size between tests requires re-calibration of the device
  -- based inital clock shift.
  , initialClockShift :: InitialClockShift
  -- ^ Some artificical clock shift applied prior to the test
  -- start. The shift is given in FINCs (if positive) or FDECs (if
  -- negative) and, thus, depdends on 'stepSizeSelect'.
  , startupDelay :: StartupDelay
  -- ^ Some intial startup delay given in the number of clock
  -- cycles of the stable clock.
  , mask :: BitVector LinkCount
  -- ^ The link mask depending on the selected topology.
  }
  deriving (Generic, NFDataX, BitPack)

clockControlConfig ::
  $(case (instancesClockConfig (Proxy @Basic125)) of (_ :: t) -> liftTypeQ @t)
clockControlConfig =
  $(lift (instancesClockConfig (Proxy @Basic125)))

{- | Instantiates a RiscV core that copies instructions coming from a hardware
implementation of Callisto (see 'topologyTest') and copies it to a register
tied to FINC/FDEC.
-}
riscvCopyTest ::
  forall dom.
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  Signal dom (CallistoResult LinkCount) ->
  Vec LinkCount (Signal dom (RelDataCount 32)) ->
  -- Freq increase / freq decrease request to clock board
  ( "FINC" ::: Signal dom Bool
  , "FDEC" ::: Signal dom Bool
  )
riscvCopyTest clk rst callistoResult dataCounts = unbundle fIncDec
 where
  (_, fIncDec) =
    toSignals
      ( circuit $ \jtag -> do
          [wbA, wbB] <-
            withClockResetEnable clk rst enableGen $ processingElement @dom peConfig -< jtag
          fIncDecCallisto -< wbA
          (fIncDec, _allStable) <-
            withClockResetEnable clk rst enableGen
              $ clockControlWb margin framesize (pure $ complement 0) dataCounts
              -< wbB
          idC -< fIncDec
      )
      (pure $ JtagIn low low low, pure ())

  fIncDecCallisto ::
    forall aw nBytes.
    (KnownNat aw, 2 <= aw, nBytes ~ 4) =>
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
        memBlobsFromElf BigEndian (Nothing, Nothing) elfPath Nothing
     )

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

{- | Instantiates a hardware implementation of Callisto and exports its results. Can
be used to drive FINC/FDEC directly (see @FINC_FDEC@ result) or to tie the
results to a RiscV core (see 'riscvCopyTest')
-}
topologyTest ::
  "SMA_MGT_REFCLK_C" ::: Clock Ext250 ->
  "SYSCLK" ::: Clock Basic125 ->
  "SYSRST" ::: Reset Basic125 ->
  "ILA_CTRL" ::: IlaControl Basic125 ->
  "GTH_RX_NS" ::: TransceiverWires GthRxS LinkCount ->
  "GTH_RX_PS" ::: TransceiverWires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  "TEST_CFG" ::: Signal Basic125 TestConfig ->
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
  , "CALIB_I" ::: Signal Basic125 InitialClockShift
  , "CALIB_E" ::: Signal Basic125 InitialClockShift
  )
topologyTest refClk sysClk sysRst IlaControl{syncRst = rst, ..} rxNs rxPs miso cfg =
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
             , calibratedClockShift
             , validationClockShift
             )
 where
  syncRst = rst `orReset` unsafeFromActiveHigh spiErr

  -- Clock board programming

  spiDone = E.dflipflop sysClk $ (== Finished) <$> spiState
  spiErr = E.dflipflop sysClk $ isErr <$> spiState

  isErr (Error _) = True
  isErr _ = False

  (_, _, spiState, spiOut) =
    let
      selectConfig = \case
        PPB_1 -> testConfig6_250_on_0a_1ppb
        PPB_10 -> testConfig6_250_on_0a_10ppb
        PPB_100 -> testConfig6_250_on_0a_100ppb
        PPM_1 -> testConfig6_250_on_0a_1ppm

      -- TODO: create some generic method for generating this, which
      -- does not rely on template haskell
      cfgOptions = PPB_1 :> PPB_10 :> PPB_100 :> PPM_1 :> Nil

      -- turn the selected configuration into an vector mask
      optionMask = fmap . (==) <$> cfgOptions <*> repeat (stepSizeSelect <$> cfg)
      -- retrieve the corresponding resets from the mask
      rsts = orReset syncRst . unsafeFromActiveLow <$> optionMask
      -- only the reset and the selected configuration differ according to
      -- 'stepSizeSelect'
      si539xSpi# r c =
        withClockResetEnable sysClk r enableGen
          $ si539xSpi c (SNat @(Microseconds 10)) (pure Nothing) miso
      -- create an SPI interface for each of the supported configurations
      spis = si539xSpi# <$> rsts <*> (selectConfig <$> cfgOptions)
     in
      (\(a, b, c, d) -> (a, b, c, unbundle d))
        . unbundle
        $ (!!)
        <$> bundle ((\(a, b, c, d) -> bundle (a, b, c, bundle d)) <$> spis)
        -- mux the selected interface according to 'stepSizeSelect'
        <*> (stepSizeSelect <$> cfg)

  -- Transceiver setup

  gthAllReset = unsafeFromActiveLow clocksAdjusted

  transceivers =
    transceiverPrbsN
      @GthTx
      @GthRx
      @Ext250
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

  -- Startup delay

  startupDelayRst =
    orReset (unsafeFromActiveLow clocksAdjusted)
      $ orReset (unsafeFromActiveLow allReady)
      $ orReset
        (unsafeFromActiveHigh transceiversFailedAfterUp)
        (unsafeFromActiveLow syncStart)

  delayCount =
    register sysClk startupDelayRst enableGen (0 :: StartupDelay)
      $ (\c s -> if c < s then satSucc SatBound c else c)
      <$> delayCount
      <*> (startupDelay <$> cfg)

  -- Clock control

  clockControlReset =
    startupDelayRst
      `orReset` unsafeFromActiveLow ((==) <$> delayCount <*> (startupDelay <$> cfg))

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
      (mask <$> cfg)
      (fmap (fmap resize) domainDiffs)

  -- Capture every 100 microseconds - this should give us a window of about 5
  -- seconds. Or: when we're in reset. If we don't do the latter, the VCDs get
  -- very confusing.
  capture = (captureFlag .&&. allReady) .||. unsafeToActiveHigh syncRst

  fincFdecIla :: Signal Basic125 ()
  fincFdecIla =
    setName @"fincFdecIla"
      ila
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

  -- Clock calibration

  clockShiftUpd = \case
    Just SpeedUp -> satSucc SatBound
    Just SlowDown -> satPred SatBound
    _ -> id

  notInCCReset = unsafeToActiveLow clockControlReset

  clockShift =
    regEn
      sysClk
      sysRst
      enableGen
      (0 :: InitialClockShift)
      (notInCCReset .&&. (== CCCalibrate) . calibrate <$> cfg)
      (clockShiftUpd <$> clockMod <*> clockShift)

  calibratedClockShift =
    register sysClk sysRst enableGen 0
      $ mux
        ( isFalling sysClk sysRst enableGen False
            $ (== CCCalibrate)
            . calibrate
            <$> cfg
        )
        clockShift
        calibratedClockShift

  validationClockShift =
    regEn
      sysClk
      sysRst
      enableGen
      (0 :: InitialClockShift)
      (notInCCReset .&&. (== CCCalibrationValidation) . calibrate <$> cfg)
      (clockShiftUpd <$> clockMod <*> validationClockShift)

  -- Initial Clock adjustment

  -- without the additional delay of 1 second here, some of the
  -- initial FINC/FDECs prior to test start will be lost.
  adjustStart = trueFor (SNat @(Seconds 1)) sysClk syncRst spiDone
  clocksAdjusted =
    spiDone
      .&&. ( (/= NoCCCalibration)
              . calibrate
              <$> cfg
              .||. (==)
              <$> initialAdjust
              <*> adjustCount
           )
  adjusting = adjustStart .&&. (not <$> clocksAdjusted)
  adjustRst = unsafeFromActiveLow adjustStart

  initialAdjust = (+) <$> calibratedClockShift <*> (initialClockShift <$> cfg)

  adjustCount =
    regEn
      sysClk
      adjustRst
      enableGen
      (0 :: InitialClockShift)
      adjusting
      $ flip upd
      <$> adjustCount
      <*> let f = isFalling sysClk adjustRst enableGen False
           in bundle $ bimap f f $ unbundle frequencyAdjustments
   where
    upd (True, _) = satSucc SatBound
    upd (_, True) = satPred SatBound
    upd _ = id

  frequencyAdjustments :: Signal Basic125 (FINC, FDEC)
  frequencyAdjustments =
    E.delay sysClk enableGen minBound {- glitch filter -}
      $ mux
        adjusting
        ( speedChangeToFincFdec sysClk adjustRst
            $ opSelect
            <$> initialAdjust
            <*> adjustCount
        )
        ( withClockResetEnable sysClk clockControlReset enableGen
            $ stickyBits @Basic125 d20
            $ speedChangeToPins
            . fromMaybe NoChange
            <$> clockMod
        )
   where
    opSelect calib adjust = case compare calib adjust of
      LT -> SlowDown
      EQ -> NoChange
      GT -> SpeedUp

  domainDiffs =
    domainDiffCounterExt sysClk clockControlReset
      <$> transceivers.rxClocks
      <*> transceivers.txClocks

-- | Top entity for this test. See module documentation for more information.
hwCcTopologyWithRiscvTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext250 ->
  "SYSCLK_300" ::: DiffClock Ext300 ->
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
hwCcTopologyWithRiscvTest refClkDiff sysClkDiff syncIn rxns rxps miso =
  (txns, txps, (riscvFinc, riscvFdec), syncOut, spiDone, spiOut)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Ext250

  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset

  ilaControl@IlaControl{..} = ilaPlotSetup IlaPlotSetup{..}
  startTest = isJust <$> testConfig

  cfg = fromMaybe disabled <$> testConfig

  ( txns
    , txps
    , hwFincFdecs
    , callistoResult
    , callistoReset
    , dataCounts
    , _stats
    , spiDone
    , spiOut
    , transceiversFailedAfterUp
    , allReady
    , allStable
    , calibI
    , calibE
    ) =
      topologyTest
        refClk
        sysClk
        sysRst
        ilaControl{skipTest = skip}
        rxns
        rxps
        miso
        cfg

  (riscvFinc, riscvFdec) =
    unbundle
      $ mux (unsafeToActiveHigh callistoReset) hwFincFdecs
      $ bundle
      $ riscvCopyTest sysClk callistoReset callistoResult dataCounts

  -- check that tests are not synchronously start before all
  -- transceivers are up
  startBeforeAllReady =
    sticky
      sysClk
      syncRst
      (startTest .&&. syncStart .&&. ((not <$> allReady) .||. transceiversFailedAfterUp))

  endSuccess :: Signal Basic125 Bool
  endSuccess =
    trueFor (SNat @AllStablePeriod) sysClk syncRst allStable
      .&&. ( (/= CCCalibrationValidation)
              . calibrate
              <$> cfg
              .||. (\i e -> abs (i - e) < acceptableNoiseLevel)
              <$> calibI
              <*> calibE
           )

  done = endSuccess .||. transceiversFailedAfterUp .||. startBeforeAllReady
  success = not <$> (transceiversFailedAfterUp .||. startBeforeAllReady)

  skip =
    register
      sysClk
      sysRst
      enableGen
      False
      (maybe False (not . fpgaEnabled) <$> testConfig)

  testConfig :: Signal Basic125 (Maybe TestConfig)
  testConfig = hitlVio disabled sysClk done success

makeTopEntity 'hwCcTopologyWithRiscvTest

-- | Top entity for this test. See module documentation for more information.
hwCcTopologyTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext250 ->
  "SYSCLK_300" ::: DiffClock Ext300 ->
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
hwCcTopologyTest refClkDiff sysClkDiff syncIn rxns rxps miso =
  (txns, txps, unbundle hwFincFdecs, syncOut, spiDone, spiOut)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Ext250
  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset
  ilaControl@IlaControl{..} = ilaPlotSetup IlaPlotSetup{..}
  startTest = isJust <$> testConfig

  cfg = fromMaybe disabled <$> testConfig

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
    , calibI
    , calibE
    ) =
      topologyTest
        refClk
        sysClk
        sysRst
        ilaControl{skipTest = skip}
        rxns
        rxps
        miso
        cfg

  -- check that tests are not synchronously start before all
  -- transceivers are up
  startBeforeAllReady =
    sticky
      sysClk
      syncRst
      (syncStart .&&. ((not <$> allReady) .||. transceiversFailedAfterUp))

  endSuccess :: Signal Basic125 Bool
  endSuccess =
    trueFor (SNat @(Seconds 5)) sysClk syncRst allStable
      .&&. ( (/= CCCalibrationValidation)
              . calibrate
              <$> cfg
              .||. (\i e -> abs (i - e) < acceptableNoiseLevel)
              <$> calibI
              <*> calibE
           )

  skip = maybe False (not . fpgaEnabled) <$> testConfig

  testConfig :: Signal Basic125 (Maybe TestConfig)
  testConfig =
    hitlVio
      disabled
      sysClk
      -- done
      ( startTest
          .&&. (skip .||. endSuccess .||. transceiversFailedAfterUp .||. startBeforeAllReady)
      )
      -- success
      ( skip
          .||. (allStable .&&. (not <$> (transceiversFailedAfterUp .||. startBeforeAllReady)))
      )

makeTopEntity 'hwCcTopologyTest

tests :: HitlTestsWithPostProcData TestConfig SimConf
tests =
  Map.fromList
    [ -- CALIBRATION --
      -----------------

      -- detect the natual clock offsets to be elided from the later tests
      calibrateClockOffsets
    , -- TESTS --
      -----------

      -- initial clock shifts   startup delays            topology
      tt icsDiamond ((m *) <$> sdDiamond) diamond
    , tt icsComplete ((m *) <$> sdComplete) $ complete d3
    , tt icsCyclic ((m *) <$> sdCyclic) $ cyclic d5
    , tt icsTorus ((m *) <$> sdTorus) $ torus2d d2 d3
    , tt icsStar ((m *) <$> sdStar) $ star d7
    , tt icsLine ((m *) <$> sdLine) $ line d4
    , tt icsHourglass ((m *) <$> sdHourglass) $ hourglass d3
    , -- CALIBRATION VERIFICATON --
      -----------------------------
      validateClockOffsetCalibration
    ]
 where
  m = 1_000_000

  icsDiamond = -1000 :> -500 :> 2000 :> 3000 :> Nil
  sdDiamond = 0 :> 10 :> 200 :> 3 :> Nil

  icsComplete = -10000 :> 0 :> 10000 :> Nil
  sdComplete = 200 :> 0 :> 200 :> Nil

  icsCyclic = 0 :> 500 :> 1000 :> 1500 :> 2000 :> Nil
  sdCyclic = 0 :> 10 :> 0 :> 100 :> 0 :> Nil

  icsTorus = -3000 :> -3500 :> -4000 :> 4000 :> 3500 :> 3000 :> Nil
  sdTorus = 0 :> 0 :> 0 :> 100 :> 100 :> 100 :> Nil

  icsStar = 0 :> 1000 :> -1000 :> 2000 :> -2000 :> 3000 :> -3000 :> 4000 :> Nil
  sdStar = 0 :> 40 :> 80 :> 120 :> 160 :> 200 :> 240 :> 280 :> Nil

  icsLine = 10000 :> 0 :> 0 :> -10000 :> Nil
  sdLine = 200 :> 0 :> 0 :> 200 :> Nil

  icsHourglass = -10000 :> 10000 :> -10000 :> 10000 :> -10000 :> 10000 :> Nil
  sdHourglass = 0 :> 200 :> 0 :> 200 :> 0 :> 200 :> Nil

  ClockControlConfig{..} = clockControlConfig

  defSimCfg =
    def
      { samples = 1000
      , duration = natToNum @(PeriodToCycles Basic125 (Seconds 60))
      , stabilityMargin = snatToNum cccStabilityCheckerMargin
      , stabilityFrameSize = snatToNum cccStabilityCheckerFramesize
      , reframe = cccEnableReframing
      , rusty = cccEnableRustySimulation
      , waitTime = fromEnum cccReframingWaitTime
      , stopAfterStable =
          Just
            $ natToNum @(PeriodToCycles Basic125 AllStablePeriod)
      }

  calibrateClockOffsets = calibrateCC False
  validateClockOffsetCalibration = calibrateCC True
  calibrateCC validate =
    ( -- the names must be chosen such that the run is executed first/last
      (if validate then "zzz_validate" else "0_calibrate") <> "_clock_offsets"
    ,
      ( toList
          $ imap (,)
          $ repeat @FpgaCount
            TestConfig
              { fpgaEnabled = True
              , calibrate =
                  if validate
                    then CCCalibrationValidation
                    else CCCalibrate
              , stepSizeSelect = commonStepSizeSelect
              , initialClockShift = 0
              , startupDelay = 0
              , mask = maxBound
              }
      , defSimCfg
          { mTopologyType = Just $ Complete $ natToInteger @FpgaCount
          , clockOffsets = toList $ repeat @FpgaCount 0
          , startupDelays = toList $ repeat @FpgaCount 0
          }
      )
    )

  -- tests the given topology
  tt ::
    forall n.
    (KnownNat n, n <= FpgaCount) =>
    Vec n InitialClockShift ->
    Vec n StartupDelay ->
    Topology n ->
    (TestName, (Probes TestConfig, SimConf))
  tt clockShifts startDelays t =
    ( fromString $ topologyName t
    ,
      ( toList
          ( zipWith4 testData indicesI clockShifts startDelays
              $ linkMasks @n t
          )
          <> [ (fromInteger i, disabled)
             | let n = natToNum @n
             , i <- [n, n + 1 .. natToNum @LinkCount]
             ]
      , let
          -- clock period in picoseconds
          clkPeriodPs :: (Num a) => a
          clkPeriodPs = case clockControlConfig of
            (_ :: ClockControlConfig dom a b c) ->
              snatToNum (clockPeriod @dom)
          -- a 1000 times the factor of the selected parts-per-x scale
          -- ratio, where the reduction by a factor of 1000 accounts
          -- to the required conversion of from Picoseconds to
          -- Femtoseconds for 'clkPeriodPs' already applied at this
          -- point to reduce the loss-op-presion introduced otherwise.
          stepSizeDiv = case commonStepSizeSelect of
            PPB_1 -> 1_000_000
            PPB_10 -> 100_000
            PPB_100 -> 10_000
            PPM_1 -> 1_000
         in
          defSimCfg
            { mTopologyType = Just $ topologyType t
            , clockOffsets =
                (/ stepSizeDiv)
                  . (* clkPeriodPs)
                  . fromIntegral
                  <$> toList clockShifts
            , startupDelays = fromIntegral <$> toList startDelays
            }
      )
    )

  testData ::
    forall n.
    (KnownNat n, n <= FpgaCount) =>
    Index n ->
    InitialClockShift ->
    StartupDelay ->
    BitVector LinkCount ->
    (Index FpgaCount, TestConfig)
  testData i initialClockShift startupDelay mask =
    ( zeroExtend @Index @n @(FpgaCount - n) i
    , TestConfig
        { fpgaEnabled = True
        , calibrate = NoCCCalibration
        , stepSizeSelect = commonStepSizeSelect
        , ..
        }
    )
