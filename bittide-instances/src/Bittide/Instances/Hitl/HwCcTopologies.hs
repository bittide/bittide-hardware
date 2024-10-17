-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
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
  commonSpiConfig,
  csDupe,
  cSigMap,
  tests,
) where

import Clash.Explicit.Prelude hiding (PeriodToCycles)
import qualified Clash.Explicit.Prelude as E
import Clash.Prelude (withClockResetEnable)

import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import GHC.Float.RealFracMethods (roundFloatInteger)
import Language.Haskell.TH (runIO)
import LiftType (liftTypeQ)
import System.FilePath

import Bittide.Arithmetic.PartsPer (PartsPer, ppm)
import Bittide.Arithmetic.Time
import Bittide.CircuitUtils
import Bittide.ClockControl
import Bittide.ClockControl.Callisto
import Bittide.ClockControl.DebugRegister (DebugRegisterCfg (..), debugRegisterWb)
import Bittide.ClockControl.Registers (ClockControlData (clockMod), clockControlWb)
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
import Bittide.Simulate.Config (CcConf (..))
import Bittide.Topology
import Bittide.Transceiver (transceiverPrbsN)

import Bittide.Hitl

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

import qualified Bittide.Arithmetic.PartsPer as PartsPer
import qualified Bittide.Transceiver as Transceiver
import qualified Bittide.Transceiver.ResetManager as ResetManager
import qualified Data.Map.Strict as Map (fromList)

type AllStablePeriod = Seconds 5

{- | The number of FINCs (if positive) or FDECs (if negative) applied
prior to the test start leading to some desired initial clock
offset.
-}
type FincFdecCount = Signed 32

{- | The number of clock cycles to wait before starting clock control
according to the local, but stable system clock of a node.
-}
type StartupDelay = Unsigned 32

-- | Availabe step size configurations.
data StepSizeSelect
  = PPB_1
  | PPB_10
  | PPB_100
  | PPB_500
  | PPM_1
  deriving (Generic, NFDataX, BitPack, Eq, Enum, Bounded, Show)

-- | Calibration stages
data CCCalibrationStage
  = -- | Apply previously measured clock offsets.
    NoCCCalibration
  | -- | Measure \"natural\" clock offsets.
    CCCalibrate
  | -- | Verify that the clocks still have similar \"natural\" offsets as when
    -- they were calibrated. If this is not the case, the clock frequencies
    -- shifted during the test, invalidating the results. Also see
    -- 'acceptableNoiseLevel'.
    CCCalibrationValidation
  deriving (Generic, NFDataX, BitPack, Eq, Enum, Bounded, Show)

{- | The step size, as it is used by all tests. Note that changing the
step size for individual tests requires recalibration of the clock
offsets, which is why we fix it to a single and common value here.
-}
commonStepSizeSelect :: StepSizeSelect
commonStepSizeSelect =
  -- Don't forget to update the value of f_step this value in "Callisto.hs" and
  -- "callisto.rs".
  PPB_100

commonStepSizePartsPer :: PartsPer
commonStepSizePartsPer = case commonStepSizeSelect of
  PPB_1 -> PartsPer.ppb 1
  PPB_10 -> PartsPer.ppb 10
  PPB_100 -> PartsPer.ppb 100
  PPB_500 -> PartsPer.ppb 500
  PPM_1 -> PartsPer.ppm 1

partsPerToSteps :: PartsPer -> FincFdecCount
partsPerToSteps =
  fromIntegral . roundFloatInteger . PartsPer.toSteps commonStepSizePartsPer

commonSpiConfig :: TestConfig6_200_on_0a_RegisterMap
commonSpiConfig = case commonStepSizeSelect of
  PPB_1 -> testConfig6_200_on_0a_1ppb
  PPB_10 -> testConfig6_200_on_0a_10ppb
  PPB_100 -> testConfig6_200_on_0a_100ppb
  PPB_500 -> testConfig6_200_on_0a_500ppb
  PPM_1 -> testConfig6_200_on_0a_1ppm

{- | Accepted noise between the inital clock control calibration run
and the last calibration verifiction run.
-}
acceptableNoiseLevel :: FincFdecCount
acceptableNoiseLevel = 6

disabled :: TestConfig
disabled =
  TestConfig
    { fpgaEnabled = False
    , calibrate = NoCCCalibration
    , initialClockShift = Nothing
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
  , initialClockShift :: Maybe FincFdecCount
  -- ^ Artificical clock shift applied prior to the test start, expressed as
  -- number of FINCs (if positive) or FDECs (if negative).
  , startupDelay :: StartupDelay
  -- ^ Some intial startup delay given in the number of clock
  -- cycles of the stable clock.
  , mask :: BitVector LinkCount
  -- ^ The link mask depending on the selected topology.
  }
  deriving (Generic, NFDataX, BitPack, Show)

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
implementation of Callisto (see 'topologyTest') and copies it to a register
tied to FINC/FDEC.
-}
riscvCopyTest ::
  forall dom.
  (KnownDomain dom, 1 <= DomainPeriod dom) =>
  Clock dom ->
  Reset dom ->
  Signal dom (BitVector LinkCount) ->
  Signal dom (CallistoResult LinkCount) ->
  Vec LinkCount (Signal dom (RelDataCount 32)) ->
  -- Freq increase / freq decrease request to clock board
  ( "FINC" ::: Signal dom Bool
  , "FDEC" ::: Signal dom Bool
  )
riscvCopyTest clk rst mask callistoResult dataCounts = unbundle fIncDec
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
                (clockControlWb margin framesize mask dataCounts)
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
        memBlobsFromElf BigEndian (Nothing, Nothing) elfPath Nothing
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
results to a RiscV core (see 'riscvCopyTest')
-}
topologyTest ::
  "SMA_MGT_REFCLK_C" ::: Clock Ext200 ->
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
  , "CALIB_I" ::: Signal Basic125 FincFdecCount
  , "CALIB_E" ::: Signal Basic125 FincFdecCount
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
    withClockResetEnable sysClk syncRst enableGen
      $ si539xSpi commonSpiConfig (SNat @(Microseconds 10)) (pure Nothing) miso

  -- Transceiver setup
  gthAllReset = unsafeFromActiveLow clocksAdjusted

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
      callistoClockControl
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
      (0 :: FincFdecCount)
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
      (0 :: FincFdecCount)
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

  initialAdjust = (+) <$> calibratedClockShift <*> (fromMaybe 0 . initialClockShift <$> cfg)

  adjustCount =
    regEn
      sysClk
      adjustRst
      enableGen
      (0 :: FincFdecCount)
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
        ( speedChangeToStickyPins
            sysClk
            clockControlReset
            enableGen
            (SNat @Si539xHoldTime)
            clockMod
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
hwCcTopologyWithRiscvTest refClkDiff sysClkDiff syncIn rxns rxps miso =
  (txns, txps, (riscvFinc, riscvFdec), syncOut, spiDone, spiOut)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Ext200

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
      $ riscvCopyTest sysClk callistoReset (mask <$> cfg) callistoResult dataCounts

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
hwCcTopologyTest refClkDiff sysClkDiff syncIn rxns rxps miso =
  (txns, txps, unbundle hwFincFdecs, syncOut, spiDone, spiOut)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Ext200
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

tests :: HitlTestGroup
tests = testGroup
 where
  m = 1_000_000

  icsDiamond = map ppm $ -10 :> -5 :> 20 :> 30 :> Nil
  sdDiamond = 0 :> 10 :> 200 :> 3 :> Nil

  icsComplete = map ppm $ -100 :> 0 :> 100 :> Nil
  sdComplete = 200 :> 0 :> 200 :> Nil

  icsCyclic = map ppm $ 0 :> 5 :> 10 :> 15 :> 20 :> Nil
  sdCyclic = 0 :> 10 :> 0 :> 100 :> 0 :> Nil

  icsTorus = map ppm $ -30 :> -35 :> -40 :> 40 :> 35 :> 30 :> Nil
  sdTorus = 0 :> 0 :> 0 :> 100 :> 100 :> 100 :> Nil

  icsStar = map ppm $ 0 :> 10 :> -10 :> 20 :> -20 :> 30 :> -30 :> 40 :> Nil
  sdStar = 0 :> 40 :> 80 :> 120 :> 160 :> 200 :> 240 :> 280 :> Nil

  icsLine = map ppm $ 100 :> 0 :> 0 :> -100 :> Nil
  sdLine = 200 :> 0 :> 0 :> 200 :> Nil

  icsHourglass = map ppm $ -100 :> 100 :> -100 :> 100 :> -100 :> 100 :> Nil
  sdHourglass = 0 :> 200 :> 0 :> 200 :> 0 :> 200 :> Nil

  ClockControlConfig{..} = clockControlConfig

  defSimCfg =
    def
      { samples = 1000
      , duration = natToNum @(PeriodToCycles Basic125 (Seconds 60))
      , stabilityMargin = snatToNum cccStabilityCheckerMargin
      , stabilityFrameSize = snatToNum cccStabilityCheckerFramesize
      , reframe = cccEnableReframing
      , waitTime = fromEnum cccReframingWaitTime
      , stopAfterStable =
          Just
            $ natToNum @(PeriodToCycles Basic125 AllStablePeriod)
      }

  calibrateClockOffsets = calibrateCC False
  validateClockOffsetCalibration = calibrateCC True

  calibrateCC :: Bool -> HitlTestCase HwTargetRef TestConfig CcConf
  calibrateCC validate =
    HitlTestCase
      { name = (if validate then "zzz_validate" else "0_calibrate") <> "_clock_offsets"
      , parameters =
          Map.fromList $ allHwTargets
            <&> (,TestConfig
                    { fpgaEnabled = True
                    , calibrate =
                        if validate
                          then CCCalibrationValidation
                          else CCCalibrate
                    , initialClockShift = Nothing
                    , startupDelay = 0
                    , mask = maxBound
                    })
      , postProcData =
          defSimCfg
            { ccTopologyType = Complete $ natToInteger @FpgaCount
            , clockOffsets = Nothing
            , startupDelays = toList $ repeat @FpgaCount 0
            }
      }

  -- tests the given topology
  tt ::
    forall n.
    (KnownNat n, n <= FpgaCount) =>
    Maybe (Vec n PartsPer) ->
    Vec n StartupDelay ->
    Topology n ->
    HitlTestCase HwTargetRef TestConfig CcConf
  tt clockShifts startDelays t =
    HitlTestCase
      { name = topologyName t
      , parameters =
          Map.fromList
            $ toList
              ( zipWith4
                  testData
                  indicesI
                  (maybeVecToVecMaybe (map partsPerToSteps <$> clockShifts))
                  startDelays
                  (linkMasks @n t)
              )
            <> [ (HwTargetByIndex (fromInteger i), disabled)
               | let n = natToNum @n
               , i <- [n, n + 1 .. natToNum @LinkCount]
               ]
      , postProcData =
          defSimCfg
            { ccTopologyType = topologyType t
            , clockOffsets = toList <$> clockShifts
            , startupDelays = fromIntegral <$> toList startDelays
            }
      }

  maybeVecToVecMaybe :: forall n a. (KnownNat n) => Maybe (Vec n a) -> Vec n (Maybe a)
  maybeVecToVecMaybe = \case
    Just v -> Just <$> v
    Nothing -> repeat Nothing

  testData ::
    forall n.
    (KnownNat n, n <= FpgaCount) =>
    Index n ->
    Maybe FincFdecCount ->
    StartupDelay ->
    BitVector LinkCount ->
    (HwTargetRef, TestConfig)
  testData i initialClockShift startupDelay mask =
    ( HwTargetByIndex (fromIntegral i)
    , TestConfig
        { fpgaEnabled = True
        , calibrate = NoCCCalibration
        , ..
        }
    )

{- FOURMOLU_DISABLE -} -- fourmolu doesn't do well with tabular structures
  testGroup =
    HitlTestGroup
    { topEntity = 'hwCcTopologyTest
    , extraXdcFiles = []
    , externalHdl = []
    , testCases =
        [ -- detect the natual clock offsets to be elided from the later tests
          calibrateClockOffsets

          -- initial clock shifts   startup delays            topology
        , tt (Just icsDiamond)      ((m *) <$> sdDiamond)     diamond
        , tt (Just icsComplete)     ((m *) <$> sdComplete)    (complete d3)
        , tt (Just icsCyclic)       ((m *) <$> sdCyclic)      (cyclic d5)
        , tt (Just icsTorus)        ((m *) <$> sdTorus)       (torus2d d2 d3)
        , tt (Just icsStar)         ((m *) <$> sdStar)        (star d7)
        , tt (Just icsLine)         ((m *) <$> sdLine)        (line d4)
        , tt (Just icsHourglass)    ((m *) <$> sdHourglass)   (hourglass d3)

          -- make sure the clock offsets detected during calibration is still the same
        , validateClockOffsetCalibration
        ]
    , mPostProc = Nothing
    }
{- FOURMOLU_ENABLE -}
