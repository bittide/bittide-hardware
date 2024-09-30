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
module Bittide.Instances.Hitl.SwCcTopologies (
  swCcTopologyTest,
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
import LiftType (liftTypeQ)

import Bittide.Arithmetic.PartsPer (PartsPer, ppm)
import Bittide.Arithmetic.Time
import Bittide.ClockControl
import Bittide.ClockControl.Callisto.Util (FDEC, FINC, speedChangeToPins, stickyBits)
import Bittide.ClockControl.CallistoSw (CallistoSwResult (..), callistoSwClockControl)
import Bittide.ClockControl.Si5395J
import Bittide.ClockControl.Si539xSpi (ConfigState (Error, Finished), si539xSpi)
import Bittide.Counter
import Bittide.ElasticBuffer
import Bittide.Instances.Domains
import Bittide.Simulate.Config (CcConf (..))
import Bittide.Topology
import Bittide.Transceiver (transceiverPrbsN)

import Bittide.Hitl
import Bittide.Instances.Hitl.IlaPlot (
  IlaControl (..),
  IlaPlotSetup (..),
  -- callistoSwClockControlWithIla,
  ilaPlotSetup,
 )
import Bittide.Instances.Hitl.Setup

import Clash.Annotations.TH (makeTopEntity)
import Clash.Class.Counter
import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.Ila (Depth (..), IlaConfig (..), ila, ilaConfig)
import Clash.Cores.Xilinx.Xpm.Cdc (xpmCdcSingle)
import Clash.Cores.Xilinx.Xpm.Cdc.Handshake.Extra (xpmCdcMaybeLossy)
import Clash.Functor.Extra
import Clash.Sized.Extra (unsignedToSigned)
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Clash.Xilinx.ClockGen

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
    , reframingEnabled = False
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
  , reframingEnabled :: Bool
  -- ^ Whether or not to run this test with reframing enabled.
  }
  deriving (Generic, NFDataX, BitPack, Show)

clockControlConfig ::
  $(case (instancesClockConfig (Proxy @Basic125)) of (_ :: t) -> liftTypeQ @t)
clockControlConfig =
  $(lift (instancesClockConfig (Proxy @Basic125)))

type FifoSize = 5 -- = 2^5 = 32

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
  , "CALLISTO_SW_RESULT" ::: Signal Basic125 (CallistoSwResult LinkCount)
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
  , "ugnsStable" ::: Vec LinkCount (Signal Basic125 Bool)
  )
topologyTest refClk sysClk sysRst IlaControl{syncRst = rst, ..} rxNs rxPs miso cfg =
  fincFdecIla
    `hwSeqX` ( transceivers.txNs
             , transceivers.txPs
             , fincFdecs
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
             , ugnsStable
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
        , txDatas = txCounters
        , txReadys = txAllStables
        , rxReadys = repeat (pure True)
        }

  allReady =
    trueFor (SNat @(Milliseconds 500)) sysClk syncRst (and <$> bundle transceivers.linkReadys)
  transceiversFailedAfterUp =
    sticky sysClk syncRst (isFalling sysClk syncRst enableGen False allReady)

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

  ( clockMod
    , stabilities
    , allStable0
    , _allCentered
    , swUpdatePeriod
    ) =
      unbundle
        $ fmap
          ( \CallistoSwResult{..} ->
              ( maybeSpeedChange
              , stability
              , allStable
              , allSettled
              , updatePeriod
              )
          )
          callistoResult

  fincFdecs = speedChangeToPins . fromMaybe NoChange <$> clockMod

  FillStats swUpdatePeriodMin swUpdatePeriodMax = unbundle $ fillStats sysClk syncRst swUpdatePeriod

  -- callistoResult =
  -- callistoSwClockControlWithIla @LinkCount @CccBufferSize
  -- (SNat @CccStabilityCheckerMargin)
  -- (SNat @(CccStabilityCheckerFramesize Basic125))
  -- (head transceivers.txClocks)
  -- sysClk
  -- clockControlReset
  -- (reframingEnabled <$> cfg)
  -- IlaControl{..}
  -- (mask <$> cfg)
  -- (fmap (fmap resize) domainDiffs)
  callistoResult =
    callistoSwClockControl @LinkCount @CccBufferSize
      (SNat @CccStabilityCheckerMargin)
      (SNat @(CccStabilityCheckerFramesize Basic125))
      sysClk
      clockControlReset
      enableGen
      (reframingEnabled <$> cfg)
      (mask <$> cfg)
      (resize <<$>> domainDiffs)

  -- Capture every 100 microseconds - this should give us a window of about 5
  -- seconds. Or: when we're in reset. If we don't do the latter, the VCDs get
  -- very confusing.
  -- capture = (captureFlag .&&. allReady) .||. unsafeToActiveHigh syncRst

  captureFlag =
    riseEvery
      sysClk
      sysRst
      enableGen
      (SNat @(PeriodToCycles Basic125 (Milliseconds 1)))

  timeSucc = countSucc @(Unsigned 16, Index (PeriodToCycles Basic125 (Milliseconds 1)))
  timer = register sysClk sysRst enableGen (0, 0) (timeSucc <$> timer)
  milliseconds1 = fst <$> timer

  fincFdecIla :: Signal Basic125 ()
  fincFdecIla =
    setName @"fincFdecIla"
      ila
      ( ilaConfig
          $ "trigger_fdi_0"
          :> "capture_fdi_0"
          :> "probe_fdi_milliseconds"
          :> "probe_allStable0"
          :> "probe_transceiversFailedAfterUp"
          :> "probe_nFincs"
          :> "probe_nFdecs"
          :> "probe_net_nFincs"
          :> "probe_ugn0"
          :> "probe_ugn1"
          :> "probe_ugn2"
          :> "probe_ugn3"
          :> "probe_ugn4"
          :> "probe_ugn5"
          :> "probe_ugn6"
          :> "stability0"
          :> "stability1"
          :> "stability2"
          :> "stability3"
          :> "stability4"
          :> "stability5"
          :> "stability6"
          :> "ugnStable0"
          :> "ugnStable1"
          :> "ugnStable2"
          :> "ugnStable3"
          :> "ugnStable4"
          :> "ugnStable5"
          :> "ugnStable6"
          :> "probe_linkReadys"
          :> "probe_linkUps"
          :> "fifoUnderflows"
          :> "fifoOverflows"
          :> "swUpdatePeriod"
          :> "swUpdatePeriodMin"
          :> "swUpdatePeriodMax"
          :> "probe_dDiff0"
          :> "probe_dDiff1"
          :> "probe_dDiff2"
          :> "probe_dDiff3"
          :> "probe_dDiff4"
          :> "probe_dDiff5"
          :> "probe_dDiff6"
          :> "probe_syncRst"
          :> "probe_gthAllReset"
          :> "probe_startupDelayRst"
          :> "probe_clockControlReset"
          :> "probe_notInCCReset"
          :> "probe_txResets2"
          :> "probe_adjustStart"
          :> "probe_clocksAdjusted"
          :> "probe_adjusting"
          :> "probe_adjustCount"
          :> "probe_initialAdjust"
          :> "probe_adjustRst"
          :> "probe_calibratedClockShift"
          :> "probe_clockShift"
          :> "probe_initialClockShift"
          :> "probe_calibrate"
          :> "probe_spiDone"
          :> "probe_frequencyAdjustments"
          :> "probe_allReady"
          :> "probe_syncStart"
          :> "probe_delayCount"
          :> "probe_startupDelay"
          :> Nil
      )
        { depth = D16384
        }
      sysClk
      -- Trigger as soon as we come out of reset
      (unsafeToActiveLow sysRst)
      captureFlag
      -- Debug probes
      milliseconds1
      allStable0
      transceiversFailedAfterUp
      nFincs
      nFdecs
      (fmap unsignedToSigned nFincs - fmap unsignedToSigned nFdecs)
      ugn0
      ugn1
      ugn2
      ugn3
      ugn4
      ugn5
      ugn6
      stability0
      stability1
      stability2
      stability3
      stability4
      stability5
      stability6
      ugnStable0
      ugnStable1
      ugnStable2
      ugnStable3
      ugnStable4
      ugnStable5
      ugnStable6
      (bundle transceivers.linkReadys)
      (bundle transceivers.linkUps)
      (pack . reverse <$> bundle fifoUnderflowsFree)
      (pack . reverse <$> bundle fifoOverflowsFree)
      swUpdatePeriod
      swUpdatePeriodMin
      swUpdatePeriodMax
      dDiff0
      dDiff1
      dDiff2
      dDiff3
      dDiff4
      dDiff5
      dDiff6
      (unsafeFromReset syncRst)
      (unsafeFromReset gthAllReset)
      (unsafeFromReset startupDelayRst)
      (unsafeFromReset clockControlReset)
      notInCCReset
      txResetsThing
      adjustStart
      clocksAdjusted
      adjusting
      adjustCount
      initialAdjust
      (unsafeFromReset adjustRst)
      calibratedClockShift
      clockShift
      (fromMaybe 0 . initialClockShift <$> cfg)
      (pack . calibrate <$> cfg)
      spiDone
      (pack <$> frequencyAdjustments)
      allReady
      syncStart
      delayCount
      (startupDelay <$> cfg)

  {-
    clockMod
    clockShift
    initialClockShift
    calibrate
    spiDone
    frequencyAdjustments
    allReady
    syncStart
    delayCount
    startupDelay
  -}

  txResetsThing = bundle $ zipWith oofOwOuchie transceivers.txClocks txResets2
   where
    oofOwOuchie txClock txReset = unsafeSynchronizer txClock sysClk $ unsafeFromReset txReset

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

  domainDiffs :: Vec LinkCount (Signal Basic125 FincFdecCount)
  domainDiffs =
    domainDiffCounterExt sysClk clockControlReset
      <$> transceivers.rxClocks
      <*> transceivers.txClocks

  txAllStables = zipWith (xpmCdcSingle sysClk) transceivers.txClocks (repeat allStable1)
  allStable1 = sticky sysClk syncRst allStable0
  txResets2 =
    zipWith
      orReset
      transceivers.txResets
      (map unsafeFromActiveLow txAllStables)

  availableMask :: Vec LinkCount (Signal Basic125 Bit)
  availableMask = unbundle (bv2v . mask <$> cfg)
  txCounters = zipWith3 txCounter transceivers.txClocks txResets2 availableMask
  txCounter ::
    Clock GthTx -> Reset GthTx -> Signal Basic125 Bit -> Signal GthTx (BitVector 64)
  txCounter txClk txRst txMask = result
   where
    txMask' = unsafeSynchronizer sysClk txClk txMask
    next txMaskBit = case txMaskBit of
      1 -> countSucc
      _ -> id
    result =
      register txClk txRst enableGen (0xaabb_ccdd_eeff_1234 :: BitVector 64)
        $ liftA2 next txMask' result
  -- see NOTE [magic start values]

  rxFifos =
    zipWith4
      go
      transceivers.txClocks
      transceivers.rxClocks
      txResets2
      transceivers.rxDatas
   where
    go = resettableXilinxElasticBuffer @FifoSize @_ @_ @(Maybe (BitVector 64))

  (fillLvls, fifoUnderflowsTx, fifoOverflowsTx, _ebMode, rxCntrs) = unzip5 rxFifos

  fifoOverflowsFree :: Vec LinkCount (Signal Basic125 Overflow)
  fifoOverflowsFree = zipWith (`xpmCdcSingle` sysClk) transceivers.txClocks fifoOverflowsTx
  fifoUnderflowsFree :: Vec LinkCount (Signal Basic125 Underflow)
  fifoUnderflowsFree = zipWith (`xpmCdcSingle` sysClk) transceivers.txClocks fifoUnderflowsTx

  ugns :: Vec LinkCount (Signal GthTx (BitVector 64))
  ugns =
    zipWith
      (-)
      txCounters
      (map (fmap (fromMaybe 0x1122_3344_1122_3344)) rxCntrs)
  -- see NOTE [magic start values]

  -- NOTE [magic start values]
  -- These values could be anything, but are chosen to be recognisable and help debugging.
  --   0xaabbccddeeff1234 - 0x1122334411223344 = 0x99999999dddcdef0
  -- If you ever see the ugn being a constant 0x99999999dddcdef0
  -- then you know the your counter isn't running and you're receiving 'Nothing',
  -- If you see 0x99999999.......... and it's counting up, then you're receiving Nothing,
  -- but your counter is running.

  ugnStable1sec = zipWith3 (stableForMs (SNat @1000)) transceivers.txClocks transceivers.txResets ugns

  freeUgnDatas = zipWith5 go transceivers.txClocks (repeat sysClk) ugns fillLvls ugnStable1sec
   where
    go clkIn clkOut ugn fillLvl stable =
      regMaybe
        clkOut
        noReset
        enableGen
        (0, 0, False, unpack 0)
        (xpmCdcMaybeLossy clkIn clkOut inp)
     where
      fillStat = fillStats clkIn noReset fillLvl
      inp = Just <$> bundle (ugn, fillLvl, stable, fillStat)

  ugnsStable = map (fmap (\(_, _, x, _) -> x)) freeUgnDatas

  ( ugnD0
    , ugnD1
    , ugnD2
    , ugnD3
    , ugnD4
    , ugnD5
    , ugnD6
    ) = vecToTuple freeUgnDatas
  (ugn0, _fill0, ugnStable0, _fillStats0) = unbundle ugnD0
  (ugn1, _fill1, ugnStable1, _fillStats1) = unbundle ugnD1
  (ugn2, _fill2, ugnStable2, _fillStats2) = unbundle ugnD2
  (ugn3, _fill3, ugnStable3, _fillStats3) = unbundle ugnD3
  (ugn4, _fill4, ugnStable4, _fillStats4) = unbundle ugnD4
  (ugn5, _fill5, ugnStable5, _fillStats5) = unbundle ugnD5
  (ugn6, _fill6, ugnStable6, _fillStats6) = unbundle ugnD6

  ( stability0
    , stability1
    , stability2
    , stability3
    , stability4
    , stability5
    , stability6
    ) = vecToTuple $ unbundle stabilities
  ( dDiff0
    , dDiff1
    , dDiff2
    , dDiff3
    , dDiff4
    , dDiff5
    , dDiff6
    ) = vecToTuple domainDiffs

{- | Tracks the min/max values of the input during the last milliseconds

Updates once per millisecond.
-}
fillStats ::
  forall dom a.
  (KnownDomain dom, Ord a, Num a, Bounded a, NFDataX a) =>
  Clock dom ->
  Reset dom ->
  Signal dom a ->
  Signal dom (FillStats a)
fillStats clk rst = moore clk rst enableGen go (\(_, _, x) -> x) (maxBound, mempty, mempty)
 where
  go ::
    (IndexMs dom 1, FillStats a, FillStats a) ->
    a ->
    (IndexMs dom 1, FillStats a, FillStats a)
  go (cntr, prevStats, out) inp
    | cntr == 0 = (maxBound, mempty, new)
    | otherwise = (cntr - 1, new, out)
   where
    new = mappend prevStats (FillStats inp inp)

data FillStats a = FillStats {fillMin :: a, fillMax :: a}
  deriving (Generic, NFDataX, BitPack)

instance Bundle (FillStats a) where
  type Unbundled dom (FillStats a) = FillStats (Signal dom a)
  bundle (FillStats sigMin sigMax) = liftA2 FillStats sigMin sigMax
  unbundle x = FillStats{fillMin = fmap fillMin x, fillMax = fmap fillMax x}

instance (Ord a) => Semigroup (FillStats a) where
  a <> b =
    FillStats
      { fillMin = min (fillMin a) (fillMin b)
      , fillMax = max (fillMax a) (fillMax b)
      }

instance (Bounded a, Ord a) => Monoid (FillStats a) where
  mempty = FillStats{fillMin = maxBound, fillMax = minBound}

{- | Counts how many cycles the input signal has been stable

Stable means equal to its previous value according to the 'Eq' instance.
The 'BitPack' instance is only used as a convenient way of intialization,
it resets to a previous value of @unpack 0@.
-}
stableFor ::
  forall n dom a.
  (KnownNat n, KnownDomain dom, Eq a, BitPack a, NFDataX a) =>
  Clock dom ->
  Reset dom ->
  Signal dom a ->
  Signal dom (Unsigned n)
stableFor clk rst = moore clk rst enableGen go snd (unpack 0, 0)
 where
  go :: (a, Unsigned n) -> a -> (a, Unsigned n)
  go (prev, cntr) inp
    | inp == prev = (prev, satSucc SatBound cntr)
    | otherwise = (inp, 0)

-- | Wrapper around 'stableFor' that checks the input has been stable for atleast @ms@ milliseconds
stableForMs ::
  forall ms dom a.
  (KnownNat ms, KnownDomain dom, Eq a, BitPack a, NFDataX a) =>
  SNat ms ->
  Clock dom ->
  Reset dom ->
  Signal dom a ->
  Signal dom Bool
stableForMs SNat clk rst inp =
  liftA2 (>=) stable (snatToNum (SNat @(PeriodToCycles dom (Milliseconds ms))))
 where
  stable = stableFor @(CLog 2 (PeriodToCycles dom (Milliseconds ms))) clk rst inp

-- | Top entity for this test. See module documentation for more information.
swCcTopologyTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
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
swCcTopologyTest refClkDiff sysClkDiff syncIn rxns rxps miso =
  (txns, txps, unbundle swFincFdecs, syncOut, spiDone, spiOut)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Ext200
  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset
  ilaControl@IlaControl{..} = ilaPlotSetup IlaPlotSetup{..}
  startTest = isJust <$> testConfig

  cfg = fromMaybe disabled <$> testConfig

  ( txns
    , txps
    , swFincFdecs
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
    , _ugnsStable
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

  -- allUgnsStable = and <$> bundle ugnsStable
  -- allStable' = allStable .&&. allUgnsStable

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

makeTopEntity 'swCcTopologyTest

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
                    , reframingEnabled = False
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
    Bool ->
    HitlTestCase HwTargetRef TestConfig CcConf
  tt clockShifts startDelays t r =
    HitlTestCase
      { name = topologyName t
      , parameters =
          Map.fromList
            $ toList
              ( zipWith5
                  testData
                  indicesI
                  (maybeVecToVecMaybe (map partsPerToSteps <$> clockShifts))
                  startDelays
                  (linkMasks @n t)
                  (repeat r)
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
            , reframe = r
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
    Bool ->
    (HwTargetRef, TestConfig)
  testData i initialClockShift startupDelay mask reframingEnabled =
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
    { topEntity = 'swCcTopologyTest
    , extraXdcFiles = []
    , externalHdl = []
    , testCases =
        [ -- detect the natual clock offsets to be elided from the later tests
          calibrateClockOffsets

          -- initial clock shifts   startup delays            topology          enable reframing?
        , tt (Just icsDiamond)      ((m *) <$> sdDiamond)     diamond           False
        , tt (Just icsComplete)     ((m *) <$> sdComplete)    (complete d3)     False
        , tt (Just icsCyclic)       ((m *) <$> sdCyclic)      (cyclic d5)       False
        , tt (Just icsTorus)        ((m *) <$> sdTorus)       (torus2d d2 d3)   False
        , tt (Just icsStar)         ((m *) <$> sdStar)        (star d7)         False
        , tt (Just icsLine)         ((m *) <$> sdLine)        (line d4)         False
        , tt (Just icsHourglass)    ((m *) <$> sdHourglass)   (hourglass d3)    False

          -- make sure the clock offsets detected during calibration is still the same
        , validateClockOffsetCalibration
        ]
    , mPostProc = Nothing
    }
{- FOURMOLU_ENABLE -}
