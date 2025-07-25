-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
module Bittide.Instances.Hitl.SwCcTopologies where

import Clash.Explicit.Prelude hiding (PeriodToCycles)
import qualified Clash.Explicit.Prelude as E
import Clash.Prelude (HiddenClockResetEnable, exposeReset, hasReset, withClockResetEnable)
import qualified Prelude as P

import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Proxy
import GHC.Float.RealFracMethods (roundFloatInteger)
import LiftType (liftTypeQ)
import Protocols.MemoryMap (MM, MemoryMap)
import System.FilePath ((</>))
import VexRiscv (JtagIn (..), JtagOut)

import Bittide.Arithmetic.PartsPer (PartsPer, ppm)
import Bittide.Arithmetic.Time
import Bittide.ClockControl hiding (speedChangeToFincFdec)
import Bittide.ClockControl.Callisto.Types (
  CallistoResult (allStable, jtagOut, maybeSpeedChange, stability),
  Stability (..),
 )
import Bittide.ClockControl.CallistoSw (SwControlConfig (..), callistoSwClockControl)
import Bittide.ClockControl.Si5395J
import Bittide.ClockControl.Si539xSpi (ConfigState (Error, Finished), si539xSpi)
import Bittide.Counter
import Bittide.ElasticBuffer
import Bittide.Extra.Maybe (orNothing)
import Bittide.Hitl
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.IlaPlot (
  IlaControl (..),
  IlaPlotSetup (..),
  callistoClockControlWithIla,
  ilaPlotSetup,
 )
import Bittide.Instances.Hitl.Setup (FpgaCount, LinkCount)
import Bittide.SharedTypes (withBittideByteOrder)
import Bittide.Simulate.Config (CcConf (..))
import Bittide.Topology (
  Topology (topologyName, topologyType),
  TopologyType (Complete),
  complete,
  cyclic,
  diamond,
  hourglass,
  line,
  star,
  torus2d,
 )
import Bittide.Transceiver (transceiverPrbsN)

import Clash.Annotations.TH (makeTopEntity)
import Clash.Class.Counter
import Clash.Cores.Xilinx.Ila (Depth (..), IlaConfig (..), ila, ilaConfig)
import Clash.Cores.Xilinx.VIO
import Clash.Cores.Xilinx.Xpm (xpmCdcArraySingle)
import Clash.Cores.Xilinx.Xpm.Cdc (xpmCdcSingle)
import Clash.Functor.Extra
import Clash.Sized.Extra (unsignedToSigned)
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Clash.Xilinx.ClockGen

import qualified Bittide.Arithmetic.PartsPer as PartsPer
import qualified Bittide.Instances.Hitl.Driver.SwCcTopologies as D
import qualified Bittide.Instances.Hitl.Setup as Setup
import qualified Bittide.Transceiver as Transceiver
import qualified Bittide.Transceiver.ResetManager as ResetManager
import qualified Clash.Cores.Xilinx.GTH as Gth
import qualified Data.Map.Strict as Map (fromList)

{- $setup
>>> import Clash.Explicit.Prelude
>>> import qualified Bittide.Arithmetic.PartsPer as PartsPer
-}

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
  -- Don't forget to update the value of f_step this value in "callisto.rs".
  PPB_10

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

>>> acceptableNoiseLevel == partsPerToSteps (PartsPer.ppb 1500)
True
-}
acceptableNoiseLevel :: FincFdecCount
acceptableNoiseLevel =
  -- This value corresponds to 1.5ppm. We can't calculate it directly, because
  -- we need it as a constant and we cannot use TemplateHaskell due to staging
  -- restrictions. The doctest above ensures that the value remains up to date.
  150

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
  -- ^ Whether or not to run this test with reframing enabled. See issue #693.
  }
  deriving (Generic, NFDataX, BitPack, Show)

clockControlConfig ::
  $(case (instancesClockConfig (Proxy @Basic125)) of (_ :: t) -> liftTypeQ @t)
clockControlConfig =
  $(lift (instancesClockConfig (Proxy @Basic125)))

txCounterStartUgn :: BitVector 63
txCounterStartUgn = 0x2abb_ccdd_eeff_1234

rxCounterStartUgn :: BitVector 64
rxCounterStartUgn = 0x9122_3344_1122_3344

type FifoSize = 5 -- = 2^5 = 32

memoryMap :: MemoryMap
memoryMap = mm
 where
  (SimOnly mm, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =
    topologyTest
      clockGen
      clockGen
      ( IlaControl
          { syncRst = resetGen
          , syncOut = pure False
          , syncStart = pure False
          , scheduledCapture = pure False
          , globalTimestamp = pure (0, 0)
          , skipTest = pure False
          }
      )
      (SimOnly (repeat 0))
      0
      0
      0
      (pure disabled)
      resetGen
      0
      (pure (JtagIn 0 0 0))

{- | Instantiates a hardware implementation of Callisto and exports its results. Can
be used to drive FINC/FDEC directly (see @FINC_FDEC@ result) or to tie the
results to a RiscV core (see 'riscvCopyTest')
-}
topologyTest ::
  "SMA_MGT_REFCLK_C" ::: Clock Ext200 ->
  "SYSCLK" ::: Clock Basic125 ->
  "ILA_CTRL" ::: IlaControl Basic125 ->
  "GTH_RX_S" ::: Gth.SimWires GthRx LinkCount ->
  "GTH_RX_NS" ::: Gth.Wires GthRxS LinkCount ->
  "GTH_RX_PS" ::: Gth.Wires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  "TEST_CFG" ::: Signal Basic125 TestConfig ->
  "PROG_EN" ::: Reset Basic125 ->
  "CALIBRATED_SHIFT" ::: Signal Basic125 FincFdecCount ->
  "JTAG" ::: Signal Basic125 JtagIn ->
  ( "MEMORY_MAP" ::: MM
  , "GTH_TX_S" ::: Gth.SimWires GthTx LinkCount
  , "GTH_TX_NS" ::: Gth.Wires GthTxS LinkCount
  , "GTH_TX_PS" ::: Gth.Wires GthTxS LinkCount
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
  , "CLOCK_SHIFT" ::: Signal Basic125 FincFdecCount
  , "allUgnsStable" ::: Signal Basic125 Bool
  , "noFifoOverflows" ::: Signal Basic125 Bool
  , "noFifoUnderflows" ::: Signal Basic125 Bool
  , "JTAG" ::: Signal Basic125 JtagOut
  )
topologyTest refClk sysClk IlaControl{syncRst = rst, ..} rxs rxNs rxPs miso cfg progEn ccs jtagIn =
  hwSeqX
    fincFdecIla
    ( mm
    , transceivers.txSims
    , transceivers.txNs
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
    , clockShift
    , allUgnsStable
    , noFifoOverflows
    , noFifoUnderflows
    , callistoResult.jtagOut
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
        , channelNames = Setup.channelNames
        , clockPaths = Setup.clockPaths
        , rxSims = rxs
        , rxNs
        , rxPs
        , txDatas = txCounters
        , txStarts = repeat (pure True)
        , rxReadys = repeat (pure True)
        }

  allReady =
    trueFor (SNat @(Milliseconds 500)) sysClk syncRst (and <$> bundle transceivers.linkReadys)
  transceiversFailedAfterUp =
    sticky sysClk syncRst (isFalling sysClk syncRst enableGen False allReady)

  othersNotInCCReset = maybe False (\val -> msb val == high) <<$>> transceivers.rxDatas

  othersNotInCCResetSync = zipWith go othersNotInCCReset transceivers.rxClocks
   where
    go sig rxClk = xpmCdcSingle rxClk sysClk sig

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
      <*> cfg.startupDelay

  -- Clock control
  clockControlReset =
    startupDelayRst
      `orReset` unsafeFromActiveLow ((==) <$> delayCount <*> cfg.startupDelay)

  clockMod = callistoResult.maybeSpeedChange
  allStable0 = callistoResult.allStable
  allStable1 = sticky sysClk syncRst allStable0

  ccConfig :: SwControlConfig Basic125
  ccConfig = SwControlConfig jtagIn cfg.reframingEnabled

  callistoSwClockControlInner ::
    forall nLinks eBufBits dom.
    ( HiddenClockResetEnable dom
    , KnownNat nLinks
    , KnownNat eBufBits
    , 1 <= nLinks
    , 1 <= eBufBits
    , nLinks + eBufBits <= 32
    , 1 <= DomainPeriod dom
    ) =>
    Reset dom ->
    -- \| Links suitable for clock control (i.e., recovered clocks won't go down
    -- again)
    Signal dom (BitVector nLinks) ->
    SwControlConfig dom ->
    -- \| Link mask
    Signal dom (BitVector nLinks) ->
    Vec nLinks (Signal dom (RelDataCount eBufBits)) ->
    (MM, Signal dom (CallistoResult nLinks))
  callistoSwClockControlInner extraRst linksUp ccConfig0 linkMask bufferCounts =
    withBittideByteOrder
      $ exposeReset
        (callistoSwClockControl ccConfig0 linkMask linksUp bufferCounts)
        newReset
   where
    oldReset = unsafeToActiveHigh hasReset
    extraRst1 = unsafeToActiveHigh extraRst
    newReset = unsafeFromActiveHigh $ oldReset .&&. extraRst1

  linksSuitableForCc = fmap pack $ bundle $ transceivers.handshakesDoneFree

  (mm, callistoResult) =
    callistoClockControlWithIla @LinkCount @CccBufferSize
      transceivers.txClock
      sysClk
      clockControlReset
      ccConfig
      (callistoSwClockControlInner progEn linksSuitableForCc)
      IlaControl{..}
      cfg.mask
      (resize <<$>> domainDiffs)

  fincFdecIla :: Signal Basic125 ()
  fincFdecIla =
    setName @"fincFdecIla"
      ila
      ( ilaConfig
          -- Use "fdi" (Frequency Decrement and Increment) to avoid collisions with other
          -- ILA net names.
          $ "trigger_fdi"
          :> "capture_fdi"
          :> "probe_fdi_milliseconds"
          :> "probe_allStable0"
          :> "probe_net_nFincs"
          :> "probe_ugn0"
          :> "probe_ugn1"
          :> "probe_ugn2"
          :> "probe_ugn3"
          :> "probe_ugn4"
          :> "probe_ugn5"
          :> "probe_ugn6"
          :> "probe_othersNotInCCReset"
          :> "probe_dDiff0"
          :> "probe_dDiff1"
          :> "probe_dDiff2"
          :> "probe_dDiff3"
          :> "probe_dDiff4"
          :> "probe_dDiff5"
          :> "probe_dDiff6"
          :> "probe_ugnsStable"
          :> "probe_fifoOverflows"
          :> "probe_fifoUnderflows"
          :> Nil
      )
        { depth = D16384
        }
      sysClk
      -- Trigger as soon as we come out of reset
      syncStart
      captureFlag
      -- Debug probes
      milliseconds1
      allStable0
      (fmap unsignedToSigned nFincs - fmap unsignedToSigned nFdecs)
      ugn0
      ugn1
      ugn2
      ugn3
      ugn4
      ugn5
      ugn6
      (bundle othersNotInCCResetSync)
      dDiff0
      dDiff1
      dDiff2
      dDiff3
      dDiff4
      dDiff5
      dDiff6
      (bundle ugnsStable)
      (bundle fifoOverflowsFree)
      (bundle fifoUnderflowsFree)

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
      (isFalling sysClk syncRst enableGen False ((== Just SpeedUp) <$> clockMod))
      (satSucc SatBound <$> nFincs)

  nFdecs =
    regEn
      sysClk
      clockControlReset
      enableGen
      (0 :: Unsigned 32)
      (isFalling sysClk syncRst enableGen False ((== Just SlowDown) <$> clockMod))
      (satSucc SatBound <$> nFdecs)

  -- Clock calibration

  clockShiftUpd = \case
    Just SpeedUp -> satSucc SatBound
    Just SlowDown -> satPred SatBound
    _ -> id

  notInCCReset = unsafeToActiveLow clockControlReset

  callistoEnteredPulse = isRising sysClk clockControlReset enableGen False (isJust <$> clockMod)

  clockShift =
    regEn
      sysClk
      syncRst
      enableGen
      (0 :: FincFdecCount)
      ( callistoEnteredPulse
          .&&. notInCCReset
          .&&. (/= NoCCCalibration)
          . (.calibrate)
          <$> cfg
      )
      (clockShiftUpd <$> clockMod <*> clockShift)

  -- Initial Clock adjustment

  -- without the additional delay of 1 second here, some of the
  -- initial FINC/FDECs prior to test start will be lost.
  adjustStart = trueFor (SNat @(Seconds 1)) sysClk syncRst spiDone
  clocksAdjusted =
    spiDone
      .&&. ( (/= NoCCCalibration)
              . (.calibrate)
              <$> cfg
              .||. (==)
              <$> initialAdjust
              <*> adjustCount
           )
  adjusting = adjustStart .&&. (not <$> clocksAdjusted)
  adjustRst = unsafeFromActiveLow adjustStart

  initialAdjust = (+) <$> ccs <*> (fromMaybe 0 <$> cfg.initialClockShift)

  adjustCountEnable = mux ((== minBound) <$> adjustCount) (pure True) setupEnteredPulse

  adjustCount =
    regEn
      sysClk
      adjustRst
      enableGen
      (0 :: FincFdecCount)
      (adjusting .&&. adjustCountEnable)
      $ upd
      <$> setupAdjustments
      <*> adjustCount
   where
    upd SpeedUp = satSucc SatBound
    upd SlowDown = satPred SatBound
    upd _ = id

  setupAdjust = opSelect <$> initialAdjust <*> adjustCount
   where
    opSelect calib adjust = case compare calib adjust of
      LT -> SlowDown
      EQ -> NoChange
      GT -> SpeedUp

  (setupState, setupAdjustments) =
    unbundle
      $ speedChangeToFincFdec @Si539xHoldTime @Si539xMinUpdatePeriod sysClk adjustRst setupAdjust
  setupEnteredPulse = isRising sysClk adjustRst enableGen False (inPulse <$> setupState)
   where
    inPulse = \case
      Pulse _ _ -> True
      _ -> False

  frequencyAdjustments :: Signal Basic125 (FINC, FDEC)
  frequencyAdjustments =
    E.delay sysClk enableGen minBound
      $ mux
        adjusting
        (speedChangeToPins <$> setupAdjustments)
        ( speedChangeToStickyPins sysClk clockControlReset enableGen (SNat @Si539xHoldTime) clockMod
        )

  domainDiffs :: Vec LinkCount (Signal Basic125 FincFdecCount)
  domainDiffs =
    zipWith3
      (domainDiffCounterExt sysClk)
      (orReset clockControlReset . unsafeFromActiveLow <$> othersNotInCCResetSync)
      transceivers.rxClocks
      (repeat transceivers.txClock)

  txAllStable = xpmCdcSingle sysClk transceivers.txClock allStable1
  txReset2 =
    orReset
      transceivers.txReset
      (unsafeFromActiveLow txAllStable)

  txNotInCCResets :: Vec LinkCount (Signal GthTx Bool)
  txNotInCCResets = go <$> (repeat transceivers.txClock)
   where
    go txClk = unsafeSynchronizer sysClk txClk notInCCReset

  txCounters = zipWith3 txCounter (repeat transceivers.txClock) (repeat txReset2) txNotInCCResets
  txCounter txClk txRst notInCCReset' = result
   where
    notInCCReset'' :: Signal GthTx (BitVector 1)
    notInCCReset'' = boolToBV <$> notInCCReset'
    dataCounter = register txClk txRst enableGen txCounterStartUgn (dataCounter + 1)
    counter = mux notInCCReset' dataCounter 0
    result = (++#) <$> notInCCReset'' <*> counter
  -- see NOTE [magic start values]

  rxFifos =
    zipWith4
      go
      (repeat transceivers.txClock)
      transceivers.rxClocks
      (repeat txReset2)
      transceivers.rxDatas
   where
    go = resettableXilinxElasticBuffer @FifoSize @_ @_ @(Maybe (BitVector 64))

  (_fillLvls, fifoUnderflowsTx, fifoOverflowsTx, _ebMode, mRxCntrs) = unzip5 rxFifos
  rxCntrs = zipWith go txNotInCCResets mRxCntrs
   where
    go txRst = regMaybe transceivers.txClock (unsafeFromActiveLow txRst) enableGen rxCounterStartUgn

  fifoOverflowsFree :: Vec LinkCount (Signal Basic125 Overflow)
  fifoOverflowsFree = map (xpmCdcSingle transceivers.txClock sysClk) fifoOverflowsTx
  fifoUnderflowsFree :: Vec LinkCount (Signal Basic125 Underflow)
  fifoUnderflowsFree = map (xpmCdcSingle transceivers.txClock sysClk) fifoUnderflowsTx

  ugns1 :: Vec LinkCount (Signal GthTx (BitVector 64))
  ugns1 = zipWith (-) txCounters rxCntrs
  -- see NOTE [magic start values]

  ugns2 :: Vec LinkCount (Signal Basic125 (BitVector 64))
  ugns2 = zipWith go othersNotInCCResetSync ugns1
   where
    go enaSig =
      regEn
        sysClk
        clockControlReset
        enableGen
        rxCounterStartUgn
        enaSig
        . xpmCdcArraySingle transceivers.txClock sysClk

  ugnsStable :: Vec LinkCount (Signal Basic125 Bool)
  ugnsStable = (.stable) <<$>> (unbundle callistoResult.stability)

  maskWithCfg ::
    Bool ->
    Vec LinkCount (Signal Basic125 Bool) ->
    Signal Basic125 (Vec LinkCount Bool)
  maskWithCfg dflt = liftA2 go1 cfg.mask . bundle
   where
    go1 m = zipWith go2 (bitCoerce m)
    go2 m val = if m then val else dflt

  allUgnsStable =
    trueFor (SNat @(Seconds 2)) sysClk clockControlReset
      $ and
      <$> maskWithCfg True ugnsStable

  findFifoError bits = result
   where
    masked = maskWithCfg False bits
    observingError = or <$> masked
    observedError = sticky sysClk clockControlReset observingError
    result = not <$> observedError

  noFifoOverflows = findFifoError fifoOverflowsFree
  noFifoUnderflows = findFifoError fifoUnderflowsFree

  -- NOTE [magic start values]
  -- These values could be anything, but are chosen to be recognisable and help debugging.
  --   0x2abbccddeeff1234 - 0x1122334411223344 = 0x19999999dddcdef0
  -- If you ever see the ugn being a constant 0x19999999dddcdef0
  -- then you know the your counter isn't running and you're receiving 'Nothing',
  -- If you see 0x19999999.......... and it's counting up, then you're receiving Nothing,
  -- but your counter is running.

  ( ugn0
    , ugn1
    , ugn2
    , ugn3
    , ugn4
    , ugn5
    , ugn6
    ) = vecToTuple ugns2

  ( dDiff0
    , dDiff1
    , dDiff2
    , dDiff3
    , dDiff4
    , dDiff5
    , dDiff6
    ) = vecToTuple domainDiffs

type WaitCycles dom hold prd = PeriodToCycles dom (prd - hold)

data ToFincFdecState dom hold min
  = Wait (Index (WaitCycles dom hold min))
  | Pulse (Index (PeriodToCycles dom hold)) SpeedChange
  | Idle
  deriving (Generic, NFDataX, Eq)

speedChangeToFincFdec ::
  forall hold prd dom.
  (KnownDomain dom, KnownNat hold, KnownNat prd) =>
  (hold + 1 <= prd) => -- 'hold < prd' becomes 'hold + 1 <= prd'
  Clock dom ->
  Reset dom ->
  Signal dom SpeedChange ->
  Signal dom (ToFincFdecState dom hold prd, SpeedChange)
speedChangeToFincFdec clk rst =
  dflipflop clk . mealy clk rst enableGen go1 (Wait maxBound)
 where
  go1 ::
    ToFincFdecState dom hold prd ->
    SpeedChange ->
    (ToFincFdecState dom hold prd, (ToFincFdecState dom hold prd, SpeedChange))
  go1 state@(Wait n) _s
    | n == 0 = (Idle, (state, NoChange))
    | otherwise = (Wait (n - 1), (state, NoChange))
  go1 state@(Pulse n s) _s
    | n == 0 = (Wait maxBound, (state, s))
    | otherwise = (Pulse (n - 1) s, (state, s))
  go1 Idle NoChange = (Idle, (Idle, NoChange))
  go1 Idle s = (Pulse maxBound s, (Idle, NoChange))

regEnOnce ::
  forall dom a.
  (KnownDomain dom, NFDataX a) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  a ->
  Signal dom Bool ->
  Signal dom a ->
  Signal dom a
regEnOnce clk rst ena dft enaS input = fromMaybe dft <$> output
 where
  output =
    regEn
      clk
      rst
      ena
      Nothing
      (isNothing <$> output)
      (orNothing <$> enaS <*> input)

-- | Top entity for this test. See module documentation for more information.
swCcTopologyTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "SYNC_IN" ::: Signal Basic125 Bool ->
  "GTH_RX_S" ::: Gth.SimWires GthRx LinkCount ->
  "GTH_RX_NS" ::: Gth.Wires GthRxS LinkCount ->
  "GTH_RX_PS" ::: Gth.Wires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  "JTAG" ::: Signal Basic125 JtagIn ->
  ( "GTH_TX_S" ::: Gth.SimWires GthTx LinkCount
  , "GTH_TX_NS" ::: Gth.Wires GthTxS LinkCount
  , "GTH_TX_PS" ::: Gth.Wires GthTxS LinkCount
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
  , "JTAG" ::: Signal Basic125 JtagOut
  )
swCcTopologyTest refClkDiff sysClkDiff syncIn rxs rxns rxps miso jtagIn =
  hwSeqX
    tleDebugIla
    (txs, txns, txps, unbundle swFincFdecs, syncOut, spiDone, spiOut, jtagOut)
 where
  refClk = Gth.ibufds_gte3 refClkDiff :: Clock Ext200
  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset
  ilaControl@IlaControl{..} = ilaPlotSetup IlaPlotSetup{..}
  startTest = isJust <$> testConfig

  testCounter =
    regEn
      sysClk
      sysRst
      enableGen
      (0 :: Unsigned 4)
      (isFalling sysClk sysRst enableGen False startTest)
      (satSucc SatBound <$> testCounter)

  cfg = fromMaybe disabled <$> testConfig

  ( _mm
    , txs
    , txns
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
    , clockShift
    , allUgnsStable
    , noFifoOverflows
    , noFifoUnderflows
    , jtagOut
    ) =
      topologyTest
        refClk
        sysClk
        ilaControl{skipTest = skip}
        rxs
        rxns
        rxps
        miso
        cfg
        progEnRst
        calibratedClockShift
        jtagIn

  captureFlag =
    riseEvery
      sysClk
      syncRst
      enableGen
      (SNat @(PeriodToCycles Basic125 (Milliseconds 1)))

  milliseconds1 =
    regEn
      sysClk
      syncRst
      enableGen
      (0 :: Unsigned 16)
      captureFlag
      (satSucc SatBound <$> milliseconds1)

  tleDebugIla :: Signal Basic125 ()
  tleDebugIla =
    setName @"tleDebugIla"
      ila
      ( ilaConfig
          -- Use "tle" (Top Level Entity) to avoid collisions with other ILA net names.
          $ "trigger_tle_0"
          :> "capture_tle_0"
          :> "probe_tle_milliseconds"
          :> "probe_ilacfg_allReady"
          :> "probe_ilacfg_startTest"
          :> "probe_ilactl_syncStart"
          :> "probe_startTest"
          :> "probe_allStable"
          :> "probe_tle_allUgnsStable"
          :> "probe_tle_noFifoOverflows"
          :> "probe_tle_noFifoUnderflows"
          :> "probe_endSuccess"
          :> "probe_startBeforeAllReady"
          :> "probe_tle_transceiversFailedAfterUp"
          :> "probe_testDone"
          :> "probe_testSuccess"
          :> Nil
      )
        { depth = D16384
        }
      sysClk
      -- syncNodePrevEnteredReset
      (unsafeToActiveHigh syncRst)
      captureFlag
      milliseconds1
      allReady
      startTest
      syncStart
      startTest
      allStable
      allUgnsStable
      noFifoUnderflows
      noFifoOverflows
      endSuccess
      startBeforeAllReady
      transceiversFailedAfterUp
      testDone
      testSuccess

  -- Check that tests are not synchronously started before all transceivers are up
  startBeforeAllReady =
    sticky
      sysClk
      syncRst
      (syncStart .&&. ((not <$> allReady) .||. transceiversFailedAfterUp))

  calibratedClockShift = capturedOnce
   where
    -- Possibly overkill (unsure if capturing needs this many conditions, but it works).
    testSuccessRising = isRising sysClk sysRst enableGen False testSuccess
    cfgIsCalibrate = (\c -> c.calibrate == CCCalibrate) <$> cfg
    captureCond = ((== 0) <$> testCounter) .&&. cfgIsCalibrate .&&. testSuccessRising
    capturedOnce = regEnOnce sysClk sysRst enableGen 0 captureCond clockShift

  offsetFromCalibrated = (-) <$> clockShift <*> calibratedClockShift
  withinNoiseLevel = (< acceptableNoiseLevel) . abs <$> offsetFromCalibrated

  notValidatingCalibration = (/= CCCalibrationValidation) <$> cfg.calibrate

  fifoSuccess = noFifoUnderflows .&&. noFifoOverflows

  endSuccess :: Signal Basic125 Bool
  endSuccess =
    trueFor (SNat @(Seconds 5)) sysClk syncRst
      $ allStable
      .&&. allUgnsStable
      .&&. fifoSuccess
      .&&. (notValidatingCalibration .||. withinNoiseLevel)

  skip = maybe False (not . (.fpgaEnabled)) <$> testConfig

  testDone =
    startTest
      .&&. ( skip
              .||. endSuccess
              .||. transceiversFailedAfterUp
              .||. startBeforeAllReady
              .||. (not <$> fifoSuccess)
           )

  testSuccess =
    skip
      .||. ( allStable
              .&&. allUgnsStable
              .&&. fifoSuccess
              .&&. (not <$> (transceiversFailedAfterUp .||. startBeforeAllReady))
           )

  (unbundle -> (testStart, testConfig0, progEn)) =
    setName @"vioHitlt"
      $ vioProbe
        ("probe_test_done" :> "probe_test_success" :> Nil)
        ("probe_test_start" :> "probe_test_data" :> "probe_prog_en" :> Nil)
        (False, disabled, False)
        sysClk
        testDone
        testSuccess

  testConfig = mux testStart (Just <$> testConfig0) (pure Nothing)
  progEnRst = unsafeFromActiveLow progEn
{-# OPAQUE swCcTopologyTest #-}

makeTopEntity 'swCcTopologyTest

swCcOneTopologyTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "SYNC_IN" ::: Signal Basic125 Bool ->
  "GTH_RX_S" ::: Gth.SimWires GthRx LinkCount ->
  "GTH_RX_NS" ::: Gth.Wires GthRxS LinkCount ->
  "GTH_RX_PS" ::: Gth.Wires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  "JTAG" ::: Signal Basic125 JtagIn ->
  ( "GTH_TX_S" ::: Gth.SimWires GthTx LinkCount
  , "GTH_TX_NS" ::: Gth.Wires GthTxS LinkCount
  , "GTH_TX_PS" ::: Gth.Wires GthTxS LinkCount
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
  , "JTAG" ::: Signal Basic125 JtagOut
  )
swCcOneTopologyTest = swCcTopologyTest
{-# OPAQUE swCcOneTopologyTest #-}
makeTopEntity 'swCcOneTopologyTest

tests :: [HitlTestGroup]
tests = [testGroup True, testGroup False]
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
      , reframe = cccEnableReframing
      , waitTime = fromEnum cccReframingWaitTime
      }

  -- Measure clock offsets. Used to get clocks to a common start point at start of test
  calibrateClockOffsets = calibrateCC False

  -- Measure clock offsets again. Verify that they haven't changed since start of test
  validateClockOffsetCalibration = calibrateCC True

  calibrateCC :: Bool -> HitlTestCase HwTargetRef TestConfig CcConf
  calibrateCC validate =
    HitlTestCase
      { name = (if validate then "zzz_validate" else "0_calibrate") <> "_clock_offsets"
      , parameters =
          Map.fromList $ Setup.allHwTargets
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
            { ccTopologyType = Complete $ natToNum @FpgaCount
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
      { name = t.topologyName
      , parameters =
          Map.fromList
            $ toList
              ( zipWith5
                  testData
                  indicesI
                  (maybeVecToVecMaybe (map partsPerToSteps <$> clockShifts))
                  startDelays
                  (Setup.linkMasks @n t)
                  (repeat r)
              )
            <> [ (HwTargetByIndex (fromInteger i), disabled)
               | let n = natToNum @n
               , i <- [n, n + 1 .. natToNum @LinkCount]
               ]
      , postProcData =
          defSimCfg
            { ccTopologyType = t.topologyType
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

  testGroup justOneTest =
    HitlTestGroup
      { topEntity = if justOneTest then 'swCcOneTopologyTest else 'swCcTopologyTest
      , extraXdcFiles = ["jtag" </> "config.xdc", "jtag" </> "pmod1.xdc"]
      , externalHdl = []
      , testCases = calibrateClockOffsets : cases <> [validateClockOffsetCalibration]
      , mDriverProc = Just D.driverFunc
      , mPostProc = Nothing
      }
   where
    cases
      | justOneTest = P.take 1 allCases
      | otherwise = allCases

{- FOURMOLU_DISABLE -} -- fourmolu doesn't do well with tabular structures
  allCases = [
      -- initial clock shifts   startup delays            topology          enable reframing?
      tt (Just icsDiamond)      ((m *) <$> sdDiamond)     diamond           False
    , tt (Just icsComplete)     ((m *) <$> sdComplete)    (complete d3)     False
    , tt (Just icsCyclic)       ((m *) <$> sdCyclic)      (cyclic d5)       False
    , tt (Just icsTorus)        ((m *) <$> sdTorus)       (torus2d d2 d3)   False
    , tt (Just icsStar)         ((m *) <$> sdStar)        (star d7)         False
    , tt (Just icsLine)         ((m *) <$> sdLine)        (line d4)         False
    , tt (Just icsHourglass)    ((m *) <$> sdHourglass)   (hourglass d3)    False
    ]
{- FOURMOLU_ENABLE -}
