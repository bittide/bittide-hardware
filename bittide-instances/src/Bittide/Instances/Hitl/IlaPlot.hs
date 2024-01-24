-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | ILA extension for the generation of plot data monitoring the
-- clock modifications and the data counts of the elastic buffers. The
-- extension is intended to be used by the hardware-in-the-loop tests.
module Bittide.Instances.Hitl.IlaPlot
  ( -- * Parameters
    SyncPulsePeriod
  , ScheduledCapturePeriod
  , AccWindowHeight
  , CompressedBufferSize
  , MaxPulseCount
    -- * Timestamp Types
  , GlobalTimestamp
  , LocalTimestamp
    -- * Interface Types
  , CaptureCondition(..)
  , IlaPlotSetup(..)
  , IlaControl(..)
  , PlotData(..)
  , RfStageChange(..)
    -- * ILA Plot
  , ilaPlotSetup
  , callistoClockControlWithIla
    -- * Helpers
  , SyncPulseCycles
  , ScheduledCaptureCycles
  , DDiv
  , DDivCheck
  , trueFor
  , accWindow
  , overflowResistantDiff
  , DiffResult(..)
  , syncOutGenerator
  , syncInRecover
  ) where

import GHC.Stack (HasCallStack)

import Clash.Explicit.Prelude
import Clash.Explicit.Signal.Extra
import Clash.Sized.Extra (concatUnsigneds)

import Bittide.Arithmetic.Time (Seconds, Milliseconds, PeriodToCycles)
import Bittide.ClockControl (SpeedChange(..), DataCount, ClockControlConfig)
import Bittide.ClockControl.Callisto
  (CallistoResult(..), ReframingState(..), callistoClockControl)
import Bittide.ClockControl.StabilityChecker
import Bittide.Extra.Maybe (orNothing)

import Clash.Cores.Xilinx.Xpm.Cdc.Gray (xpmCdcGray)
import Clash.Cores.Xilinx.Xpm.Cdc.Single (xpmCdcSingle)
import Clash.Cores.Xilinx.Ila (IlaConfig(..), Depth(..), ila, ilaConfig)
import Clash.Explicit.Reset.Extra

import Control.Arrow ((***), second)
import Data.Bool (bool)
import Data.Maybe (isJust, fromMaybe)
import Data.Constraint.Nat.Extra
  (Dict(..), SatSubZero, satSubZeroMin, minLeq, maxGeqPlus)

-- | Divisible division operation, which ensures that the dividend is
-- always a multiple of the divisor. Type family resolution will get
-- /stuck/ if the dividend is not a multiple of the divisor.
type family DDiv (a :: Nat) (b :: Nat) :: Nat where
  DDiv a b = DDivCheck (Mod a b) a b

-- | Helper type family for checking the reminder of
-- 'DDiv'. Unfortunately type families cannot be scoped.
type family DDivCheck (a :: Nat) (b :: Nat) (c :: Nat) :: Nat where
  DDivCheck 0 a b = Div a b

-- | The window high of 'accWindow' for reducing the number of
-- reported clock modifications.
type AccWindowHeight = 5 :: Nat

-- | The period of the sync pulse used to share a synchronized time
-- stamp between the nodes.
type SyncPulsePeriod = Milliseconds 5

-- | The period of the scheduled capture (must be a multiple of 'SyncPulsePeriod').
type ScheduledCapturePeriod = Milliseconds 20

-- | An upper bound on the number of synchronized pulses during a test
-- run. The bound allows to count pulses up to 5 minutes without
-- producing an overflow. We assume that the test has finished or was
-- canceled within that time.
type MaxPulseCount = DDiv (5 * 60 * Seconds 1) SyncPulsePeriod

-- | The number of cycles within the given domain that fit into one
-- sync pulse period.
type SyncPulseCycles dom = PeriodToCycles dom SyncPulsePeriod

-- | A global timestamp consisting of the number of synchronized
-- pulses received and the number of cycles of the local stable clock
-- (identified by the given domain and starting with 'syncStart'). The
-- local clock cycle counter can count up to 10% more cycles than
-- mathematically required to compensate potential drifts of the local
-- stable clock.
type GlobalTimestamp dom =
  ( Index MaxPulseCount
  , Index (Div (SyncPulseCycles dom) 10 + SyncPulseCycles dom)
  )

-- | The number of cycles within the given domain fitting into one
-- scheduled capture period.
type ScheduledCaptureCycles dom = PeriodToCycles dom ScheduledCapturePeriod

-- | The local timestamp counting the cycles of the dynamic clock
-- since the last scheduled capture (starting with clock control).
-- The counter can count up to 10% more cycles than mathematically
-- required to compensate for clock changes resulting from
-- 'Bittide.ClockControl.SpeedUp' and 'Bittide.ClockControl.SlowDown'
-- applications.
type LocalTimestamp dom = Index (ScheduledCaptureCycles dom + Div
  (ScheduledCaptureCycles dom) 10)

-- | The number of pulses it takes until a scheduled capture gets
-- triggered.
type ScheduledPulseCount = DDiv ScheduledCapturePeriod SyncPulsePeriod

-- | The reduced elastic buffer size to be used for reporting only the
-- difference since the last capture.
type CompressedBufferSize = 16 :: Nat

-- | The type of change with respect to the stage of 'ReframingState'.
data RfStageChange =
    Stable
    -- ^ Indicates that the reframing stage is stable and does not change.
  | ToDetect
    -- ^ Indicates that the reframing stage changed to the @DETECT@ state.
  | ToWait
    -- ^ Indicates that the reframing stage changed to the @WAIT@ state.
  | ToDone
    -- ^ Indicates that the reframing stage changed to the @DONE@ state.
  deriving (Eq, Generic, BitPack, NFDataX)

-- | The ILA capture type.
data CaptureCondition =
    UntilTrigger
    -- ^ Identifies captures happening with or before the trigger.
    --
    -- Note that we always need to capture everything before the
    -- trigger fires, because the data that the ILA captures is
    -- undefined otherwise. Moreover, 'syncStart' does not hold at the
    -- trigger, but only after it. Hence, if the trigger position is
    -- at 0, then we store exactly one capture that is marked with the
    -- 'UntilTrigger' flag this way.
  | Calibrate
    -- ^ Identifies scheduled captures during the initial calibration
    -- period.
  | Scheduled
    -- ^ Identifies scheduled captures after the initial calibration
    -- period.
  | DataChange
    -- ^ Identifies intermediate captures that are triggered by data
    -- changes.
  deriving (Eq, Generic, NFDataX, BitPack)

-- | All signals, as they are required for using clock control with
-- ILA plotting capabilities.
data IlaPlotSetup dom =
  IlaPlotSetup
    { sysClk :: Clock dom
      -- ^ The stable system clock.
    , sysRst :: Reset dom
      -- ^ The system's reset line.
    , allUp :: Signal dom Bool
      -- ^ A boolean signal indicating that all transceivers are up.
    , startTest :: Signal dom Bool
      -- ^ The test start signal coming from the HITLT VIO interface.
    , syncIn :: Signal dom Bool
      -- ^ The signal connected to @SYNC_IN@.
    }

-- | All signals, as they are required by the ILA trigger and capture
-- conditions. You must use 'ilaPlotSetup' for generating them.
data IlaControl dom =
  IlaControl
    { syncRst :: Reset dom
      -- ^ Synchronized reset line, which is only deasserted during
      -- the actual test.
    , syncOut :: Signal dom Bool
      -- ^ The signal to be passed to @SYNC_OUT@, which
      -- is only connected for the last node in the network and wired back
      -- to @SYNC_IN@ of all nodes from there.
      --
      -- Note that all nodes are in reset before their local
      -- 'startTest' VIO signal gets asserted, as 'startTest' is
      -- directly driving 'sysRst'. Thus, for the other nodes to
      -- capture the @SYNC_OUT@ signal correctly, the node receiving
      -- the `startTest` rising edge last must be the one with it's
      -- @SYNC_OUT@ physically connected to the @SYNC_IN@ of all nodes
      -- in the network. This assumption is tested by
      -- 'Bittide.Instances.Hitl.SyncInSyncOut'.
    , syncStart :: Signal dom Bool
      -- ^ Synchronized test start trigger
    , scheduledCapture :: Signal dom Bool
      -- ^ Synchronized pre-scheduled capture trigger
    , globalTimestamp :: Signal dom (GlobalTimestamp dom)
      -- ^ Synchronized pulse counter
    }

-- | The ILA plot setup controller.
ilaPlotSetup ::
  forall dom. HasCallStack =>
  (HasDefinedInitialValues dom, HasSynchronousReset dom) =>
  IlaPlotSetup dom ->
  -- ^ required input signals
  IlaControl dom
ilaPlotSetup IlaPlotSetup{..} = IlaControl{..}
 where
  -- 'syncOutGenerator' is used to drive 'SYNC_OUT'.
  syncOut =
      dflipflop sysClk
    $ syncOutGenerator sysClk startTest
    $ trueFor (SNat @(Seconds 5)) sysClk syncRst allUp

  -- first synchronize SYNC_IN to the local clock and filter from
  -- potential glitches
  syncInFiltered =
      unsafeToActiveLow
    $ resetGlitchFilter (SNat @128) sysClk
    $ unsafeFromActiveLow
    $ xpmCdcSingle sysClk sysClk syncIn

  -- generate a pulse on every change of SYNC_IN
  syncInChangepoints =
    changepoints sysClk sysRst enableGen syncInFiltered

  -- recover the activity and readiness states from SYNC_IN
  (syncActive, syncStart) =
    unbundle $ syncInRecover sysClk sysRst startTest syncInFiltered

  -- tests are reset on `sysRst` or if not synchronously active
  syncRst = sysRst `orReset` unsafeFromActiveLow syncActive

  -- generate the global timestamp from the synchronous rising and
  -- falling edges of SYNC_IN
  globalTimestamp = register sysClk syncRst enableGen (0,0) $
    mux syncInChangepoints
      (((+1) *** const 0) <$> globalTimestamp)
      (second (+1) <$> globalTimestamp)

  scheduledCapture = syncStart .&&.
    mealy sysClk syncRst enableGen
      (\c i -> (if i then satSucc SatWrap c else c, i && c == minBound))
      (minBound :: Index ScheduledPulseCount)
      syncInChangepoints

-- | Rises after the incoming signal has been 'True' for the specified
-- amount of time.
trueFor ::
  forall dom t. HasCallStack =>
  (KnownDomain dom, KnownNat t) =>
  SNat t ->
  -- ^ Use the type aliases of 'Bittide.Arithmetic.Time' for time span
  -- specification.
  Clock dom ->
  Reset dom ->
  Signal dom Bool ->
  Signal dom Bool
trueFor _ clk rst =
  moore clk rst enableGen transF (== maxBound)
    (0 :: Index (PeriodToCycles dom t))
 where
  transF counter = \case
    True -> satSucc SatBound counter
    _    -> 0

-- | A single data type for covering all of the non-clock related data
-- to be included into a capture.
data PlotData (n :: Nat) (m :: Nat) =
  PlotData
    { dEBData        :: Vec n (DataCount m, Maybe Bool, Maybe Bool)
    , dSpeedChange   :: SpeedChange
    , dRfStageChange :: RfStageChange
    }
  deriving (Generic, NFDataX, BitPack)

-- | Accumulates over multiple @FINC@/@FDEC@s to reduce the number of
-- captures recorded by the ILA (which are mostly jitter otherwise).
--
-- The compression technique works as follows: if both @FINC@ and
-- @FDEC@ are requested after each other, then they cancel each other
-- out and are not reported. Hence, only @FINC@/@FDEC@s are reported
-- that haven't canceled out before they exceed the @n@
-- boundary. Thus, for example @FINC@, @FINC@, @FDEC@, @FDEC@, ... is
-- not reported for @n > 2@ as the first two @FINC@s don't exceed the
-- boundary @n@.
accWindow ::
  forall height dom. HasCallStack =>
  (KnownNat height, KnownDomain dom) =>
  SNat height ->
  -- ^ The height of the accumulation window.
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom SpeedChange ->
  Signal dom SpeedChange
accWindow _ clk rst ena =
  mealy clk rst ena transF (True, minBound :: Index height)
 where
  transF (d,s) = \case
    NoChange -> ((d, s), NoChange)
    x        -> let d' = if x == SpeedUp then d else not d in if
      |     d' && s == maxBound -> ((not d, minBound), x       )
      | not d' && s == minBound -> ((not d, minBound), NoChange)
      |     d'                  -> ((d,     s + 1),    NoChange)
      | otherwise               -> ((d,     s - 1),    NoChange)

-- | Calculates the difference of a wrapping counter between two
-- points in time taking potential overflows into account. The
-- captured difference is wrapped into an option type, which defaults
-- to 'TooLarge' as soon as the difference exceeds the capacity of the
-- returned index type.
--
-- The difference is measured against a stored reference, which is
-- taken from the counter whenever the additional input signal gets
-- asserted. 'NoReference' is output before the first assertion of this
-- line.
--
-- The counter is assumed to only increase over time and may overflow
-- several times until the next value gets stored. This means that the
-- returned difference is measured against the point in time where the
-- reference was stored (accumulating every overflow since then) and
-- not against the actual value of the counter at that time. The
-- counter can increase by any value in the range of the counter's
-- type per cycle.
overflowResistantDiff ::
  forall dom n m. HasCallStack =>
  (KnownDomain dom, KnownNat n, KnownNat m) =>
  (1 <= n, 1 <= m) =>
  Clock dom ->
  Reset dom ->
  -- | Take the current counter value as the new reference if asserted
  Signal dom Bool ->
  -- | The input counter
  Signal dom (Unsigned n) ->
  -- | The measured difference as long as it fits into the output type
  Signal dom (DiffResult (Index m))
overflowResistantDiff clk rst trg cnt =
  mealy clk rst enableGen transF NoReference $ bundle (cnt, trg)
 where
  transF state (curValue, newRef) =
    if newRef
    then (Difference (curValue, 0, 0), Difference 0)
    else case state of
      TooLarge    -> (TooLarge,    TooLarge)
      NoReference -> (NoReference, NoReference)
      Difference (refValue, prevOverflows, prevDiff) ->
        let curDiff = curValue - refValue
            curOverflows =
              if curDiff < prevDiff
              then satSucc SatError prevOverflows
              else prevOverflows
         in if    prevOverflows == maxOverflows && curDiff < prevDiff
               ||  curOverflows == maxOverflows && curDiff > maxDiff
            then (TooLarge, TooLarge)
            else (Difference (refValue, curOverflows, curDiff), )
              $ case satSubZeroMin @(BitSize (Index m)) @n of
                  Dict -> case minLeq @(BitSize (Index m)) @n of
                    Dict -> Difference
                      $ bitCoerce
                      $ concatUnsigneds curOverflows
                      $ checkedTruncateB
                          @(Min (BitSize (Index m)) n)
                          @(n - Min (BitSize (Index m)) n)
                          curDiff

  maxDiff :: Unsigned n
  maxDiff = natToNum @(Mod (m-1) (2^n))

  maxOverflows :: Unsigned (SatSubZero (BitSize (Index m)) n)
  maxOverflows =
    let x :: Unsigned (BitSize (Index m))
        x = bitCoerce (maxBound :: Index m) `shiftR` natToNum @n
     in case satSubZeroMin @(BitSize (Index m)) @n of
          Dict -> checkedTruncateB x

-- | The result of 'overflowResistantDiff'.
data DiffResult a =
    NoReference
    -- ^ Wait for the first pulse to store the initial reference
    -- value.
  | Difference a
    -- ^ The accumulated difference since the last value has been
    -- stored.
  | TooLarge
    -- ^ Indicates that the difference against the last stored
    -- reference got to large to fit into the output type.
  deriving (Generic, BitPack, NFDataX, Functor, Eq, Ord, Show)

{-# NOINLINE callistoClockControlWithIla #-}
-- | Wrapper on 'Bittide.ClockControl.Callisto.callistoClockControl'
-- additionally dumping all the data that is required for producing
-- plots of the clock control behavior.
callistoClockControlWithIla ::
  forall n m sys dyn margin framesize. HasCallStack =>
  (KnownDomain dyn , KnownDomain sys, HasSynchronousReset sys) =>
  (KnownNat n, KnownNat m, KnownNat margin, KnownNat framesize) =>
  (1 <= n, 1 <= m, n + m <= 32, 1 <= framesize, 6 + n * (m + 4) <= 1024) =>
  CompressedBufferSize <= m =>
  Clock dyn ->
  Clock sys ->
  Reset sys ->
  ClockControlConfig sys m margin framesize ->
  IlaControl sys ->
  -- ^ Ila trigger and capture conditions
  Signal sys (BitVector n) ->
  -- ^ Link availability mask
  Vec n (Signal sys (DataCount m)) ->
  -- ^ Statistics provided by elastic buffers.
  Signal sys (CallistoResult n)
callistoClockControlWithIla dynClk clk rst ccc IlaControl{..} mask ebs =
  hwSeqX ilaInstance (muteDuringCalibration <$> calibrating <*> result)
 where
  result = callistoClockControl clk rst enableGen ccc mask ebs

  maxGeqPlusApp = maxGeqPlus @1
    @(DivRU ScheduledCapturePeriod (Max 1 (DomainPeriod dyn)))
    @(Div (ScheduledCaptureCycles dyn) 10)

  -- local timestamp on the stable clock
  localTs :: Signal sys (DiffResult (LocalTimestamp dyn))
  localTs = case maxGeqPlusApp of
    Dict -> overflowResistantDiff clk rst
              (delay clk enableGen False (isJust <$> captureCond))
          $ let ccRst = xpmResetSynchronizer Asserted clk dynClk rst
                lts :: Signal dyn (Unsigned 8)
                lts = register dynClk ccRst enableGen minBound
                    $ satSucc SatWrap <$> lts
             in xpmCdcGray dynClk clk lts

  -- collect all plot data
  localData =
    let rfStageChange CallistoResult{..} = case reframingState of
          Detect {} -> ToDetect
          Wait {}   -> ToWait
          Done {}   -> ToDone

        height = SNat @AccWindowHeight
        idcs = unbundle (stability <$> result)

        -- get the points in time where the monitored values change
        stableUpdates  = changepoints clk rst enableGen <$> (fmap stable <$> idcs)
        settledUpdates = changepoints clk rst enableGen <$> (fmap settled <$> idcs)
        modeUpdate     = changepoints clk rst enableGen (rfStageChange <$> result)

        combine eb stU seU ind = (,,)
          <$> eb
          <*> (orNothing <$> stU <*> (stable  <$> ind))
          <*> (orNothing <$> seU <*> (settled <$> ind))

        noChange = fromMaybe NoChange . maybeSpeedChange

     in PlotData
          <$> bundle (zipWith4 combine ebsC stableUpdates settledUpdates idcs)
          <*> accWindow height clk rst enableGen (noChange <$> result)
          <*> mux modeUpdate (rfStageChange <$> result) (pure Stable)

  -- compress the elastic buffer data via only reporting the
  -- differences since the last capture
  (ebDataChange, ebsC) = second unbundle $
    let transF storedDataCounts (trigger, curDataCounts) =
          let diffs = zipWith (-) curDataCounts storedDataCounts
              half = extend @_
                @(CompressedBufferSize - 1)
                @(m - CompressedBufferSize + 1)
                maxBound
              truncDiffs = truncateB @_
                @CompressedBufferSize
                @(m - CompressedBufferSize)
                <$> diffs
           in if trigger || any ((> half) . abs) diffs
              then (curDataCounts,    (True,  truncDiffs))
              else (storedDataCounts, (False, repeat 0))
     in mealyB clk rst enableGen transF (repeat 0) (scheduledCapture, bundle ebs)

  -- produce at least two calibration captures
  calibrating = unsafeToActiveLow rst .&&.
    moore clk rst enableGen
      (\s -> bool s $ satSucc SatBound s)
      (/= maxBound)
      (minBound :: Index 3)
      scheduledCapture

  -- do not forward clock modifications during calibration
  muteDuringCalibration active ccResult = ccResult
    { maybeSpeedChange = bool (maybeSpeedChange ccResult) Nothing active
    }

  -- Note that we always need to capture everything before the trigger
  -- fires, because the data that the ILA captures is undefined
  -- otherwise. Moreover, @syncStart@ does not hold at the trigger,
  -- but only after it. Hence, if the trigger position is at 0, then
  -- we store exactly one capture that is marked with the
  -- @UntilTrigger@ flag this way.
  captureCond :: Signal sys (Maybe CaptureCondition)
  captureCond = mux (not <$> syncStart)
    (pure $ Just UntilTrigger)
    (fmap fst <$> plotData)

  plotData =
    let captureType calibrate scheduled dc dat
          | scheduled && calibrate                = Just (Calibrate,  dat)
          | scheduled                             = Just (Scheduled,  dat)
          | dc || dataChange dat && not calibrate = Just (DataChange, dat)
          | otherwise                             = Nothing

        dataChange PlotData{..} =
             any (\(_, x, y) -> isJust x || isJust y) dEBData
          || dSpeedChange /= NoChange
          || dRfStageChange /= Stable
     in captureType
          <$> calibrating
          <*> scheduledCapture
          <*> ebDataChange
          <*> localData

  ilaInstance :: Signal sys ()
  ilaInstance =
    setName @"ilaPlot" $ ila
      ( ilaConfig
           $ "trigger_1"
          :> "capture_1"
          :> "condition"
          :> "global"
          :> "local"
          :> "data"
          :> Nil
      ) { depth = D16384 }
      -- the ILA must run on a stable clock
      clk
      -- trigger as soon as we start
      syncStart
      -- capture on relevant data changes
      (isJust <$> captureCond)
      -- capture the capture condition
      (fromMaybe UntilTrigger <$> captureCond)
      -- capture the globally synchronized timestamp
      globalTimestamp
      -- capture the local timestamp
      localTs
      -- capture all relevant plot data
      (maybe dummy snd <$> plotData)

  dummy = PlotData
    { dEBData        = repeat (0, Nothing, Nothing)
    , dSpeedChange   = NoChange
    , dRfStageChange = Stable
    }

-- | The state space of the Mealy machine for producing @SYNC_OUT@.
data SyncOutGen dom =
    GettingReady
  | WaitAtLeast (Index (SyncPulseCycles dom))
  | WaitForTransceivers
  | SyncPulse Bool (Index (SyncPulseCycles dom))
  | Failure
  deriving (Generic, NFDataX)

-- | The signal transformer for producing @SYNC_OUT@.
syncOutGenerator ::
  forall dom. HasCallStack =>
  KnownDomain dom =>
  Clock dom ->
  Signal dom Bool ->
  -- ^ The generator starts after this input has turned high.
  Signal dom Bool ->
  -- ^ The transceivers being ready indicator.
  Signal dom Bool
  -- ^ The generated @SYNC_OUT@ signal.
syncOutGenerator clk start inp =
  start .&&.
    mealyB clk (unsafeFromActiveLow start) enableGen
      transF (GettingReady :: SyncOutGen dom) inp
 where
  transF GettingReady        _    = (WaitAtLeast maxBound,       False)
  transF (WaitAtLeast 0)     True = (SyncPulse False maxBound,   False)
  transF (WaitAtLeast 0)     _    = (WaitForTransceivers,        True )
  transF (WaitAtLeast n)     _    = (WaitAtLeast (n - 1),        True )
  transF WaitForTransceivers True = (SyncPulse False maxBound,   False)
  transF WaitForTransceivers _    = (WaitForTransceivers,        True )
  transF (SyncPulse o 0)     True = (SyncPulse (not o) maxBound, not o)
  transF (SyncPulse o n)     True = (SyncPulse o (n - 1),        o    )
  transF _                   _    = (Failure,                    True )

-- | The state space of the Moore machine for recovering @SYNC_OUT@.
data SyncInRec dom =
    WaitForStart
  | WaitForReady
  | WaitForChange Bool
      (Index (Div (SyncPulseCycles dom) 10 + SyncPulseCycles dom))
  deriving (Generic, NFDataX)

-- | Recovers the activity cycle of a test as shared via @SYNC_OUT@.
syncInRecover ::
  forall dom. HasCallStack =>
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  Signal dom Bool ->
  -- ^ The indicator for the test being started via the VIO interface.
  Signal dom Bool ->
  -- ^ The @SYNC_IN@ signal.
  Signal dom (Bool, Bool)
  -- ^ Returns two signals: The first one indicates that the
  -- @SYNC_OUT@ signal generation has been initiated, while the second
  -- one indicates the synchronized start of the test.
syncInRecover clk rst = curry $
  moore clk rst enableGen transF out (WaitForStart :: SyncInRec dom) . bundle
 where
  transF WaitForStart        (True, True) = WaitForReady
  transF WaitForStart        (_   , _   ) = WaitForStart
  transF WaitForReady        (_   , True) = WaitForReady
  transF WaitForReady        (_   , _   ) = WaitForChange False maxBound
  transF (WaitForChange _ 0) (_   , True) = WaitForStart
  transF (WaitForChange o n) (_   , i   )
    | o == i                              = WaitForChange o (n - 1)
    | otherwise                           = WaitForChange i maxBound

  out WaitForStart    = (False, False)
  out WaitForReady    = (True,  False)
  out WaitForChange{} = (True,  True )
