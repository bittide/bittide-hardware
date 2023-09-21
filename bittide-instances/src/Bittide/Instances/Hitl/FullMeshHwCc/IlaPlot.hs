-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
module Bittide.Instances.Hitl.FullMeshHwCc.IlaPlot
  ( PlotData(..)
  , IlaControl(..)
  , RfStageChange(..)
  , CaptureCondition(..)
  , GlobalTimestamp
  , LocalTimestamp
  , AccWindowHeight
  , SyncPulsePeriod
  , syncOutGenerator
  , syncInRecover
  , callistoClockControlWithIla
  ) where

import Clash.Explicit.Prelude
import Clash.Explicit.Signal.Extra

import Bittide.Arithmetic.Time (Milliseconds, PeriodToCycles)
import Bittide.ClockControl (SpeedChange(..), DataCount, ClockControlConfig)
import Bittide.ClockControl.Callisto
  (CallistoResult(..), ReframingState(..), callistoClockControl)
import Bittide.ClockControl.StabilityChecker
import Bittide.Extra.Maybe (orNothing)

import Clash.Cores.Xilinx.Xpm.Cdc.Handshake.Extra (xpmCdcMaybeLossy)
import Clash.Cores.Xilinx.Xpm.Cdc.Gray (xpmCdcGray)
import Clash.Cores.Xilinx.Ila (IlaConfig(..), Depth(..), ila, ilaConfig)

import Data.Maybe (isJust, fromMaybe)

type AccWindowHeight = 3 :: Nat
type SyncPulsePeriod = Milliseconds 5
type SyncPulseCycles dom = PeriodToCycles dom SyncPulsePeriod

-- | A global timestamp consists of the number of synchronized pulses
-- received and the number of cycles of the local stable clock
-- (starting with 'syncStart').
type GlobalTimestamp = (Unsigned 32, Unsigned 32)

-- | The local timestamp counts the cycles of the dynamic clock
-- (starting with clock control).
type LocalTimestamp = Unsigned 32

-- | Indicator for signaling a change in the stage of 'ReframingState'.
data RfStageChange = Stable | ToDetect | ToWait | ToDone
  deriving (Eq, Generic, BitPack, NFDataX)

-- | Indicator for stating the reason of an ILA capture.
data CaptureCondition = UntilTrigger | CalibrationDone | DataChange | TheEnd
  deriving (Eq, Generic, NFDataX, BitPack)

-- | Conglomerate of signals, as they are required by the ILA trigger
-- and capture conditions.
data IlaControl dom =
  IlaControl
    { syncStart :: Signal dom Bool
      -- ^ Synchronized test start trigger
    , syncEnd :: Signal dom Bool
      -- ^ Synchronized test end trigger
    , calibrate :: Signal dom Bool
      -- ^ Calibration interval
    , calibrationDone :: Signal dom Bool
      -- ^ Calibration end trigger
    , globalTimestamp :: Signal dom GlobalTimestamp
      -- ^ Synchronized pulse counter
    }

-- | A single data type for covering all of the non-clock related data
-- to be included into a capture.
data PlotData (n :: Nat) (m :: Nat) =
  PlotData
    { dEBData       :: Vec n (DataCount m, Maybe Bool, Maybe Bool)
    , dSpeedChange  :: SpeedChange
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
accwindow ::
  forall height dom.
  (KnownNat height, KnownDomain dom) =>
  SNat height ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom SpeedChange ->
  Signal dom SpeedChange
accwindow _ clk rst ena =
  flip (mealy clk rst ena) (True, minBound :: Index height)
    $ \(d,s) -> \case
         NoChange -> ((d, s), NoChange)
         x        -> let d' = if x == SpeedUp then d else not d in if
           |     d' && s == maxBound -> ((not d, minBound), x       )
           | not d' && s == minBound -> ((not d, minBound), NoChange)
           |     d'                  -> ((d,     s + 1),    NoChange)
           | otherwise               -> ((d,     s - 1),    NoChange)


-- Wrapper on 'Bittide.ClockControl.Callisto.callistoClockControl'
-- additionally dumping all the data that is required for producing
-- plots of the clock control behavior.
{-# NOINLINE callistoClockControlWithIla #-}
callistoClockControlWithIla ::
  forall n m dom domIla margin framesize.
  ( KnownDomain dom
  , KnownDomain domIla
  , KnownNat n
  , KnownNat m
  , KnownNat margin
  , KnownNat framesize
  , 1 <= n
  , 1 <= m
  , n + m <= 32
  , 1 <= framesize
  , 4 + n * (m + 4) <= 1024
  ) =>
  Clock domIla ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  -- | Configuration for this component, see individual fields for more info.
  ClockControlConfig dom m margin framesize ->
  -- | Ila trigger and capture conditions
  IlaControl domIla ->
  -- | Link availability mask
  Signal dom (BitVector n) ->
  -- | Statistics provided by elastic buffers.
  Vec n (Signal dom (DataCount m)) ->
  Signal dom (CallistoResult n)
callistoClockControlWithIla sysClk clk rst ena ccc IlaControl{..} mask ebs =
  hwSeqX ilaInstance result
 where
  result = callistoClockControl clk rst ena ccc mask ebs

  -- local timestamp on the stable clock
  localTs :: Signal domIla LocalTimestamp
  localTs =
    let lts :: Signal dom LocalTimestamp
        lts = register clk rst ena 0 ((+1) <$> lts)
     in xpmCdcGray clk sysClk lts

  rfStageChange CallistoResult{..} = case reframingState of
    Detect {} -> ToDetect
    Wait {}   -> ToWait
    Done {}   -> ToDone

  localData =
    let
      height = SNat :: SNat AccWindowHeight
      indicators = unbundle (stability <$> result)

      -- get the points in time where the monitored values change
      stableUpdates = changepoints clk rst ena <$> (fmap stable <$> indicators)
      settledUpdates = changepoints clk rst ena <$> (fmap settled <$> indicators)
      modeUpdate = changepoints clk rst ena (rfStageChange <$> result)

      combine eb stU seU ind = (,,)
        <$> eb
        <*> (orNothing <$> stU <*> (stable  <$> ind))
        <*> (orNothing <$> seU <*> (settled <$> ind))
    in PlotData
      <$> bundle (zipWith4 combine ebs stableUpdates settledUpdates indicators)
      <*> accwindow height clk rst ena (fromMaybe NoChange . maybeSpeedChange <$> result)
      <*> mux modeUpdate (rfStageChange <$> result) (pure Stable)

  plotData = mux calibrate (pure Nothing)
    $ xpmCdcMaybeLossy clk sysClk
      ((\x -> orNothing (dataChange x) x) <$> localData)

  dataChange PlotData{..} =
       any (\(_, x, y) -> isJust x || isJust y) dEBData
    || dSpeedChange /= NoChange
    || dRfStageChange /= Stable

  -- Note that we always need to capture everything before the trigger
  -- fires, because the data that ILA captures is undefined
  -- otherwise. Moreover, @syncStart@ does not hold at the trigger,
  -- but only after it. Hence, if the trigger position is at 0, then
  -- we store exactly one capture that is marked with the
  -- @UntilTrigger@ flag this way.
  captureCond :: Signal domIla (Maybe CaptureCondition)
  captureCond =
      mux (not <$> syncStart)   (pure $ Just UntilTrigger)
    $ mux calibrationDone       (pure $ Just CalibrationDone)
    $ mux syncEnd               (pure $ Just TheEnd)
    $ mux (isJust <$> plotData) (pure $ Just DataChange)
    $ pure Nothing

  ilaInstance :: Signal domIla ()
  ilaInstance =
    ila
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
      sysClk
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
      (fromMaybe dummy <$> plotData)

  dummy = PlotData
    { dEBData        = repeat (0, Nothing, Nothing)
    , dSpeedChange   = NoChange
    , dRfStageChange = Stable
    }

-- | The state space of the Mealy machine for producing @SYNC_OUT@.
data SyncOutGen dom =
    WaitAtLeast (Index (SyncPulseCycles dom))
  | WaitForTransceivers
  | SyncPulse Bool (Index (SyncPulseCycles dom))
  | Failure
  deriving (Generic, NFDataX)

-- | The signal transformer for producing @SYNC_OUT@.
syncOutGenerator ::
  forall dom.
  KnownDomain dom =>
  Clock dom ->
  Signal dom Bool ->
  Signal dom Bool ->
  Signal dom Bool
syncOutGenerator clk start inp =
  start .&&.
    mealyB clk (unsafeFromActiveLow start) enableGen
      transF (WaitAtLeast maxBound :: SyncOutGen dom) inp
 where
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
  | WaitForChange Bool (Index (Div (SyncPulseCycles dom) 10 + SyncPulseCycles dom))
  | FailureDetect
  deriving (Generic, NFDataX)

-- | Recovers the activity cycle of a test as shared via @SYNC_OUT@.
syncInRecover ::
  forall dom.
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  Signal dom Bool ->
  Signal dom (Bool, Bool)
syncInRecover clk rst =
  moore clk rst enableGen transF out (WaitForStart :: SyncInRec dom)
 where
  transF WaitForStart            True = WaitForReady
  transF WaitForReady            True = WaitForReady
  transF WaitForReady            _    = WaitForChange False maxBound
  transF (WaitForChange True 0)  True = FailureDetect
  transF (WaitForChange o n) i
    | o == i                          = WaitForChange o (n - 1)
    | otherwise                       = WaitForChange i maxBound
  transF FailureDetect           True = FailureDetect
  transF _                       _    = WaitForStart

  out WaitForStart    = (False, False)
  out WaitForReady    = (True,  False)
  out WaitForChange{} = (True,  True )
  out FailureDetect   = (True,  False)
