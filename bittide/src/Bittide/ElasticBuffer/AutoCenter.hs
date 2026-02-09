-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ElasticBuffer.AutoCenter where

import Clash.Prelude
import Protocols

import Bittide.ClockControl (RelDataCount)

data State
  = InReset
  | Idle
      { adjustments :: Signed 32
      -- ^ Cumulative adjustments that have been submitted to the elastic buffer
      }
  | Adjusting
      { adjustments :: Signed 32
      -- ^ Cumulative adjustments that have been submitted to the elastic buffer
      , adjustment :: Signed 32
      -- ^ The adjustment that is currently being submitted to the elastic buffer
      }
  | Waiting
      { adjustments :: Signed 32
      -- ^ Cumulative adjustments that have been submitted to the elastic buffer
      , wait :: Index 256
      -- ^ Number of cycles to wait before moving to Idle state. This should be long enough
      -- to ensure that the adjustment has taken an effect on the buffer's occupancy count.
      -- It is currently hardcoded to 256 cycles, which should be well above the time it
      -- takes for an adjustment to take effect. At the same time, it should be more than
      -- quick enough to not be too slow even for systems that are wildly out of balance.
      }
  deriving (Generic, NFDataX, Show)

{- | If 'True', there are no pending adjustments to the elastic buffer. This means that the
accumulated adjustments have been applied (though not necessarily reflected in the occupancy
count yet) and that they can be used for, e.g., UGN adjustments.
-}
type IsIdle = Bool

{- | Whether the auto-centering state machine is enabled. If 'False', the state machine will
stay in the Idle state and will not submit any adjustments to the elastic buffer. Disabling
the state machine while it is active is perfectly safe -- the state machine will handle
its pending submissions and then move and stay in its 'Idle' state.
-}
type IsEnabled = Bool

{- | State machine that tries to keep the buffer centered around 0 by submitting adjustments
to the elastic buffer. Note that adjustments to the elastic buffer are destructive, and
should therefore only be used during bittide initialization.
-}
autoCenter ::
  (HiddenClock dom, KnownNat n, n <= 32) =>
  Reset dom ->
  Enable dom ->
  -- | Correction margin. If set to 1, the circuit will try to keep the buffer at
  --   [-1, 0, 1]. Note that if set to 0, the circuit will try to keep the buffer at exactly
  --   0, which will cause it to submit many small adjustments, which may not be desirable.
  Signal dom (Unsigned 16) ->
  -- | The current number of items in the buffer. The circuit will try to keep this at 0
  --   (or within the margin if set).
  Signal dom (RelDataCount n) ->
  Circuit
    ()
    ( Df dom (Signed 32)
    , CSignal dom (Signed 32)
    , CSignal dom IsIdle
    )
autoCenter reset enable margin relDataCount = Circuit go
 where
  go (_, (ack, _, _)) = ((), (adjustment, output, isIdle))
   where
    (adjustment, output, isIdle) =
      withEnable enableGen
        $ withReset reset
        $ mooreB
          goState
          goOutput
          InReset
          ( fromEnable enable
          , numConvert <$> relDataCount
          , numConvert <$> margin
          , ack
          )

-- XXX: These functions are outside of the circuit definition to avoid shadowing :-/

goState :: State -> (IsEnabled, RelDataCount 32, Signed 32, Ack) -> State
goState InReset _ = Idle 0
goState s@Idle{} (False, _relDataCount, _margin, _ack) = s
goState s@Idle{adjustments} (_isEnabled, relDataCount, margin, _ack)
  | negate margin <= relDataCount && relDataCount <= margin = s -- within margin
  | otherwise = Adjusting adjustments (negate relDataCount) -- outside margin
goState s@Adjusting{adjustments, adjustment} (_isEnabled, _relDataCount, _margin, ~(Ack ack))
  | ack = Waiting{adjustments = adjustments + adjustment, wait = 0}
  | otherwise = s
goState Waiting{adjustments, wait} (_isEnabled, _relDataCount, _margin, _ack)
  | wait == maxBound = Idle adjustments
  | otherwise = Waiting{adjustments, wait = wait + 1}

goOutput :: State -> (Maybe (Signed 32), Signed 32, IsIdle)
goOutput s = (adjustment_, adjustments_, isIdle)
 where
  -- Note! The "idle" output reflects the state of submissions, not the state of the
  -- state machine. I.e., idle means that there are no pending adjustments to the elastic
  -- buffer.
  isIdle = case s of
    InReset -> False
    Idle{} -> True
    Adjusting{} -> False
    Waiting{} -> True

  adjustments_ = case s of
    InReset -> 0
    Idle{adjustments} -> adjustments
    Adjusting{adjustments} -> adjustments
    Waiting{adjustments} -> adjustments

  adjustment_ = case s of
    InReset -> Nothing
    Idle{} -> Nothing
    Adjusting{adjustment} -> Just adjustment
    Waiting{} -> Nothing
