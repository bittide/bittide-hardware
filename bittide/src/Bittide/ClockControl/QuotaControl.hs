-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}

module Bittide.ClockControl.QuotaControl
  ( CostDelayBits
  , QuotaControlState(..)
  , quotaControl
  , quotaControlTF
  ) where

import Clash.Prelude
import Bittide.ClockControl

-- | The number of bits required for measuring the cost delay.
type CostDelayBits = 64 :: Nat

-- | The internal state of the controller's state machine.
data QuotaControlState =
  QuotaControlState
    { speedChangeRequest :: SpeedChange
    , remainingCost :: Unsigned CostDelayBits
    , incDecCounter :: Signed 17
    }
  deriving (Generic, NFDataX)

-- | Adds a cost function to clock control, which imposes bounds on
-- the maximal number of monotone clock modifications. The cost is
-- applied in terms of a temporal delay, which causes @FINC@ and
-- @FDEC@ requests of the clock controller to take longer to be passed
-- to the clock boards the closer the clock gets adpated towards the
-- physical limits.
quotaControl ::
  HiddenClockResetEnable dom =>
  Signal dom SpeedChange ->
  Signal dom (Maybe SpeedChange)
quotaControl =
  mealy quotaControlTF QuotaControlState
    { speedChangeRequest = NoChange
    , remainingCost = 0
    , incDecCounter = 0
    }

-- | The transition function of the controller's state machine.
quotaControlTF ::
  QuotaControlState ->
  SpeedChange ->
  (QuotaControlState, Maybe SpeedChange)
quotaControlTF (forceX -> qcs@QuotaControlState{..}) !newSpeedChangeRequest =
  -- the following pattern match look a bit unusual, but is the most
  -- effective way to ensure meeting timing constraints
  case (sameSpeedChangeRequest, remainingCost == 0, hasZeroCost) of
    -- new speed change request and non-zero cost
    (False, _, False) ->
      ( qcs { speedChangeRequest = newSpeedChangeRequest
            , remainingCost = satPred SatBound nonZeroCost
            }
      , Nothing
      )
    -- still the same request and non-zero cost
    (True, False, _) ->
      ( qcs { remainingCost = satPred SatBound remainingCost
            }
      , Nothing
      )
    -- zero cost or running cost reached zero
    _ ->
      ( QuotaControlState
          { speedChangeRequest = NoChange
          , remainingCost = 0
          , incDecCounter = case newSpeedChangeRequest of
              SpeedUp  -> satSucc SatBound incDecCounter
              SlowDown -> satPred SatBound incDecCounter
              _        -> incDecCounter
          }
      , Just newSpeedChangeRequest
      )

 where
  sameSpeedChangeRequest =
    newSpeedChangeRequest == speedChangeRequest

  -- clock shifts back to the center and below the first 2^14 changes
  -- are for free
  hasZeroCost =
    opposite newSpeedChangeRequest incDecCounter
      || abs incDecCounter < natToNum @(2^12)
   where
    opposite = \case
      SpeedUp  -> (<= 0)
      SlowDown -> (>= 0)
      _        -> const True

  nonZeroCost
    -- very slowly increase cost for the first 2^14 changes
    | abs incDecCounter < natToNum @(2^14) = 1
    -- linearly increase cost for the next 2^14 changes
    | abs incDecCounter < natToNum @(2^15) =
          satSucc SatBound
        $ bitCoerce @(Index (2^CostDelayBits))
        $ extend $ countLeadingOnes $ pack
        $ checkedTruncateB @15 @_ @Unsigned
        $ bitCoerce $ abs incDecCounter
    -- exponentially increase on any further change
    | otherwise =
          shift 1
        $ fromEnum
        $ checkedTruncateB @CostDelayBits
        $ bitCoerce @_ @(Index (2^17))
        $ min (natToNum @(CostDelayBits - 1))
        $ abs incDecCounter - natToNum @(2^15 - 5)

  -- counts the number of consecutive 1s in a bit vector starting at
  -- the MSB
  countLeadingOnes :: KnownNat n => BitVector (n + 1) -> Index (n + 2)
  countLeadingOnes = fst . fold rAdd . map toIB . bv2v
   where
    rAdd (n, True) _      = (n,     True)
    rAdd (n,    _) (m, y) = (n + m, y   )

    toIB 0 = (0, True)
    toIB _ = (1, False)
