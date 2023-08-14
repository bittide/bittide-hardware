-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Bittide.ClockControl.StabilityChecker where

import Clash.Prelude

import Bittide.ClockControl (DataCount, targetDataCount)
import Bittide.ClockControl.Callisto.Util (dataCountToSigned)

-- | Stability results to be returned by the 'stabilityChecker'.
data StabilityIndication =
  StabilityIndication
    { stable :: Bool
      -- ^ Indicates stability of the signal over time.
    , settled :: Bool
      -- ^ Indicates whether the signal is stable and close to
      -- 'targetDataCount'.
    }
  deriving (Generic, NFDataX, BitPack)

-- | Checks whether the @Signal@ of buffer occupancies from an elastic
-- buffer is stable and settled. The @Signal@ is considered to be
-- stable, if it stays within a @margin@ of the target buffer
-- occupancy for @framesize@ number of cycles. If the current buffer
-- occupancies exceed that margin, then the target is updated to the
-- current buffer occupancy. The @Signal@ is considered to be settled,
-- if it is stable and close (within @margin@) to the global target
-- data count.
stabilityChecker ::
  forall dom margin framesize n .
  (HiddenClockResetEnable dom, 1 <= framesize, KnownNat n) =>
  -- | Maximum number of elements the incoming buffer occupancy is
  -- allowed to deviate from the current @target@ for it to be
  -- considered "stable".
  SNat margin ->
  -- | Minimum number of clock cycles the incoming buffer occupancy
  -- must remain within the @margin@ for it to be considered "stable".
  SNat framesize ->
  -- | Incoming buffer occupancy.
  Signal dom (DataCount n) ->
  -- | Stability indicators
  Signal dom StabilityIndication
stabilityChecker SNat SNat = mealy go (0, targetDataCount)
 where
  go (!cnt, !target) input = (newState, StabilityIndication{..})
   where
    withinMargin !x !y =
      abs (dataCountToSigned x `sub` dataCountToSigned y) <= (natToNum @margin)

    newState :: (Index (framesize + 1), DataCount n)
    newState
      | withinMargin target input = (satSucc SatBound cnt, target)
      | otherwise                 = (0, input)

    stable = withinMargin target input && cnt == maxBound
    settled = stable && withinMargin targetDataCount input
