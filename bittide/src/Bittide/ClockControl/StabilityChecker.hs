-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GADTs #-}
module Bittide.ClockControl.StabilityChecker where

import Clash.Prelude

import Bittide.ClockControl (targetDataCount)
import Clash.Sized.Extra

-- | Checks whether the @Signal@ of buffer occupancies from an elastic buffer is stable.
-- The @Signal@ is considered stable if it stays within a @margin@ of the target buffer
-- occupancy for @cyclesStable@ number of cycles. The next target is set to the current
-- buffer occupancy when the current buffer occupancy is not within margin of
-- the target.
stabilityChecker ::
  forall dom margin cyclesStable n .
  (HiddenClockResetEnable dom, 1 <= cyclesStable, KnownNat n) =>
  -- | Maximum number of elements the incoming buffer occupancy is allowed to deviate
  -- from the current @target@ for it to be considered "stable".
  SNat margin ->
  -- | Minimum number of clock cycles the incoming buffer occupancy must remain within the
  -- @margin@ for it to be considered "stable".
  SNat cyclesStable ->
  -- | Incoming buffer occupancy.
  Signal dom (Unsigned n) ->
  -- | Stability indicator.
  Signal dom Bool
stabilityChecker SNat SNat = mealy go (0, targetDataCount)
 where
  go (cnt, target) input = (newState, isStable)
   where
    withinMargin =
      abs (unsignedToSigned target `sub` unsignedToSigned input) <= (natToNum @margin)

    newState :: (Index (cyclesStable + 1), Unsigned n)
    newState
      | withinMargin = (satSucc SatBound cnt, target)
      | otherwise    = (0, input)

    isStable = withinMargin && cnt == maxBound
