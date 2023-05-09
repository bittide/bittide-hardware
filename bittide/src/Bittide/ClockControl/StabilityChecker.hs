-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GADTs #-}
module Bittide.ClockControl.StabilityChecker where

import Clash.Prelude

import Bittide.ClockControl (DataCount, targetDataCount)
import Bittide.ClockControl.Callisto.Util (dataCountToSigned)

-- | Checks whether the @Signal@ of buffer occupancies from an elastic
-- buffer is stable and close to the target data counter. The @Signal@
-- is considered stable if it stays within a @margin@ of the target
-- buffer occupancy for @framesize@ number of cycles. If the
-- current buffer occupancies exceed that margin, then the target is
-- updated to the current buffer occupancy. The @Signal@ is considered
-- to be close to the target data counter, if it is stable and close
-- (within the @margin@) to the global target data count.
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
  -- | Stability indicators. The first tuple element indicates
  -- stability of the signal over time, while the second element
  -- indicates whether the signal is close to 'targetDataCount'.
  Signal dom (Bool, Bool)
stabilityChecker SNat SNat = mealy go (0, targetDataCount)
 where
  go (!cnt, !target) input = (newState, (isStable, isCloseToTarget))
   where
    withinMargin !x !y =
      abs (dataCountToSigned x `sub` dataCountToSigned y) <= (natToNum @margin)

    newState :: (Index (framesize + 1), DataCount n)
    newState
      | withinMargin target input = (satSucc SatBound cnt, target)
      | otherwise                 = (0, input)

    isStable = withinMargin target input && cnt == maxBound
    isCloseToTarget = isStable && withinMargin targetDataCount input
