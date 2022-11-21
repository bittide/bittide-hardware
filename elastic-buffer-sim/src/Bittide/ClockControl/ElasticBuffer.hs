-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ClockControl.ElasticBuffer where

import Clash.Prelude
import Clash.Signal.Internal
import GHC.Stack

import Bittide.ClockControl


-- | Determines how 'elasticBuffer' should respond to underflow/overflow.
data OverflowMode
  -- | Saturate at empty/full boundaries. Useful for testing purposes.
  = Saturate
  -- | Error on write-when-full, or read-when-empty. This mode should be used in
  -- practice. Overflowing elastic buffer cannot happen in a real Bittide system.
  | Error

-- | Simple model of a FIFO that only models the interesting part for conversion:
-- data counts.
elasticBuffer ::
  forall n readDom writeDom.
  (HasCallStack, KnownDomain readDom, KnownDomain writeDom, KnownNat n) =>
  -- | What behavior to pick on underflow/overflow
  OverflowMode ->
  Clock readDom ->
  Clock writeDom ->
  Signal readDom (DataCount n)
elasticBuffer mode clkRead clkWrite =
  go (clockTicks clkWrite clkRead) targetDataCount
 where
  go (tick:ticks) !fillLevel =
    case tick of
      ClockA  -> goWrite ticks fillLevel
      ClockB  -> goRead ticks fillLevel
      ClockAB -> go (ClockB:ClockA:ticks) fillLevel
  go [] _ =
    error "elasticBuffer.go: `ticks` should have been an infinite list"

  goWrite ticks fillLevel = go ticks newFillLevel
   where
    newFillLevel
      | fillLevel == maxBound = case mode of
          Saturate -> fillLevel
          Error -> error "elasticBuffer: overflow"
      | otherwise = fillLevel + 1

  goRead ticks fillLevel = newFillLevel :- go ticks newFillLevel
   where
    newFillLevel
      | fillLevel <= 0 = case mode of
          Saturate -> 0
          Error -> error "elasticBuffer: underflow"
      | otherwise = fillLevel - 1
