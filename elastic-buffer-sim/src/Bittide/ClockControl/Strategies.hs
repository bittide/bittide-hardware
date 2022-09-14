-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}

-- | Mock clock controller
module Bittide.ClockControl.Strategies
  ( ClockControlAlgorithm
  , ExpectStable
  , clockControl
  , callistoClockControl
  , stabilityCheck
  )
where

import Clash.Explicit.Prelude
import Clash.Prelude (exposeClockResetEnable)
import Numeric.Natural

import Bittide.ClockControl
import Bittide.ClockControl.Strategies.Callisto

-- | Determines how to influence clock frequency given statistics provided by
-- all elastic buffers.
--
-- This is the canonical controller, and uses 'callisto' under the hood.
callistoClockControl ::
  forall n dom.
  (KnownDomain dom, KnownNat n, 1 <= n) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  -- | Configuration for this component, see individual fields for more info.
  ClockControlConfig ->
  -- | Statistics provided by elastic buffers.
  Vec n (Signal dom DataCount) ->
  Signal dom (SpeedChange, ExpectStable)
callistoClockControl clk rst ena cfg =
  clockControl clk rst ena cfg (exposeClockResetEnable callisto)

type ClockControlAlgorithm dom n a =
  Clock dom ->
  Reset dom ->
  Enable dom ->
  -- | Target data count. See 'targetDataCount'.
  DataCount ->
  -- | Provide an update every /n/ cycles
  Unsigned 32 ->
  -- | Data counts from elastic buffers
  Signal dom (Vec n DataCount) ->
  -- | Speed change requested from clock multiplier
  Signal dom SpeedChange

type Target = DataCount

stabilityCheck ::
  forall dom n.
  (KnownDomain dom, KnownNat n, 1 <= n) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom (Vec n DataCount) ->
  Signal dom ExpectStable
stabilityCheck clk rst ena =
  mealy clk rst ena go (repeat (64, 0))
 where
  go ::
    Vec n (Target, Natural) ->
    Vec n DataCount ->
    (Vec n (Target, Natural), ExpectStable)
  go s i =
    let
      (nextS, stable) = unzip (zipWith g s i)
    in
      (nextS, and stable)

  g ::
    (Target, Natural) ->
    DataCount ->
    ((Target, Natural), ExpectStable)
  g (tgt, count) next
    | next <= tgt+1 && if tgt <= 0 then next >= tgt else next >= tgt-1 =
      ((tgt, count+1), count >= 2000000)
    | otherwise = ((next, 0), False)

clockControl ::
  forall n dom a.
  (KnownDomain dom, KnownNat n, 1 <= n) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  -- | Configuration for this component, see individual fields for more info.
  ClockControlConfig ->
  -- | Clock control strategy
  ClockControlAlgorithm dom n a ->
  -- | Statistics provided by elastic buffers.
  Vec n (Signal dom DataCount) ->
  -- | Whether to adjust node clock frequency
  Signal dom (SpeedChange, ExpectStable)
clockControl clk rst ena ClockControlConfig{..} f dats =
  bundle (change, stabilityCheck clk rst ena ebDats)
 where
  targetCount = targetDataCount cccBufferSize
  updateEveryNCycles = fromIntegral (cccSettlePeriod `div` cccPessimisticPeriod) + 1
  change = f clk rst ena targetCount updateEveryNCycles ebDats
  ebDats = bundle dats
