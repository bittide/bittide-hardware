-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Mock clock controller
module Bittide.ClockControl.Strategies
  ( Active
  , ClockControlAlgorithm
  , clockControl
  , callistoClockControl
  , stabilityCheck
  )
where

import Clash.Explicit.Prelude
import Data.Maybe (fromMaybe)

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
  clockControl clk rst ena cfg callisto

type Active = Bool

type ClockControlAlgorithm dom n a =
  Clock dom ->
  Reset dom ->
  Enable dom ->
  ClockControlConfig ->
  Signal dom (Vec n DataCount) ->
  Signal dom (Maybe SpeedChange)

type ExpectStable = Bool

stabilityCheck ::
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom (Maybe SpeedChange) ->
  Signal dom ExpectStable
stabilityCheck _clk _rst _ena _ = pure False

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
clockControl clk rst ena cfg f dats =
  (,) <$> converted <*> isStable
 where
  mSpeedChanges = f clk rst ena cfg (bundle dats)
  converted = fromMaybe NoChange <$> mSpeedChanges
  isStable = stabilityCheck clk rst ena mSpeedChanges
