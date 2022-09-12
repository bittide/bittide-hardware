-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}

-- | Mock clock controller
module Bittide.ClockControl.Strategies
  ( ClockControlAlgorithm
  , clockControl
  , runClockControl
  )
where

import Clash.Explicit.Prelude

import Bittide.ClockControl
import Bittide.ClockControl.Strategies.Callisto

-- | Determines how to influence clock frequency given statistics provided by
-- all elastic buffers.
--
-- This is the canonical controller, and uses 'callisto' under the hood.
clockControl ::
  forall n dom.
  (KnownNat n, 1 <= n) =>
  -- | Configuration for this component, see individual fields for more info.
  ClockControlConfig ->
  -- | Statistics provided by elastic buffers.
  Vec n (Signal dom DataCount) ->
  Signal dom SpeedChange
clockControl cfg = runClockControl cfg callisto

type ClockControlAlgorithm dom n a =
  ClockControlConfig ->
  SettlePeriod ->
  Signal dom (Vec n DataCount) ->
  Signal dom SpeedChange

runClockControl ::
  forall n dom a.
  (KnownNat n, 1 <= n) =>
  -- | Configuration for this component, see individual fields for more info.
  ClockControlConfig ->
  -- | Clock control strategy
  ClockControlAlgorithm dom n a ->
  -- | Statistics provided by elastic buffers.
  Vec n (Signal dom DataCount) ->
  -- | Whether to adjust node clock frequency
  Signal dom SpeedChange
runClockControl cfg@ClockControlConfig{..} f =
  f cfg (cccSettlePeriod + 1) . bundle
