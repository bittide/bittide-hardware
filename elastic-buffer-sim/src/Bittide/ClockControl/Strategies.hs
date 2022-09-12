-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}

-- | Mock clock controller
module Bittide.ClockControl.Strategies
  ( ClockControlAlgorithm
  , clockControl
  , callistoClockControl
  )
where

import Clash.Explicit.Prelude

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
  Vec n (Signal dom (Maybe DataCount)) ->
  Signal dom SpeedChange
callistoClockControl clk rst ena cfg =
  clockControl clk rst ena cfg callisto

type ClockControlAlgorithm dom n a =
  Clock dom ->
  Reset dom ->
  Enable dom ->
  ClockControlConfig ->
  Signal dom (Vec n (Maybe DataCount)) ->
  Signal dom SpeedChange

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
  Vec n (Signal dom (Maybe DataCount)) ->
  -- | Whether to adjust node clock frequency
  Signal dom SpeedChange
clockControl clk rst ena cfg f =
  f clk rst ena cfg . bundle
