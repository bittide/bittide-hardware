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
import Clash.Prelude (exposeClockResetEnable)

import Bittide.ClockControl
import Bittide.ClockControl.Strategies.Callisto

-- | Determines how to influence clock frequency given statistics provided by
-- all elastic buffers.
--
-- This is the canonical controller, and uses 'callisto' under the hood.
callistoClockControl ::
  forall n m dom.
  ( KnownDomain dom
  , KnownNat n
  , KnownNat m
  , 1 <= n
  , 1 <= m
  , n + m <= 32
  ) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  -- | Configuration for this component, see individual fields for more info.
  ClockControlConfig m ->
  -- | Statistics provided by elastic buffers.
  Vec n (Signal dom (DataCount m)) ->
  Signal dom SpeedChange
callistoClockControl clk rst ena cfg =
  clockControl clk rst ena cfg (exposeClockResetEnable callisto)

type ClockControlAlgorithm dom n m a =
  Clock dom ->
  Reset dom ->
  Enable dom ->
  -- | Target data count. See 'targetDataCount'.
  DataCount m ->
  -- | Provide an update every /n/ cycles
  Unsigned 32 ->
  -- | Data counts from elastic buffers
  Signal dom (Vec n (DataCount m)) ->
  -- | Speed change requested from clock multiplier
  Signal dom SpeedChange

clockControl ::
  forall n m dom a.
  ( KnownDomain dom
  , KnownNat n
  , KnownNat m
  , 1 <= n
  ) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  -- | Configuration for this component, see individual fields for more info.
  ClockControlConfig m ->
  -- | Clock control strategy
  ClockControlAlgorithm dom n m a ->
  -- | Statistics provided by elastic buffers.
  Vec n (Signal dom (DataCount m)) ->
  -- | Whether to adjust node clock frequency
  Signal dom SpeedChange
clockControl clk rst ena ClockControlConfig{..} f =
  f clk rst ena targetDataCount updateEveryNCycles . bundle
 where
  updateEveryNCycles = fromIntegral (cccSettlePeriod `div` cccPessimisticPeriod) + 1
