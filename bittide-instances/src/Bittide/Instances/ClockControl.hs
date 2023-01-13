-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.ClockControl where

import Clash.Prelude

import Bittide.ClockControl.Callisto
import Bittide.Instances.Domains
import Bittide.ClockControl

config :: ClockControlConfig Basic200 12
config = $(lift (defClockConfig @Basic200))

callisto3 ::
  Clock Basic200 ->
  Reset Basic200 ->
  Enable Basic200 ->
  -- | Data counts from elastic buffers
  Vec 3 (Signal Basic200 (DataCount 12)) ->
  -- | Speed change requested from clock multiplier
  Signal Basic200 SpeedChange
callisto3 clk rst ena dataCounts =
  callistoClockControl clk rst ena config availableLinkMask dataCounts
 where
  -- all links available
  availableLinkMask = pure $ pack $ repeat high
