-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Pnr.ClockControl where

import Clash.Prelude

import Bittide.ClockControl
import Bittide.ClockControl.Callisto
import Bittide.Instances.Domains

config :: ClockControlConfig Basic200 12 8 1500000
config = $(lift (defClockConfig @Basic200))

callisto3 ::
  Clock Basic200 ->
  Reset Basic200 ->
  Enable Basic200 ->
  -- | Data counts from elastic buffers
  Vec 3 (Signal Basic200 (RelDataCount 12)) ->
  -- | Speed change requested from clock multiplier
  Signal Basic200 (CallistoResult 3)
callisto3 clk rst ena dataCounts =
  callistoClockControl clk rst ena config availableLinkMask dataCounts
 where
  -- all links available
  availableLinkMask = pure $ complement 0
