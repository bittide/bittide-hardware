-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Counter where

import Clash.Explicit.Prelude
import Clash.Prelude (withClock)

import Bittide.Counter
import Bittide.Instances.Domains
import Bittide.Instances.Hacks

counter ::
  Clock Basic200 -> Reset Basic200 -> Enable Basic200 ->
  Clock Basic200 -> Reset Basic200 -> Enable Basic200 ->
  Signal Basic200 () ->
  Signal Basic200 (Signed 32, Bool)
counter clk0 rst0 enb0 clk1 rst1 enb1 _ =
  domainDiffCounter clk0 rst0 enb0 clk1 rst1 enb1

counterReducedPins ::
  Clock Basic200 -> Reset Basic200 -> Enable Basic200 ->
  Clock Basic200 -> Reset Basic200 -> Enable Basic200 ->
  Signal Basic200 Bit
counterReducedPins clk0 rst0 enb0 clk1 rst1 enb1 =
  withClock clk1 $
    reducePins (counter clk0 rst0 enb0 clk1 rst1 enb1) (pure 0)
