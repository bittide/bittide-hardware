-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Pnr.Counter where

import Clash.Explicit.Prelude
import Clash.Prelude (withClock)

import Bittide.Counter
import Bittide.Instances.Domains
import Bittide.Instances.Hacks
import Clash.Cores.Xilinx (withXilinx)

counter ::
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  Clock dom ->
  Reset dom ->
  Signal dom () ->
  Signal dom (Signed 32, Bool)
counter clk0 rst0 clk1 rst1 _ = withXilinx $ domainDiffCounter clk0 rst0 clk1 rst1

counterReducedPins :: Clock Basic200 -> Signal Basic200 Bit
counterReducedPins clk =
  withClock clk
    $ reducePins (counter clk noReset clk noReset) (pure 0)

counterFast :: Clock Basic400 -> Signal Basic400 Bit
counterFast clk =
  withClock clk
    $ reducePins (counter clk noReset clk noReset) (pure 0)
