-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Pnr.DomainDiffCounter where

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

domainDiffCounterFast :: Clock Basic400 -> Signal Basic400 Bit
domainDiffCounterFast clk =
  withClock clk
    $ reducePins (counter clk noReset clk noReset) (pure 0)
