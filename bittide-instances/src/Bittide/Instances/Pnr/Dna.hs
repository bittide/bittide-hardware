-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Pnr.Dna where

import Clash.Explicit.Prelude
import Clash.Prelude (withClock)

import Bittide.Instances.Domains (Basic200)
import Bittide.Instances.Hacks (reducePins)
import Clash.Cores.Xilinx.Unisim.DnaPortE2 (readDnaPortE2, simDna2)

dna ::
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  BitVector 96 ->
  Signal dom () ->
  Signal dom (Maybe (BitVector 96))
dna clk rst ena simDna _ = register clk rst ena Nothing $ readDnaPortE2 clk rst ena simDna

readDnaPortE2Fast ::
  Clock Basic200 -> Reset Basic200 -> Signal Basic200 Bit -> Signal Basic200 Bit
readDnaPortE2Fast clk rst = withClock clk $ reducePins $ dna clk rst enableGen simDna2
