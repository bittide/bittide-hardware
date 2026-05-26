-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Pnr.Dna where

import Clash.Prelude

import Bittide.Instances.Domains (Basic300)
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
dna clk rst ena simDna _ = readDnaPortE2 clk rst ena simDna

readDnaPortE2Fast ::
  Clock Basic300 -> Reset Basic300 -> Signal Basic300 Bit -> Signal Basic300 Bit
readDnaPortE2Fast clk rst = withClock clk $ reducePins $ dna clk rst enableGen simDna2
