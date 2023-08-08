-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import Clash.Prelude
import Clash.Annotations.TH

import VexRiscv

circuit ::
  "CLK" ::: Clock System ->
  "RST" ::: Reset System ->
  "INPUT" ::: Signal System Input ->
  "OUTPUT" ::: Signal System Output
circuit clk rst input =
  withClockResetEnable clk rst enableGen vexRiscv input

makeTopEntity 'circuit

main :: IO ()
main = pure ()
