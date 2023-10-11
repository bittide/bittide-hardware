-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import Clash.Prelude
import Clash.Annotations.TH

import VexRiscv

circuit ::
  "CLK" ::: Clock System ->
  "RST" ::: Reset System ->
  "TCK" ::: Clock System ->
  "INPUT" ::: Signal System Input ->
  "JTAG_IN_" ::: Signal System JtagIn ->
  "" :::
    ( "OUTPUT" ::: Signal System Output
    , "JTAG_OUT_" ::: Signal System JtagOut)
circuit clk rst tck input jtagIn =
  vexRiscv clk rst tck enableGen input jtagIn

makeTopEntity 'circuit

main :: IO ()
main = pure ()
