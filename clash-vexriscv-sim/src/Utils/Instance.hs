-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

-- | A dummy instance to test whether Clash can generate HDL for the VexRiscv
module Utils.Instance where

import Clash.Annotations.TH
import Clash.Prelude
import VexRiscv

circuit ::
  "CLK" ::: Clock System ->
  "RST" ::: Reset System ->
  "CPU_IN" ::: Signal System CpuIn ->
  "JTAG_IN" ::: Signal System JtagIn ->
  ""
    ::: ( "CPU_OUTPUT" ::: Signal System CpuOut
        , "JTAG_OUT" ::: Signal System JtagOut
        )
circuit clk rst input jtagIn =
  vexRiscv NoDumpVcd clk rst input jtagIn

{-# CLASH_OPAQUE circuit #-}
makeTopEntity 'circuit
