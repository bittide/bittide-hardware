-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Clash.Cores.Xilinx.Extra (
  readDnaPortE2I,
  module GTH,
) where

import Clash.Prelude

import Clash.Cores.Xilinx.GTH as GTH
import Clash.Cores.Xilinx.Unisim.DnaPortE2 (readDnaPortE2)

-- | Like 'dnaPortE2', but with a hidden clock, reset, and enable
readDnaPortE2I ::
  (HiddenClockResetEnable dom) =>
  -- | DNA value to use in simulation
  BitVector 96 ->
  -- | Extracted DNA value from FPGA. Will take ~100 cycles to become available.
  Signal dom (Maybe (BitVector 96))
readDnaPortE2I = hideClockResetEnable readDnaPortE2
