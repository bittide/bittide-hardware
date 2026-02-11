-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Bittide.Cpus.Riscv32 (
  bootCpu,
  managementUnitCpu,
  clockControlCpu,
  gppeCpu,
  riscv32Imc0,
  riscv32Imc1,
  riscv32Imc2,
  riscv32Imcf,
) where

import Bittide.Cpus.Types (BittideCpu)

import qualified VexRiscv_Riscv32imc0
import qualified VexRiscv_Riscv32imc1
import qualified VexRiscv_Riscv32imc2
import qualified VexRiscv_Riscv32imcf

-- riscv32Ic :: BittideCpu dom
-- riscv32Ic = VexRiscv_Riscv32ic.vexRiscv

riscv32Imc0 :: BittideCpu dom
riscv32Imc0 = VexRiscv_Riscv32imc0.vexRiscv

riscv32Imc1 :: BittideCpu dom
riscv32Imc1 = VexRiscv_Riscv32imc1.vexRiscv

riscv32Imc2 :: BittideCpu dom
riscv32Imc2 = VexRiscv_Riscv32imc2.vexRiscv

riscv32Imcf :: BittideCpu dom
riscv32Imcf = VexRiscv_Riscv32imcf.vexRiscv

bootCpu :: BittideCpu dom
bootCpu = riscv32Imc0

managementUnitCpu :: BittideCpu dom
managementUnitCpu = riscv32Imc1

clockControlCpu :: BittideCpu dom
clockControlCpu = riscv32Imcf

gppeCpu :: BittideCpu dom
gppeCpu = riscv32Imc2
