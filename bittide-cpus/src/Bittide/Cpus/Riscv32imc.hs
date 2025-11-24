-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Bittide.Cpus.Riscv32imc (
  vexRiscv0,
  vexRiscv1,
  vexRiscv2,
  vexRiscv3,
) where

import Bittide.Cpus.Types (BittideCpu)

import qualified VexRiscv_Riscv32imc0
import qualified VexRiscv_Riscv32imc1
import qualified VexRiscv_Riscv32imc2
import qualified VexRiscv_Riscv32imc3

vexRiscv0 :: BittideCpu dom
vexRiscv0 = VexRiscv_Riscv32imc0.vexRiscv

vexRiscv1 :: BittideCpu dom
vexRiscv1 = VexRiscv_Riscv32imc1.vexRiscv

vexRiscv2 :: BittideCpu dom
vexRiscv2 = VexRiscv_Riscv32imc2.vexRiscv

vexRiscv3 :: BittideCpu dom
vexRiscv3 = VexRiscv_Riscv32imc3.vexRiscv
