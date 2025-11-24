-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: CC0-1.0
import Distribution.Simple
import VexRiscv.Setup (VexRiscvSource (VexRiscvBundled), addVexRiscvHooks)

main :: IO ()
main =
  defaultMainWithHooks
    ( addVexRiscvHooks
        simpleUserHooks
        "data"
        [ "Riscv32imc0"
        , "Riscv32imc1"
        , "Riscv32imc2"
        , "Riscv32imc3"
        ]
        VexRiscvBundled
    )
