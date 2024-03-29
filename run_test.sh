#!/bin/bash

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

set -euo pipefail
IFS=$'\n\t'

cd contranomy
cabal run -friscv-altopts -- clash --verilog -main-is contranomyRVFITE Contranomy
cd ..

.github/scripts/run_riscv_formal_check.sh $1
