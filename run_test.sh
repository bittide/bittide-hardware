#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

cd contranomy
cabal run -friscv-altopts -- clash --verilog -main-is contranomyRVFITE Contranomy
cd ..

.github/scripts/run_riscv_formal_check.sh $1
