#!/bin/bash
set -exuo pipefail
IFS=$'\n\t'
HERE=$(dirname "$0")
ROOT=$(realpath "$HERE"/..)

cd "${ROOT}"
cp -r riscv-formal-config riscv-formal/cores/contranomy
sed -i 's/const rand/rand const/g' riscv-formal/checks/rvfi_macros.*
cd riscv-formal/cores/contranomy
cp "${ROOT}"/contranomy/verilog/Contranomy.contranomyRVFITE/*.inc .
cp "${ROOT}"/contranomy/verilog/Contranomy.contranomyRVFITE/*.v .
python3 "${ROOT}"/riscv-formal/checks/genchecks.py