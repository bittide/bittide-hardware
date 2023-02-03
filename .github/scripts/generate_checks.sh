#!/bin/bash

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

set -exuo pipefail
IFS=$'\n\t'
HERE=$(realpath $(dirname "$0"))
ROOT="${HERE}/../.."

cd "${ROOT}"
rm -rf riscv-formal/cores/contranomy
cp -r riscv-formal-config riscv-formal/cores/contranomy
sed -i 's/const rand/rand const/g' riscv-formal/checks/rvfi_macros.*
cd riscv-formal/cores/contranomy
cp "${ROOT}"/contranomy/verilog/Contranomy.contranomyRVFITE/*.inc .
cp "${ROOT}"/contranomy/verilog/Contranomy.contranomyRVFITE/*.v .
python3 "${ROOT}"/riscv-formal/checks/genchecks.py
