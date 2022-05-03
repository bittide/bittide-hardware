#!/bin/bash

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

set -exuo pipefail
IFS=$'\n\t'
HERE=$(dirname "$0")
ROOT=$(git rev-parse --show-toplevel)

cd "${HERE}"
./generate_checks.sh

cd "${ROOT}"/riscv-formal/cores/contranomy
rm -rf checks/"$1"
make -C checks "$1"
[[ -f checks/"$1"/PASS ]]
