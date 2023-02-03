#!/bin/bash

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

set -exuo pipefail
IFS=$'\n\t'
HERE=$(realpath $(dirname "$0"))
ROOT=$(git rev-parse --show-toplevel)

cd "${HERE}"
./generate_checks.sh

cd "${ROOT}"

# This is all.. a bit hacky. The alternative would be to fork riscv-formal and
# modify genchecks.py such that it also yield a JSON. I'm not super thrilled
# about that either.
makefile=riscv-formal/cores/contranomy/checks/makefile
checks=$(grep -oP '^all: \K.*' "$makefile")
python3 -c "import json; print(json.dumps('${checks}'.split()))" > checks.json
