#!/bin/bash
set -exuo pipefail
IFS=$'\n\t'
HERE=$(dirname "$0")
ROOT=$(realpath "$HERE"/..)

cd "${HERE}"
./generate_checks.sh

cd "${ROOT}"

# This is all.. a bit hacky. The alternative would be to fork riscv-formal and
# modify genchecks.py such that it also yield a JSON. I'm not super thrilled
# about that either.
makefile=riscv-formal/cores/contranomy/checks/makefile
checks=$(grep -oP '^all: \K.*' "$makefile")
python3 -c "import json; print(json.dumps('${checks}'.split()))" > checks.json