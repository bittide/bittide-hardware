#!/bin/bash
set -exuo pipefail
IFS=$'\n\t'
HERE=$(dirname "$0")
ROOT=$(git rev-parse --show-toplevel)

cd "${HERE}"
./generate_checks.sh

cd "${ROOT}"/riscv-formal/cores/contranomy
make -C checks "$1"
[[ -f checks/"$1"/PASS ]]
