#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2026 QBayLogic
#
# SPDX-License-Identifier: Apache-2.0

# Prepare everything the Manticore bittide demo needs, into _build/manticore:
#   1. clone the Manticore repositories            (manticore_clone.sh)
#   2. build the bittide chip RTL                  (manticore_build_hdl.sh)  -> _build/manticore/hdl
#   3. generate the per-seam hop latencies         (manticore-latencies exe) -> _build/manticore/latencies.csv
#   4. compile a program for the test              (manticore_compile_program.sh) -> _build/manticore/program
#
# After this, point the shake synthesis flow at the HDL:
#   export MANTICORE_HDL_SRC="$(git rev-parse --show-toplevel)/_build/manticore/hdl"
#   cabal run shake -- manticoreDemoTest:bitstream
#
# All configuration is via the environment variables documented in the
# individual scripts (MANTICORE_HW_REF, MANTICORE_DIMX, MANTICORE_PROGRAM, ...).

set -euo pipefail
IFS=$'\n\t'

SCRIPTDIR="$(cd -- "$(dirname "$0")" >/dev/null 2>&1 && pwd -P)"
TOPDIR="$(git rev-parse --show-toplevel)"

"${SCRIPTDIR}/manticore_clone.sh"
"${SCRIPTDIR}/manticore_build_hdl.sh"

# For the multi-chip torus build, the program must be scheduled with the real
# per-directed-seam latencies. They are derived from the rig's golden UGNs (the
# same ones the WireDemo uses) by the `manticore-latencies` Haskell exe, which
# lives in bittide-instances and runs in this (nix) shell. The single-chip build
# (MANTICORE_TORUS_DIMX = 0) has no seams, so the CSV is skipped and the compile
# step runs without --hop-latencies. An explicitly-set MANTICORE_HOP_LATENCIES
# is honoured as-is (no regeneration).
if [ -z "${MANTICORE_HOP_LATENCIES:-}" ] && [ "${MANTICORE_TORUS_DIMX:-8}" -gt 0 ] 2>/dev/null; then
  LATENCIES_CSV="${TOPDIR}/_build/manticore/latencies.csv"
  mkdir -p "$(dirname "${LATENCIES_CSV}")"
  echo "Generating per-seam hop latencies -> ${LATENCIES_CSV}"
  ( cd "${TOPDIR}" && cabal run -v0 manticore-latencies -- "${LATENCIES_CSV}" )
  export MANTICORE_HOP_LATENCIES="${LATENCIES_CSV}"
fi

"${SCRIPTDIR}/manticore_compile_program.sh"

echo
echo "=== Manticore bittide artifacts ready under ${TOPDIR}/_build/manticore ==="
echo "  HDL:     _build/manticore/hdl      (set MANTICORE_HDL_SRC to this for shake)"
echo "  program: _build/manticore/program  (manifest.json + exec.bin streams)"
