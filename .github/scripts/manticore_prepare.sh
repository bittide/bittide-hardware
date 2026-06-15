#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2026 QBayLogic
#
# SPDX-License-Identifier: Apache-2.0

# Prepare everything the Manticore bittide demo needs, into _build/manticore:
#   1. clone the Manticore repositories            (manticore_clone.sh)
#   2. build the bittide chip RTL                  (manticore_build_hdl.sh)  -> _build/manticore/hdl
#   3. compile a program for the test              (manticore_compile_program.sh) -> _build/manticore/program
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

"${SCRIPTDIR}/manticore_clone.sh"
"${SCRIPTDIR}/manticore_build_hdl.sh"
"${SCRIPTDIR}/manticore_compile_program.sh"

TOPDIR="$(git rev-parse --show-toplevel)"
echo
echo "=== Manticore bittide artifacts ready under ${TOPDIR}/_build/manticore ==="
echo "  HDL:     _build/manticore/hdl      (set MANTICORE_HDL_SRC to this for shake)"
echo "  program: _build/manticore/program  (manifest.json + exec.bin streams)"
