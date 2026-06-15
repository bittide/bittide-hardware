#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2026 QBayLogic
#
# SPDX-License-Identifier: Apache-2.0

# Build the Manticore bittide chip RTL (the `-t bittide` target of manticore-hw)
# and place the emitted Verilog in _build/manticore/hdl. That directory is what
# the ManticoreDemo top entity points MANTICORE_HDL_SRC at (externalHdl), so the
# shake synthesis flow can pick up the foreign Verilog.
#
# Requires the Manticore sources (run manticore_clone.sh first) and a JDK 11 +
# sbt. Inside the nix dev shell these come from $MANTICORE_JDK / $MANTICORE_SBT
# (see flake.nix); outside, set those or have a JDK 11 `sbt` on PATH.
#
# Configure via environment variables:
#   MANTICORE_DIMX            (default: 2)
#   MANTICORE_DIMY            (default: 2)
#   MANTICORE_ENABLE_CFU      (default: true)   -- must match the compiled program
#   MANTICORE_NHOP            (default: 1)

set -euo pipefail
IFS=$'\n\t'

TOPDIR="$(git rev-parse --show-toplevel)"
HWDIR="${TOPDIR}/_build/manticore/src/manticore-hw"
OUTDIR="${TOPDIR}/_build/manticore/hdl"

MANTICORE_DIMX="${MANTICORE_DIMX:-2}"
MANTICORE_DIMY="${MANTICORE_DIMY:-2}"
MANTICORE_ENABLE_CFU="${MANTICORE_ENABLE_CFU:-true}"
MANTICORE_NHOP="${MANTICORE_NHOP:-1}"

if [ ! -d "${HWDIR}" ]; then
  echo "ERROR: ${HWDIR} not found; run manticore_clone.sh first" >&2
  exit 1
fi

SBT="${MANTICORE_SBT:-sbt}"
if [ -n "${MANTICORE_JDK:-}" ]; then
  export JAVA_HOME="${MANTICORE_JDK}"
  export PATH="${MANTICORE_JDK}/bin:${PATH}"
fi

echo "Building Manticore bittide HDL (${MANTICORE_DIMX}x${MANTICORE_DIMY}, CFU=${MANTICORE_ENABLE_CFU})"
echo "  java: $(java -version 2>&1 | head -1)"

rm -rf "${OUTDIR}"
mkdir -p "${OUTDIR}"

# -Dmanticore.no_uram=true: the KCU105/bittide rig FPGA (xcku040) has no URAM.
( cd "${HWDIR}" \
  && SBT_OPTS="-Xmx12g -Xss64M -Dmanticore.no_uram=true -Dsbt.watch.mode=polling -Dsbt.server.autostart=false" \
     "${SBT}" -batch \
       "runMain manticore.machine.Main -t bittide -x ${MANTICORE_DIMX} -y ${MANTICORE_DIMY} --enable_custom_alu ${MANTICORE_ENABLE_CFU} --n_hop ${MANTICORE_NHOP} -o ${OUTDIR}" )

if [ ! -f "${OUTDIR}/ManticoreBittideChip.v" ]; then
  echo "ERROR: ManticoreBittideChip.v not emitted in ${OUTDIR}" >&2
  exit 1
fi

echo "Manticore bittide HDL ready in ${OUTDIR}:"
ls -1 "${OUTDIR}"/*.v
