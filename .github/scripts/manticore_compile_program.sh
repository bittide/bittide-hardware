#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2026 QBayLogic
#
# SPDX-License-Identifier: Apache-2.0

# Compile a Manticore program for the bittide demo test and place the result in
# _build/manticore/program. The output is a manticore-runtime layout
# (manifest.json + per-binary exec.bin streams) that the demo's host driver
# loads into the chip's global memory over the DMI window.
#
# Uses the manticore-compiler (masm). The default program is the MIPS32
# benchmark (the same design as the KCU105 golden), compiled from Verilog via
# masm's frontend (v2masmyosys, the yosys fork submodule). The frontend is
# built here if not already present, using the external ABC from the flake
# (ABCEXTERNAL) so the build never has to fetch/compile ABC.
#
# A pre-lowered single .masm file may be given as MANTICORE_PROGRAM instead, in
# which case masm reads it directly and the frontend is not built.
#
# IMPORTANT: the program's custom-function setting must match the HDL build.
# The default HDL build enables the CFU (--enable_custom_alu true), so we
# compile WITH custom functions (masm's default; pass MANTICORE_NO_CF=1 to
# disable, matching an --enable_custom_alu false HDL build).
#
# Requires the Manticore sources (run manticore_clone.sh first), JDK 11 + sbt
# ($MANTICORE_JDK / $MANTICORE_SBT), and — for Verilog input — the frontend
# build deps + external ABC ($MANTICORE_ABC or `abc` on PATH); all from the nix
# dev shell (see flake.nix).
#
# Configure via environment variables:
#   MANTICORE_DIMX            (default: 2)        -- must match the HDL build
#   MANTICORE_DIMY            (default: 2)
#   MANTICORE_PROGRAM         (default: benchmarks/MIPS32/main.sv benchmarks/MIPS32/mips32.sv)
#   MANTICORE_NO_CF           (default: unset)    -- set to 1 for a no-CFU build
#   MANTICORE_ABC             (default: `abc` on PATH) -- external ABC for the frontend

set -euo pipefail
IFS=$'\n\t'

TOPDIR="$(git rev-parse --show-toplevel)"
COMPILERDIR="${TOPDIR}/_build/manticore/src/manticore-compiler"
OUTDIR="${TOPDIR}/_build/manticore/program"

MANTICORE_DIMX="${MANTICORE_DIMX:-4}"
MANTICORE_DIMY="${MANTICORE_DIMY:-4}"
# 8x16 2D-torus demo: the program is compiled for the GLOBAL torus
# (TORUS_DIMX x TORUS_DIMY) and split into one image per 4x4 chip
# (--chip-dim-x/y = DIMX/DIMY), with the distributed stall wave. TORUS = 0 keeps
# the legacy single-chip build (global == chip dims, no split, no stall wave).
MANTICORE_TORUS_DIMX="${MANTICORE_TORUS_DIMX:-8}"
MANTICORE_TORUS_DIMY="${MANTICORE_TORUS_DIMY:-16}"
MANTICORE_STALL_WAVE="${MANTICORE_STALL_WAVE:-true}"
MANTICORE_STALL_MARGIN="${MANTICORE_STALL_MARGIN:-4}"
# TDM serialization period of the inter-chip seam links: each physical seam wire
# carries nLinks logical links, one packet per logical link per period, so the
# scheduler must keep a full period free around every crossing. Without this the
# schedule overdrives the seam mux (packet loss on the rig; the RTL sim shows the
# same as dbg_seam_overflow). 8 = 2*chipDim slots, verified overflow-free in the
# 8-chip MultiChipPerMgmtSim. Only meaningful for the multi-chip torus build.
MANTICORE_TDM_PERIOD="${MANTICORE_TDM_PERIOD:-8}"
MANTICORE_PROGRAM="${MANTICORE_PROGRAM:-benchmarks/picorv32/picorv32.v benchmarks/picorv32/loop_multi.v}"

# global torus dims for masm -x/-y (= chip dims for the single-chip build)
if [ "${MANTICORE_TORUS_DIMX}" -gt 0 ] 2>/dev/null; then
  MASM_X="${MANTICORE_TORUS_DIMX}"
  MASM_Y="${MANTICORE_TORUS_DIMY}"
  torus_args=(--chip-dim-x "${MANTICORE_DIMX}" --chip-dim-y "${MANTICORE_DIMY}")
  if [ "${MANTICORE_STALL_WAVE}" = "true" ]; then
    torus_args+=(--stall-wave --stall-margin "${MANTICORE_STALL_MARGIN}")
  fi
  if [ "${MANTICORE_TDM_PERIOD}" -gt 1 ] 2>/dev/null; then
    torus_args+=(--tdm-period "${MANTICORE_TDM_PERIOD}")
  fi
else
  MASM_X="${MANTICORE_DIMX}"
  MASM_Y="${MANTICORE_DIMY}"
  torus_args=()
fi

if [ ! -d "${COMPILERDIR}" ]; then
  echo "ERROR: ${COMPILERDIR} not found; run manticore_clone.sh first" >&2
  exit 1
fi

SBT="${MANTICORE_SBT:-sbt}"
if [ -n "${MANTICORE_JDK:-}" ]; then
  export JAVA_HOME="${MANTICORE_JDK}"
  export PATH="${MANTICORE_JDK}/bin:${PATH}"
fi

cf_args=()
if [ "${MANTICORE_NO_CF:-}" = "1" ]; then
  cf_args+=(--no-cf)
fi

# Per-directed-link seam latencies for the compiler's NoC schedule. For a
# single chip (the current demo) there are no inter-chip seams, so this is
# empty/unset. For a multi-chip torus across the rig it must be derived from the
# golden UGNs (per directed link: wire = golden_ugn + marginFrames +
# seamInternalDelay, latency = wire + period + 3; see
# Hitl/ManticoreDemo/Latencies.hs linkLatency for the full derivation). Point
# MANTICORE_HOP_LATENCIES at that latencies.csv when it exists.
latency_args=()
if [ -n "${MANTICORE_HOP_LATENCIES:-}" ]; then
  if [ ! -f "${MANTICORE_HOP_LATENCIES}" ]; then
    echo "ERROR: MANTICORE_HOP_LATENCIES=${MANTICORE_HOP_LATENCIES} not found" >&2
    exit 1
  fi
  latency_args+=(--hop-latencies "${MANTICORE_HOP_LATENCIES}")
fi

echo "  java: $(java -version 2>&1 | head -1)"

# Publish the 'hardware' submodule (manticore-machine) to the local ivy repo:
# the compiler's build resolves it (a Test-scope dependency that sbt's `update`
# resolves regardless of the assembly task). Mirrors the compiler README.
echo "Publishing manticore-machine (hardware submodule)"
# Skip the scaladoc artifact: only the jar + pom are needed for dependency
# resolution, and doc generation is fragile on the (older) hardware sources.
( cd "${COMPILERDIR}/hardware" \
  && SBT_OPTS="-Xmx8g -Xss32M -Dsbt.watch.mode=polling -Dsbt.server.autostart=false" \
     "${SBT}" -batch "set Compile / packageDoc / publishArtifact := false" publishLocal )

echo "Building masm (manticore-compiler assembly)"
( cd "${COMPILERDIR}" \
  && SBT_OPTS="-Xmx8g -Xss32M -Dsbt.watch.mode=polling -Dsbt.server.autostart=false" \
     "${SBT}" -batch assembly )

JAR="${COMPILERDIR}/target/scala-2.13/manticore-compiler-assembly-0.1.0-SNAPSHOT.jar"
if [ ! -f "${JAR}" ]; then
  echo "ERROR: masm jar not found at ${JAR}" >&2
  exit 1
fi

# Resolve the program source file(s). MANTICORE_PROGRAM may be a space-separated
# list (e.g. the two MIPS32 .sv files); each entry is taken relative to the
# compiler repo or as an absolute path.
PROGRAM_FILES=()
# Split the (possibly multi-file) program spec on spaces despite the script's
# narrow IFS.
IFS=' ' read -r -a _program_entries <<< "${MANTICORE_PROGRAM}"
for entry in "${_program_entries[@]}"; do
  if [ -f "${entry}" ]; then
    PROGRAM_FILES+=("$(readlink -f "${entry}")")
  elif [ -f "${COMPILERDIR}/${entry}" ]; then
    PROGRAM_FILES+=("${COMPILERDIR}/${entry}")
  else
    echo "ERROR: program source '${entry}' not found (cwd or under ${COMPILERDIR})" >&2
    exit 1
  fi
done

# Verilog input needs the v2masmyosys frontend; a single .masm is read directly.
needs_frontend=1
if [ "${#PROGRAM_FILES[@]}" -eq 1 ] && [[ "${PROGRAM_FILES[0]}" == *.masm ]]; then
  needs_frontend=0
fi

if [ "${needs_frontend}" -eq 1 ]; then
  FRONTEND="${COMPILERDIR}/frontend"
  if [ ! -x "${FRONTEND}/v2masmyosys" ]; then
    if [ ! -f "${FRONTEND}/Makefile" ]; then
      echo "ERROR: frontend submodule missing at ${FRONTEND}; run manticore_clone.sh" >&2
      exit 1
    fi
    ABC="${MANTICORE_ABC:-$(command -v abc || true)}"
    if [ -z "${ABC}" ]; then
      echo "ERROR: external ABC not found; set MANTICORE_ABC or add abc-verifier to the env" >&2
      exit 1
    fi
    echo "Building the v2masmyosys frontend (external ABC: ${ABC})"
    # Force-include <cstdint>: this Yosys fork predates GCC 13+/15 dropping the
    # transitive <cstdint> include, so its vendored sources (json11, ...) use
    # uint8_t without including it. Yosys prepends the env CXXFLAGS to its own.
    CXXFLAGS="-include cstdint ${CXXFLAGS:-}" \
      make -C "${FRONTEND}" CONFIG=gcc ABCEXTERNAL="${ABC}" -j"$(nproc)"
  fi
fi

echo "Compiling program [${PROGRAM_FILES[*]}] for ${MANTICORE_DIMX}x${MANTICORE_DIMY}"
rm -rf "${OUTDIR}"
mkdir -p "${OUTDIR}"

# OR-Tools (used by placement) dlopens libstdc++.so.6 from its JNI lib; expose
# the C++ runtime ($MANTICORE_CXX_LIB from the nix shell) on the library path.
if [ -n "${MANTICORE_CXX_LIB:-}" ]; then
  export LD_LIBRARY_PATH="${MANTICORE_CXX_LIB}:${LD_LIBRARY_PATH:-}"
fi

# MASM_ROOT lets the compiler locate the frontend binary and repo-relative
# resources (mirror the ./masm wrapper). --dump-* mirror the manticore-runtime
# test flow (create_test.cmake).
MASM_ROOT="${COMPILERDIR}" java -cp "${JAR}" manticore.compiler.Main \
  -x "${MASM_X}" -y "${MASM_Y}" \
  -o "${OUTDIR}" \
  --dump-ascii --dump-register-file --dump-scratch-pad \
  "${torus_args[@]}" \
  "${latency_args[@]}" \
  "${cf_args[@]}" \
  "${PROGRAM_FILES[@]}"

if [ ! -f "${OUTDIR}/manifest.json" ]; then
  echo "ERROR: manifest.json not produced in ${OUTDIR}" >&2
  exit 1
fi

echo "Manticore program ready in ${OUTDIR}:"
echo "  manifest: ${OUTDIR}/manifest.json"
find "${OUTDIR}" -name 'exec.bin' -printf '  binary:   %p\n'
