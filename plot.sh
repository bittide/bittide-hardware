#!/usr/bin/env bash
set -e
set -u

export JOB_ID="$1"
export CUTOFF_MS="$2"
export TOPOLOGY="$3"
export FIBER="$4"
export K_P="$5"
export STEP_SIZE="$6"
export SAMPLE_SPEED="$7"
export BITTIDE_PAPERS_DIR="$8"
export BITTIDE_PAPERS_RESULTS_DIR="${BITTIDE_PAPERS_DIR}"/extras/results
export TARGET_DIR="${FIBER}_${STEP_SIZE}_${SAMPLE_SPEED}_${K_P}"

export RUNREF="https://github.com/bittide/bittide-hardware/actions/runs/${JOB_ID}"

export PLOT_FINCFDECS="no"
cabal run bittide-tools:cc-plot "${JOB_ID}":hwCcTopologyTest _build

mkdir -p "${BITTIDE_PAPERS_RESULTS_DIR}"/"${TARGET_DIR}"/
rm -rf "${BITTIDE_PAPERS_RESULTS_DIR}"/"${TARGET_DIR}"/"${TOPOLOGY}"
mv "${TOPOLOGY}" "${BITTIDE_PAPERS_RESULTS_DIR}"/"${TARGET_DIR}"/

export PLOT_FINCFDECS="yes"
cabal run bittide-tools:cc-plot "${JOB_ID}":hwCcTopologyTest _build

mv "${TOPOLOGY}"/clocks.pdf "${BITTIDE_PAPERS_RESULTS_DIR}"/"${TARGET_DIR}"/"${TOPOLOGY}"/clocks_with_fincfdecs.pdf
