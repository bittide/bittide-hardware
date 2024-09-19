#!/usr/bin/env bash
set -e
set -u

export JOB_ID="$1"
export CUTOFF_MS="$2"
export TOPOLOGY="$3"
export FIBER="$4"
export BITTIDE_PAPERS_DIR="$5"
export BITTIDE_PAPERS_RESULTS_DIR="${BITTIDE_PAPERS_DIR}"/asplos/results
export TARGET_DIR="${FIBER}"_10ppb_60ms_k2e9

export RUNREF="https://github.com/bittide/bittide-hardware/actions/runs/${JOB_ID}"

export PLOT_FINCFDECS="no"
cabal run bittide-tools:cc-plot "${JOB_ID}":hwCcTopologyTest _build

rm -rf "${BITTIDE_PAPERS_RESULTS_DIR}"/"${TARGET_DIR}"/"${TOPOLOGY}"
mv "${TOPOLOGY}" "${BITTIDE_PAPERS_RESULTS_DIR}"/"${TARGET_DIR}"/

export PLOT_FINCFDECS="yes"
cabal run bittide-tools:cc-plot "${JOB_ID}":hwCcTopologyTest _build

mv "${TOPOLOGY}"/clocks.pdf "${BITTIDE_PAPERS_RESULTS_DIR}"/"${TARGET_DIR}"/"${TOPOLOGY}"/clocks_with_fincfdecs.pdf
