#!/usr/bin/env bash
set -e
set -u

JOB_ID_2KM_k2e9=18092705175
JOB_ID_2M_k2e9=XXXXXXXXXX

BITTIDE_PAPERS_DIR=/home/martijn/code/paper_qbay_hw


./plot.sh "${JOB_ID_2KM_k2e9}" 50000  hourglass 2km k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"
./plot.sh "${JOB_ID_2KM_k2e9}" 5000   complete  2km k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"
./plot.sh "${JOB_ID_2KM_k2e9}" 15000  hypercube 2km k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"
./plot.sh "${JOB_ID_2KM_k2e9}" 80000  line      2km k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"
./plot.sh "${JOB_ID_2KM_k2e9}" 25000  torus2d   2km k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"

./plot.sh "${JOB_ID_2M_k2e9}" 50000  hourglass 2m k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"
./plot.sh "${JOB_ID_2M_k2e9}" 5000   complete  2m k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"
./plot.sh "${JOB_ID_2M_k2e9}" 15000  hypercube 2m k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"
./plot.sh "${JOB_ID_2M_k2e9}" 80000  line      2m k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"
./plot.sh "${JOB_ID_2M_k2e9}" 25000  torus2d   2m k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"
