#!/usr/bin/env bash
set -e
set -u

# Warning: these expire in 7 days or so, so grab em fast
JOB_ID_2KM_k2e9=18487988993
JOB_ID_2M_k2e9=18474989166
JOB_ID_2M_k2e8=18474989997

BITTIDE_PAPERS_DIR=/home/martijn/code/paper_qbay_hw

# XXX: You cannot run these plot scripts at the same time, because they critically
#      depend on a bunch of types that change between settings!

cd _build
rm -rf hitl vivado
gh run download "${JOB_ID_2KM_k2e9}" --name _build-hwCcTopologyTest-debug
cd ..
rm -rf complete/  cycle/ hourglass/ hypercube/ line/ torus2d/

./plot.sh "${JOB_ID_2KM_k2e9}" 50000  hourglass 2km k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"
./plot.sh "${JOB_ID_2KM_k2e9}" 5000   complete  2km k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"
./plot.sh "${JOB_ID_2KM_k2e9}" 15000  hypercube 2km k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"
./plot.sh "${JOB_ID_2KM_k2e9}" 75000  line      2km k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"
./plot.sh "${JOB_ID_2KM_k2e9}" 15000  torus2d   2km k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"

# cd _build
# rm -rf hitl vivado
# gh run download "${JOB_ID_2M_k2e9}" --name _build-hwCcTopologyTest-debug
# cd ..
# rm -rf complete/  cycle/ hourglass/ hypercube/ line/ torus2d/

# ./plot.sh "${JOB_ID_2M_k2e9}" 50000 hourglass 2m k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"
# ./plot.sh "${JOB_ID_2M_k2e9}" 5000  complete  2m k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"
# ./plot.sh "${JOB_ID_2M_k2e9}" 15000 hypercube 2m k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"
# ./plot.sh "${JOB_ID_2M_k2e9}" 75000 line      2m k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"
# ./plot.sh "${JOB_ID_2M_k2e9}" 15000 torus2d   2m k2e9 10ppb 60ms "${BITTIDE_PAPERS_DIR}"

# cd _build
# rm -rf hitl vivado
# gh run download "${JOB_ID_2M_k2e8}" --name _build-hwCcTopologyTest-debug
# cd ..
# rm -rf complete/  cycle/ hourglass/ hypercube/ line/ torus2d/

# ./plot.sh "${JOB_ID_2M_k2e8}" 5000  hourglass 2m k2e8 100ppb 20ms "${BITTIDE_PAPERS_DIR}"
# ./plot.sh "${JOB_ID_2M_k2e8}" 500   complete  2m k2e8 100ppb 20ms "${BITTIDE_PAPERS_DIR}"
# ./plot.sh "${JOB_ID_2M_k2e8}" 1500  hypercube 2m k2e8 100ppb 20ms "${BITTIDE_PAPERS_DIR}"
# ./plot.sh "${JOB_ID_2M_k2e8}" 7500  line      2m k2e8 100ppb 20ms "${BITTIDE_PAPERS_DIR}"
# ./plot.sh "${JOB_ID_2M_k2e8}" 1500  torus2d   2m k2e8 100ppb 20ms "${BITTIDE_PAPERS_DIR}"
