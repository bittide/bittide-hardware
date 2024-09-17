#!/usr/bin/env bash
set -e
set -u

JOB_ID_2KM=10850182763
JOB_ID_2M=10850489509

BITTIDE_PAPERS_DIR=/home/martijn/code/bittide-papers


./plot.sh "${JOB_ID_2KM}" 50000 hourglass 2km "${BITTIDE_PAPERS_DIR}"
./plot.sh "${JOB_ID_2KM}" 5000  complete  2km "${BITTIDE_PAPERS_DIR}"
./plot.sh "${JOB_ID_2KM}" 10000 hypercube 2km "${BITTIDE_PAPERS_DIR}"

./plot.sh "${JOB_ID_2M}"  50000 hourglass 2m "${BITTIDE_PAPERS_DIR}"
./plot.sh "${JOB_ID_2M}"  5000  complete  2m "${BITTIDE_PAPERS_DIR}"
./plot.sh "${JOB_ID_2M}"  10000 hypercube 2m "${BITTIDE_PAPERS_DIR}"
