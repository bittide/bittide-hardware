#!/bin/bash
set -xou pipefail

pcregrep --exclude-dir=riscv-formal --exclude-dir=.git -LMr '\n\Z' .

if [[ $? == 0 ]]; then
    echo "Files without a newline end detected. See ^"
    exit 1;
fi
