#!/bin/bash
set -xou pipefail

grep -E ' $' -n -r . \
    --include=*.{hs,hs-boot,sh} \
    --exclude-dir=contranomy/dist-newstyle \
    --exclude-dir=riscv-formal

if [[ $? == 0 ]]; then
    echo "EOL whitespace detected. See ^"
    exit 1;
fi
