#!/bin/bash

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

set -xou pipefail

grep -E ' $' -n -r . \
    --include=*.{hs,hs-boot,sh,cabal,.md} \
    --exclude-dir=contranomy/dist-newstyle \
    --exclude-dir=clash-vexriscv

if [[ $? == 0 ]]; then
    echo "EOL whitespace detected. See ^"
    exit 1;
fi
