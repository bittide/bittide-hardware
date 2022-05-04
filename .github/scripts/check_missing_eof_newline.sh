#!/bin/bash

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

set -xou pipefail

pcregrep --exclude-dir=riscv-formal --exclude-dir=.git --exclude-dir=devicetree/blobs -LMr '\n\Z' .

if [[ $? == 0 ]]; then
    echo "Files without a newline end detected. See ^"
    exit 1;
fi
