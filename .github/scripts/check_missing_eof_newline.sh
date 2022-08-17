#!/bin/bash

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

set -xou pipefail

pcregrep \
    --exclude=*.svg \
    --exclude=*.drawio \
    --exclude-dir=riscv-formal \
    --exclude-dir=.git \
    --binary-files=without-match -LMr '\n\Z' .

if [[ $? == 0 ]]; then
    echo "Files without a newline end detected. See ^"
    exit 1;
fi
