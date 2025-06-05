#!/bin/bash
# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set -xou pipefail

grep -E -B 10 '^Warning:' "$1"

if [[ $? == 0 ]]; then
    echo "Cabal produced warnings. See ^"
    exit 1;
fi
