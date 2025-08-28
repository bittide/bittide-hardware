#!/bin/bash
# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set -xou pipefail

grep -v '^Warning: The package list for .* is .* old\.$' "$1" \
    | grep -E -B 10 '^Warning:'

if [[ ${PIPESTATUS[1]} == 0 ]]; then
    echo "Cabal produced warnings. See ^"
    exit 1;
fi
