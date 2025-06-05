#!/bin/bash
# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set -xou pipefail

grep --include=*.hs -E -r '\-fconstraint-solver-iterations *= *0'

if [[ $? == 0 ]]; then
    echo "Found infinite recursion setting for solver plugins. Please don't do this!"
    exit 1;
fi
