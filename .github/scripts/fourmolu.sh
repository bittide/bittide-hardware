#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set -euf -o pipefail

# The pre-commit framework passes the files to be processed as arguments.
# We can loop through all arguments ($@) and apply our commands to each file.
for file in "$@"; do
  fourmolu --quiet --mode inplace "$file"
  .github/scripts/fix_spdx_header.py "$file"
done
