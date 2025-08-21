#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set -euf -o pipefail


for file in "$@"; do
  fourmolu --quiet --mode inplace "$file"
  .github/scripts/fix_spdx_header.py "$file"
done
