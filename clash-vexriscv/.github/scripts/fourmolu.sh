#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set -euf -o pipefail

cd "$(git rev-parse --show-toplevel)"

echo "Fourmolu.."
git ls-files *.hs | xargs --max-procs=0 -I {} fourmolu --quiet --mode inplace "{}"

echo "Fixing SPDX headers.."
git ls-files *.hs | xargs --max-procs=0 -I {} python3 .github/scripts/fix_spdx_header.py "{}"

