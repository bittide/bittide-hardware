#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set -euf -o pipefail

git ls-files *.cabal cabal.project \
  | grep --extended-regexp --invert-match '^clash-vexriscv/' \
  | xargs --max-procs=0 -I {} cabal-gild -i "{}" -o "{}"
