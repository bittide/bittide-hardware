#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set -euf -o pipefail

git ls-files *.hs \
  | grep --extended-regexp --invert-match '^clash-vexriscv/' \
  | xargs --max-procs=0 -I {} fourmolu --quiet --mode inplace "{}"
