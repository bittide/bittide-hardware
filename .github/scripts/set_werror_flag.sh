#!/bin/bash
# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# cd to root of git repo
cd "$(git rev-parse --show-toplevel)"

FILE=".github/synthesis/debug.json"

if [ -f "${FILE}" ]; then
  echo "${FILE} exists, not copying cabal.project.local.."
else
  echo "${FILE} does not exist, copying cabal.project.local.."
  cp .github/cabal.project.local cabal.project.local
fi
