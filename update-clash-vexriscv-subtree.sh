#!/usr/bin/env bash
set -euo pipefail

# Usage:
#
#   ./update-clash-vexriscv-subtree.sh <hash>
#
# Example:
#
#   ./update-clash-vexriscv-subtree.sh 6a0805f237ec511d2db9cbabbb8f28e6bbe93db4
#

# SPDX-FileCopyrightText: 2022-2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

git subtree pull --prefix clash-vexriscv/ https://github.com/clash-lang/clash-vexriscv.git "$1" --squash
