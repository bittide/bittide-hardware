#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set -e

# Default stdout to /dev/null
OPENOCD_STDOUT_LOG="${OPENOCD_STDOUT_LOG:-/dev/null}"
stdout_dir="$(dirname "${OPENOCD_STDOUT_LOG}")"
mkdir -p "${stdout_dir}"
OPENOCD_STDOUT_LOG="$(realpath "${OPENOCD_STDOUT_LOG}")"

# Default stderr to /dev/null
OPENOCD_STDERR_LOG="${OPENOCD_STDERR_LOG:-/dev/null}"
stderr_dir="$(dirname "${OPENOCD_STDERR_LOG}")"
mkdir -p "${stderr_dir}"
OPENOCD_STDERR_LOG="$(realpath "${OPENOCD_STDERR_LOG}")"

cd "$(dirname "$0")"
exec openocd-vexriscv $@ \
  > >(tee "${OPENOCD_STDOUT_LOG}") \
  2> >(tee "${OPENOCD_STDERR_LOG}" >&2)
