#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set -e

if [ -z "${EXEC_PATH}" ]
then
  return 1
fi

# Default stdout to /dev/null
STDOUT_LOG="${STDOUT_LOG:-/dev/null}"
stdout_dir="$(dirname "${STDOUT_LOG}")"
mkdir -p "${stdout_dir}"
STDOUT_LOG="$(realpath "${STDOUT_LOG}")"

# Default stderr to /dev/null
STDERR_LOG="${STDERR_LOG:-/dev/null}"
stderr_dir="$(dirname "${STDERR_LOG}")"
mkdir -p "${stderr_dir}"
STDERR_LOG="$(realpath "${STDERR_LOG}")"

cd "$(dirname "$0")"
exec ${EXEC_PATH} $@ \
  > >(tee "${STDOUT_LOG}") \
  2> >(tee "${STDERR_LOG}" >&2)
