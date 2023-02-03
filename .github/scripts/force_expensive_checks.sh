#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set -exuo pipefail
IFS=$'\n\t'

commit_msg="$(git log -1 --pretty=%B)"

set +e
echo "${commit_msg}" | grep -q '\[force_expensive_checks\]'
exit_code="$?"
set -e

if [[ "${exit_code}" == "0" ]]; then
  echo "true"
elif [[ "${exit_code}" == "1" ]]; then
  echo "false"
else
  echo "Unexpected exit code: ${exit_code}" >> /dev/stderr
  exit 1;
fi
