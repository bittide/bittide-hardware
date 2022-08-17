#!/bin/bash

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# Exits with 0 if changes were made to files relevant to the RISCV formal
# checks. E.g., CI configuration, formal check configuration, RTL, ...
#

# If script is not run from a PR, we compare HEAD and HEAD~1. This works
# because we only use squash and merge as merge strategies, not rebase.
if [ -n "${GITHUB_HEAD_REF}" -a -n "${GITHUB_BASE_REF}" ]; then
head_ref=origin/${GITHUB_HEAD_REF}
base_ref=origin/${GITHUB_BASE_REF}
else
head_ref="HEAD"
base_ref="HEAD~1"
fi

git diff \
  --exit-code --quiet "${base_ref}".."${head_ref}" \
  -- .github/ contranomy/ riscv-formal-config/ riscv-formal/

[ $? -ne 0 ]
