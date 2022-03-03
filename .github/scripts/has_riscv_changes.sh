#!/bin/bash
# Exits with 0 if changes were made to files relevant to the RISCV formal
# checks. E.g., CI configuration, formal check configuration, RTL, ...
#

# If script is not run from a PR, we compare HEAD and HEAD~1. This works
# because we only use squash and merge as merge strategies, not rebase.
head_ref=${GITHUB_HEAD_REF:-HEAD}
base_ref=${GITHUB_BASE_REF:-HEAD~1}

git diff \
  --exit-code --quiet "${base_ref}".."${head_ref}" \
  .github/ contranomy/ riscv-formal-config/ riscv-formal/

[ $? -ne 0 ]
