#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2026 QBayLogic
#
# SPDX-License-Identifier: Apache-2.0

# Clone (or update) the Manticore repositories used by the Manticore bittide demo
# into _build/manticore/src. Both the RTL generator (manticore-hw) and the
# program compiler (manticore-compiler) are needed.
#
# Configure via environment variables (defaults track the lmbollen forks; the
# pinned commits below are the branch tips at the time of writing):
#   MANTICORE_HW_REPO        (default: https://github.com/lmbollen/manticore-hw.git)
#   MANTICORE_HW_REF         (default: kcu105-port            @ 9f625400)
#   MANTICORE_COMPILER_REPO  (default: https://github.com/lmbollen/manticore-compiler.git)
#   MANTICORE_COMPILER_REF   (default: kcu105-frontend-fix    @ d0e485e5)
#   MANTICORE_FRONTEND_REPO  (default: the .gitmodules URL; frontend submodule @ dc0086913)
#
# A repo value may be a local path (e.g. /path/to/manticore-hw) for testing
# without pushing; it is cloned the same way.

set -euo pipefail
IFS=$'\n\t'

TOPDIR="$(git rev-parse --show-toplevel)"
SRCDIR="${TOPDIR}/_build/manticore/src"

MANTICORE_HW_REPO="${MANTICORE_HW_REPO:-https://github.com/lmbollen/manticore-hw.git}"
MANTICORE_HW_REF="${MANTICORE_HW_REF:-kcu105-port}"
MANTICORE_COMPILER_REPO="${MANTICORE_COMPILER_REPO:-https://github.com/lmbollen/manticore-compiler.git}"
MANTICORE_COMPILER_REF="${MANTICORE_COMPILER_REF:-kcu105-frontend-fix}"

# $1 = repo (url or path), $2 = ref, $3 = destination directory
clone_or_update() {
  local repo="$1"
  local ref="$2"
  local dest="$3"

  if [ -d "${dest}/.git" ]; then
    echo "Updating ${dest} (${repo} @ ${ref})"
    git -C "${dest}" remote set-url origin "${repo}"
    git -C "${dest}" fetch --depth 1 origin "${ref}"
  else
    echo "Cloning ${repo} @ ${ref} into ${dest}"
    rm -rf "${dest}"
    mkdir -p "${dest}"
    git clone --depth 1 --branch "${ref}" "${repo}" "${dest}" 2>/dev/null \
      || { # branch flag fails for a bare commit-ish; fall back to fetch
        git -C "${dest}" init -q
        git -C "${dest}" remote add origin "${repo}"
        git -C "${dest}" fetch --depth 1 origin "${ref}"
      }
  fi
  git -C "${dest}" checkout -q FETCH_HEAD 2>/dev/null || git -C "${dest}" checkout -q "${ref}"
  echo "  -> $(git -C "${dest}" rev-parse HEAD)"
}

mkdir -p "${SRCDIR}"
clone_or_update "${MANTICORE_HW_REPO}"       "${MANTICORE_HW_REF}"       "${SRCDIR}/manticore-hw"
clone_or_update "${MANTICORE_COMPILER_REPO}" "${MANTICORE_COMPILER_REF}" "${SRCDIR}/manticore-compiler"

# The program compiler's Verilog frontend (v2masmyosys) is a git submodule. Only
# it is needed (the `hardware`/`runtime` submodules are test-only); init just it.
# MANTICORE_FRONTEND_REPO overrides the submodule URL (e.g. a local checkout).
COMPILER_DIR="${SRCDIR}/manticore-compiler"
if [ -n "${MANTICORE_FRONTEND_REPO:-}" ]; then
  git -C "${COMPILER_DIR}" config submodule.frontend.url "${MANTICORE_FRONTEND_REPO}"
fi
echo "Initializing the 'frontend' submodule of manticore-compiler"
# -c protocol.file.allow=always: permits a local-path submodule URL (git blocks
# the 'file' transport by default since CVE-2022-39253). No effect on https URLs.
git -C "${COMPILER_DIR}" -c protocol.file.allow=always submodule update --init -- frontend
echo "  frontend -> $(git -C "${COMPILER_DIR}/frontend" rev-parse HEAD)"

echo "Manticore sources ready under ${SRCDIR}"
