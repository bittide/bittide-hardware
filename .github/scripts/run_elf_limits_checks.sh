#!/bin/bash

# SPDX-FileCopyrightText: 2026 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

TOPDIR="$(git rev-parse --show-toplevel)"
BINDIR="${TOPDIR}/_build/cargo/firmware-binaries/riscv32imc-unknown-none-elf"
SRCDIR="${TOPDIR}/firmware-binaries"

# PATH setup to include ~/.cargo/bin
case ":${PATH}:" in
  *:"$HOME/.cargo/bin":*)
    # Already included
    ;;
  *)
    if [ -z "${IN_NIX_SHELL}" ]
    then
      # Not in nix shell, can prepend
      export PATH="$HOME/.cargo/bin:${PATH}"
    else
      # In nix shell, prefer suffix
      export PATH="${PATH}:$HOME/.cargo/bin"
    fi
    ;;
esac

__conv_length() {
  local LENNUM
  read LENNUM
  case $LENNUM in
    0x* | 0X*)
      echo "$((16#$(echo ${LENNUM} | sd '0[xX]' '')))"
      ;;
    *)
      echo "${LENNUM}"
      ;;
  esac
}

__run_elf_limits_checks() {
  for MEMBER in $(tq -f "${SRCDIR}/Cargo.toml" 'workspace.members' | sd '[\[\],"]' '')
  do
    for MODE in {"debug","release"}
    do
      BINNAME="$(echo $MEMBER | sd '.+/(.+)' '$1')"
      MEMFILE="$(fd 'memory.x' "${BINDIR}/${MODE}/build/" | rg "${BINNAME}-[a-z0-9]+/" | head -1 | sd '\n' '')"
      if [ -z "${MEMFILE}" ]
      then
          echo "Failed to find memory.x file for binary ${BINNAME}"
          return 1
      fi
      MEMDATA="$(rg '^ +[ID]MEM' "${MEMFILE}")"
      IMEM="$(echo "$MEMDATA" | rg 'IMEM' | sd '.+LENGTH ?= ?(.+)' '$1' | __conv_length)"
      DMEM="$(echo "$MEMDATA" | rg 'DMEM' | sd '.+LENGTH ?= ?(.+)' '$1' | __conv_length)"
      elf-limits \
        --instruction-mem-limit "${IMEM}" \
        --data-mem-limit "${DMEM}" \
        "${BINDIR}/${MODE}/${BINNAME}" \
        || return 1
    done
  done
}

__run_elf_limits_checks
