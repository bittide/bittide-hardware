#!/bin/bash

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0


# Because our Rust code is split between different Cargo workspaces
# for Reasons TM, cargo commands can't be run directly.
#
# This script runs any command in *all* the workspaces.


# Echoes the given command to stderr, then executes it.
function run() {
    # https://stackoverflow.com/a/76153233/14637
    echo -n '>' >&2
    for arg in "$@"; do
        printf " %q" "$arg" >&2
    done
    echo >&2
    "$@"
}

cd firmware-support;
  printf "firmware-support " >&2;
  run cargo "$@";
  fs_res="$?"
cd ..

cd firmware-binaries;
  printf "firmware-binaries " >&2;
  run cargo "$@";
  fb_res="$?"
cd ..

cd host-tools;
  printf "host-tools " >&2;
  run cargo "$@";
  ht_res="$?"
cd ..

if [[ fs_res -ne 0 ]]; then
  echo "firmware-support failure!"
fi

if [[ fb_res -ne 0 ]]; then
  echo "firmware-binaries failure!"
fi

if [[ ht_res -ne 0 ]]; then
  echo "host-tools failure!"
fi

if [[ fs_res -ne 0 ]] || [[ fb_res -ne 0 ]] || [[ ht_res -ne 0 ]]; then
  exit 1
fi
