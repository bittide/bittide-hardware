#!/bin/bash

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

set -euxo pipefail

ROOT=$(git rev-parse --show-toplevel)

cd "${ROOT}"/devicetree

# read textual representation of blobs
for src in blobs/*.dtb; do
  filename=$(basename "$src" .dtb)

  dtc -I dtb -O dts "$src" > "blobs/$filename.dts.before"
done


./compile.sh

# read textual representation of *new* blobs
for src in blobs/*.dtb; do
  filename=$(basename "$src" .dtb)

  dtc -I dtb -O dts "$src" > "blobs/$filename.dts.after"
done

# compare the textual representations
for src in blobs/*.dtb; do
  filename=$(basename "$src" .dtb)

  diff -u "blobs/$filename.dts.before" "blobs/$filename.dts.after"
done

rm blobs/*.after blobs/*.before
