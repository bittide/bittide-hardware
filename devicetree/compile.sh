#!/bin/bash

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

set -euxo pipefail

for src in ./*.dts; do
  filename=$(basename "$src" .dts)
  dtc -O dtb -b 0 "$src" -o "blobs/$filename.dtb"
done
