#!/bin/bash

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

out_dir=../../firmware-integration-tests
target_dir=../../target

rm -rf $out_dir
mkdir -p $out_dir

for test in $(ls src/bin/*.rs); do
  filename=$(basename $test .rs)
  expect_file=src/bin/$filename.expected

  cp $target_dir/riscv32imc-unknown-none-elf/release/$filename $out_dir/
  cp $expect_file $out_dir/
done
