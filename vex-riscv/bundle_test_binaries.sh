#!/bin/bash

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

out_dir=build_out_dir/target
target_dir=../target

out_dir_release=$out_dir/riscv32imc-unknown-none-elf/release
out_dir_debug=$out_dir/riscv32imc-unknown-none-elf/debug

rm -rf $out_dir
mkdir -p $out_dir
mkdir -p $out_dir_debug
mkdir -p $out_dir_release


for test in $(ls vex-test-programs/src/bin/*.rs); do
  filename=$(basename $test .rs)
  expect_file=src/bin/$filename.expected

  cp $target_dir/riscv32imc-unknown-none-elf/release/$filename $out_dir_release/
  cp $target_dir/riscv32imc-unknown-none-elf/debug/$filename $out_dir_debug/
done

cd build_out_dir
tar -c -f ../vex-riscv-test-binaries.tar target
cd ..
