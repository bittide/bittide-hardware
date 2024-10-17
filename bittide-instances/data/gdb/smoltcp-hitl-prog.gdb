# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
file "./_build/cargo/firmware-binaries/riscv32imc-unknown-none-elf/release/smoltcp_client"
target extended-remote :3333

# Load binary on CPU and start execution
load
start
