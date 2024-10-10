# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
file "./_build/cargo/firmware-binaries/riscv32imc-unknown-none-elf/debug/hello"
target extended-remote :3333

# Load binary on CPU
load

# Set a breakpoint at 'test_success' (note the CPU starts at '_start')
break hello::test_success

# Reset program, should run until 'test_success' is hit
jump _start

# List register values
i r

# Show stack trace
bt

# Show available functions
info functions

# Remove breakpoint
disable 1

# Ask super process to terminate us
continue
