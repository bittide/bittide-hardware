<!--
SPDX-FileCopyrightText: 2023 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# VexRiscv core for Clash

This repository contains a [VexRiscv](https://github.com/SpinalHDL/VexRiscv) based CPU core and
bindings for use in [Clash](https://clash-lang.org/).

For now this repository only contains one
[example CPU configuration](clash-vexriscv/example-cpu/src/main/scala/example/ExampleCpu.scala),
which implements a 32 bit IMC RISC-V core.

This package includes simulation via [`verilator`](https://github.com/verilator/verilator) as
well as a black-box for Verilog synthesis.

The core interfaces with other components via [Wishbone](https://cdn.opencores.org/downloads/wbspec_b4.pdf)
interfaces, using [`clash-protocols`](https://github.com/clash-lang/clash-protocols) types.

## Building

For building the CPU, the following software needs to be installed and available in the `PATH`:

- a recent JDK installation
- SBT, the scala build tool
- a recent C compiler
- `verilator`, at least version 5.001 (development version at time of writing)
- `make` for building the verilated library and FFI code

## Notes for using the core

- VexRiscv has a "reset vector" for the instruction bus. This is the initial PC that gets fetched.
  This address only gets presented to the IBUS after at least one cycle of RST being asserted.

- The contents of memories need to be stored in little endian. This means that for example the
  contents of an ELF file need to be endian-swapped before being used as the contents of the
  instruction storage. This applies to all storages.
