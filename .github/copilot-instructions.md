<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Bittide Hardware Repository - Copilot Instructions

Run in docker container. See .github/workflows/copilot-setup-steps.yml for reference. The
container has all dependencies installed to build the project. Always run within a Nix shell:

```bash
nix develop --extra-experimental-features "nix-command flakes" --command bash -c "echo OK"
```

## Overview

This repository implements a **hardware-software co-design system** for a Bittide FPGA-based architecture. Here's how everything fits together:

### **1. Hardware Build Flow (Clash HDL → Verilog → FPGA)**

**Hardware is written in Clash HDL** (a Haskell-based hardware description language):

1. **Clash Compilation** (`bittide/`, `bittide-instances/`)
   - Write hardware designs in Clash (Haskell)
   - Build with: `cabal build bittide-instances`
   - Generates **Verilog** output to `_build/clash/`
   - Generates **memory maps** (JSON) to `_build/memory_maps/`

**Firmware is written in Rust** for RISC-V processors embedded in the FPGA:

**The CPU**: VexRiscv (RV32IMC) - a soft RISC-V core synthesized in the FPGA
- Lives in `clash-vexriscv/`
- Generated from Scala → Verilog
- Integrated into Clash designs via **C++ FFI** for simulation
- Interfaces: Wishbone buses for instruction/data memory and peripherals

**Firmware Structure**:
- `firmware-support/` - Hardware Abstraction Layer (HAL)
  - `bittide-hal/` - Device drivers generated from memory maps
  - `bittide-sys/` - System libraries (networking via smoltcp)
  - `bittide-macros/` - Rust macros
  - `memmap-generate/` - **Code generator** (see below)

- `firmware-binaries/` - Actual programs
  - `examples/` - Hello world, networking demos
  - `test-cases/` - Hardware tests
  - `clock-control/`, `management-unit/` - System firmware

**Build Process**:
```bash
./cargo.sh build --release  # Builds for both workspaces
