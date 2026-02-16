<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# C Hello Example

This example demonstrates how to write RISC-V firmware in C while using the Rust `riscv-rt` runtime for initialization.

## How it works

1. **Rust wrapper** (`src/main.rs`): Minimal wrapper that uses `riscv-rt` for startup and calls the C `c_main()` function
2. **C code** (`src/main.c`): Your actual C program with direct memory-mapped I/O
3. **Build script** (`build.rs`):
   - Generates `memory.x` linker script from the hardware memory map
   - Uses **C headers from `bittide-hal-c`** for device/register addresses
   - Compiles C code with correct RISC-V flags

## Auto-generated memory map headers

This example uses the **`bittide-hal-c`** crate, which automatically generates C headers from all hardware memory maps. The headers are located in `firmware-support/bittide-hal-c/generated/` and contain:

- `#define` macros for device base addresses (e.g., `UART_BASE`, `TIMER_BASE`)
- `#define` macros for register access as volatile pointers (e.g., `UART_DATA`, `TIMER_COMMAND`)
- Documentation comments with type, access, and size information

Example usage in C:
```c
#include "vexriscv_memmap.h"

// Write to UART data register
*UART_DATA = 'H';

// Read timer frequency
uint64_t freq = *TIMER_FREQUENCY;
```

The headers are shared across all C firmware projects, ensuring consistency.

## Building

From the repository root (in the nix develop shell):

```bash
cd firmware-binaries
cargo build --release --target riscv32imc-unknown-none-elf -p c_hello
```

The resulting ELF file will be at:
```
_build/cargo/firmware-binaries/riscv32imc-unknown-none-elf/release/c_hello
```

Note: This uses **clang** which has built-in RISC-V cross-compilation support, so no separate cross-compiler toolchain is needed.

## Running in Simulator

Test your C program in the VexRiscv simulator:

```bash
TEST_BINARY_NAME=c_hello cabal run vexriscv-sim
```

You should see:
```
Hello from C!
Running on RISC-V
Test value: 0xdeadbeef
```

## Using Other Memory Maps

To use headers from other devices (e.g., Ethernet), just include the appropriate header:

```c
#include "vexriscv_memmap.h"
#include "ethernet_memmap.h"

void init_ethernet(void) {
    // Use Ethernet device registers
    *MAC_STATUS_STATUS = 0;
}
```

All available headers are in `firmware-support/bittide-hal-c/generated/`.

## Adding more C files

In `build.rs`, add more `.file()` calls:

```rust
cc::Build::new()
    .file("src/main.c")
    .file("src/utils.c")
    .file("src/drivers.c")
    .flag("-march=rv32imc")
    .flag("-mabi=ilp32f")
    .flag("-Os")
    .compile("c_main");
```
