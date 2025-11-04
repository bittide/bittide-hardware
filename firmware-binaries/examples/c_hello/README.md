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
   - Compiles C code with correct RISC-V flags

## Getting the correct addresses

Before running this, you need to update the memory addresses in `src/main.c` to match your hardware configuration:

1. Build the hardware to generate the memory map:
   ```bash
   shake <your-target>:hdl
   ```

2. Check the generated memory map:
   ```bash
   cat _build/memory_maps/VexRiscv.json
   ```

3. Look for the device addresses and update the `#define` statements in `main.c`:
   - UART_BASE
   - STATUS_BASE
   - etc.

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

## Adding more C files

In `build.rs`, add more `.file()` calls:

```rust
cc::Build::new()
    .file("src/main.c")
    .file("src/utils.c")
    .file("src/drivers.c")
    .flag("-march=rv32imc")
    .flag("-mabi=ilp32")
    .flag("-Os")
    .compile("c_main");
```
