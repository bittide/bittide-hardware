<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# bittide-hal-c

Auto-generated C header files for Bittide hardware memory maps.

## Overview

This crate automatically generates C header files from the Bittide hardware memory map JSON files. The headers provide type-safe memory-mapped I/O access for C firmware running on the RISC-V cores in the FPGA.

## Generated Headers

Headers are generated in the `generated/` directory at build time. Each memory map JSON file produces a corresponding `.h` file:

- `VexRiscv.json` → `generated/vexriscv_memmap.h`
- `Ethernet.json` → `generated/ethernet_memmap.h`
- `ScatterGather.json` → `generated/scattergather_memmap.h`
- etc.

## What's in the Headers?

Each header contains:

### Device Base Addresses
```c
#define UART_BASE           0x60000000UL
#define TIMER_BASE          0x40000000UL
```

### Register Definitions
```c
/**
 * UART data register
 * Type: BitVector(8)
 * Access: read-write
 * Size: 1 byte
 */
#define UART_DATA           ((volatile uint8_t*)(UART_BASE + 0x00))

/**
 * UART status register
 * Type: BitVector(8)
 * Access: read-only
 * Size: 1 byte
 */
#define UART_STATUS         ((volatile uint8_t*)(UART_BASE + 0x04))
```

## Using in C Firmware

### Option 1: Direct Include Path

In your `build.rs`:

```rust
use std::path::PathBuf;

fn main() {
    let hal_c_headers = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../firmware-support/bittide-hal-c/generated");

    cc::Build::new()
        .file("src/main.c")
        .include(hal_c_headers)
        .compile("my_firmware");
}
```

Then in your C code:

```c
#include "vexriscv_memmap.h"

void main(void) {
    *UART_DATA = 'H';
    *UART_DATA = 'i';
}
```

### Option 2: Copy Header to Your Project

You can also copy the specific header(s) you need into your project's `src/` directory and include them directly.

## Rebuilding

Headers are automatically regenerated when:
- Any JSON file in `_build/memory_maps/` changes
- The hardware is rebuilt (via `cabal build` or `shake <target>:hdl`)

To manually rebuild:

```bash
cd firmware-support/bittide-hal-c
cargo build
```

The generated headers will be in `firmware-support/bittide-hal-c/generated/`.

## Multiple Device Instances

When there are multiple instances of the same device type, they are automatically numbered:

```c
#define CAPTURE_UGN_0_BASE           0x90000000UL
#define CAPTURE_UGN_1_BASE           0xA0000000UL
#define CAPTURE_UGN_2_BASE           0xB0000000UL

#define CAPTURE_UGN_0_LOCAL_COUNTER  ((volatile uint64_t*)(CAPTURE_UGN_0_BASE + 0x00))
#define CAPTURE_UGN_1_LOCAL_COUNTER  ((volatile uint64_t*)(CAPTURE_UGN_1_BASE + 0x00))
```

## See Also

- `bittide-hal` - Rust HAL with type-safe device drivers
- `memmap-generate` - Code generation library
- `firmware-binaries/examples/c_hello` - Example C firmware
