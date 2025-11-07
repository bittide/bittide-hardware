// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Bittide Hardware Abstraction Layer for C
//!
//! This crate provides auto-generated C header files for all Bittide hardware
//! memory maps, plus a hardware abstraction layer with separate headers and
//! implementations.
//!
//! ## Generated Headers
//!
//! Headers are generated in the `generated/` directory with names like:
//! - `vexriscv_memmap.h`
//! - `ethernet_memmap.h`
//! - `scattergather_memmap.h`
//! - etc.
//!
//! Each header contains:
//! - Device base address defines (e.g., `UART_BASE`)
//! - Register offset defines as volatile pointers (e.g., `UART_DATA`)
//! - Documentation comments with type, access, and size information
//!
//! ## Hardware Abstraction Layer
//!
//! The HAL is organized into header files (`include/`) and implementation files (`src/`):
//!
//! ### Headers (`include/`)
//! - **`bittide_hal.h`**: Main header that includes all HAL components
//! - **`bittide_timer.h`**: Timer, Duration, and Instant type definitions and function declarations
//! - **`bittide_uart.h`**: UART type definitions and function declarations
//!
//! ### Implementations (`src/`)
//! - **`bittide_timer.c`**: Timer function implementations (~40 functions)
//! - **`bittide_uart.c`**: UART function implementations (9 functions)
//!
//! ### Timer API (`bittide_timer.h` / `bittide_timer.c`)
//!
//! Provides two-layer API:
//! - **Low-level (cycles)**: Direct cycle count operations
//!   - `timer_now_cycles()`, `timer_wait_cycles()`, `timer_wait_until_cycles()`
//! - **High-level (microseconds)**: Duration and Instant abstractions
//!   - `Duration`: Relative time representation (microseconds)
//!   - `Instant`: Absolute time representation (microseconds)
//!   - `Timer`: Timer device with memory-mapped registers
//!   - Functions: `timer_now()`, `timer_wait()`, `timer_wait_until()`, etc.
//!
//! ### UART API (`bittide_uart.h` / `bittide_uart.c`)
//!
//! Struct-based API with initialization:
//! - `Uart`: UART device structure holding register pointers
//! - `uart_init()`: Initialize UART with register addresses
//! - Output: `uart_putc()`, `uart_puts()`
//! - Input: `uart_getc()`
//! - Formatting: `uart_puthex32()`, `uart_puthex64()`, `uart_putdec()`
//!
//! ## Usage from C Firmware
//!
//! To use the HAL in your C firmware build.rs:
//!
//! ```rust,no_run
//! # #[allow(clippy::needless_doctest_main)]
//! use std::path::PathBuf;
//!
//! fn main() {
//!     // Add the bittide-hal-c generated directory to the include path
//!     let hal_c_generated = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
//!         .join("../../firmware-support/bittide-hal-c/generated");
//!
//!     // Add the bittide-hal-c include directory for the HAL headers
//!     let hal_c_include = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
//!         .join("../../firmware-support/bittide-hal-c/include");
//!
//!     // Add the bittide-hal-c src directory for the HAL implementations
//!     let hal_c_src = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
//!         .join("../../firmware-support/bittide-hal-c/src");
//!
//!     let mut build = cc::Build::new();
//!     build
//!         .file("src/main.c")
//!         .include(&hal_c_generated)
//!         .include(&hal_c_include);
//!
//!     // Add all .c files from HAL src directory
//!     for entry in std::fs::read_dir(&hal_c_src).expect("Failed to read HAL src directory") {
//!         let entry = entry.expect("Failed to read directory entry");
//!         let path = entry.path();
//!         if path.extension().and_then(|s| s.to_str()) == Some("c") {
//!             build.file(&path);
//!         }
//!     }
//!
//!     build.compile("my_firmware");
//!
//!     // Add rerun-if-changed for HAL headers
//!     for entry in std::fs::read_dir(&hal_c_include).expect("Failed to read HAL include directory") {
//!         let entry = entry.expect("Failed to read directory entry");
//!         let path = entry.path();
//!         if path.extension().and_then(|s| s.to_str()) == Some("h") {
//!             println!("cargo:rerun-if-changed={}", path.display());
//!         }
//!     }
//!
//!     // Add rerun-if-changed for HAL implementations
//!     for entry in std::fs::read_dir(&hal_c_src).expect("Failed to read HAL src directory") {
//!         let entry = entry.expect("Failed to read directory entry");
//!         let path = entry.path();
//!         if path.extension().and_then(|s| s.to_str()) == Some("c") {
//!             println!("cargo:rerun-if-changed={}", path.display());
//!         }
//!     }
//! }
//! ```
//!
//! Then in your C code:
//!
//! ```c
//! #include "vexriscv_memmap.h"
//! #include "bittide_hal.h"  // Includes bittide_timer.h and bittide_uart.h
//!
//! void main(void) {
//!     // Initialize UART
//!     Uart uart = uart_init(UART_DATA, UART_STATUS);
//!     uart_puts(&uart, "Hello, World!\r\n");
//!
//!     // Initialize timer
//!     Timer timer = timer_init(
//!         TIMER_COMMAND,
//!         TIMER_SCRATCHPAD,
//!         TIMER_FREQUENCY,
//!         TIMER_CMP_RESULT
//!     );
//!
//!     // Get current time
//!     Instant now = timer_now(&timer);
//!     uart_putdec(&uart, instant_micros(&now));
//!     uart_puts(&uart, " microseconds\r\n");
//!
//!     // Wait for 1 second
//!     Duration one_sec = duration_from_secs(1);
//!     timer_wait(&timer, one_sec);
//!
//!     // Or use low-level cycle API
//!     uint64_t freq = timer_frequency(&timer);
//!     uint64_t start = timer_now_cycles(&timer);
//!     timer_wait_cycles(&timer, freq);  // Wait for 1 second worth of cycles
//!     uint64_t end = timer_now_cycles(&timer);
//! }
//! ```
//!

#![no_std]
