// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Bittide Hardware Abstraction Layer for C
//!
//! This crate provides auto-generated C header files for all Bittide hardware
//! memory maps, plus a hardware abstraction layer in `include/bittide_hal.h`.
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
//! The HAL is organized into separate header files:
//!
//! - **`bittide_hal.h`**: Main header that includes all HAL components
//! - **`bittide_timer.h`**: Timer, Duration, and Instant types and functions
//! - **`bittide_uart.h`**: UART helper functions
//!
//! ### Timer API (`bittide_timer.h`)
//!
//! - `Duration`: Relative time representation (microseconds)
//! - `Instant`: Absolute time representation (microseconds)
//! - `Timer`: Timer device with memory-mapped registers
//! - Functions: `timer_now()`, `timer_wait()`, `timer_wait_until()`, etc.
//!
//! ### UART Helpers (`bittide_uart.h`)
//!
//! - `uart_putc()`, `uart_puts()`, `uart_getc()`
//! - `uart_puthex32()`, `uart_puthex64()`, `uart_putdec()`
//!
//! ## Usage from C Firmware
//!
//! To use these headers in your C firmware build.rs:
//!
//! ```rust,no_run
//! # #[allow(clippy::needless_doctest_main)]
//! use std::env;
//! use std::path::PathBuf;
//!
//! fn main() {
//!     // Add the bittide-hal-c generated directory to the include path
//!     let hal_c_generated = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
//!         .join("../../firmware-support/bittide-hal-c/generated");
//!
//!     // Add the bittide-hal-c include directory for the HAL
//!     let hal_c_include = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
//!         .join("../../firmware-support/bittide-hal-c/include");
//!
//!     cc::Build::new()
//!         .file("src/main.c")
//!         .include(hal_c_generated)
//!         .include(hal_c_include)
//!         .compile("my_firmware");
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
//!
//!     // Wait for 1 second
//!     Duration one_sec = duration_from_secs(1);
//!     timer_wait(&timer, one_sec);
//!
//!     // Use UART
//!     uart_puts(UART_DATA, UART_STATUS, "Hello, World!\r\n");
//! }
//! ```
//!

#![no_std]
