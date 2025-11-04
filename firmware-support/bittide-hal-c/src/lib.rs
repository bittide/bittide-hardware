// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Bittide Hardware Abstraction Layer for C
//!
//! This crate provides auto-generated C header files for all Bittide hardware
//! memory maps. The headers are generated at build time from the JSON memory map
//! files in `_build/memory_maps/`.
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
//!     let hal_c_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
//!         .join("../../firmware-support/bittide-hal-c/generated");
//!
//!     cc::Build::new()
//!         .file("src/main.c")
//!         .include(hal_c_dir)
//!         .compile("my_firmware");
//! }
//! ```
//!
//! Then in your C code:
//!
//! ```c
//! #include "vexriscv_memmap.h"
//!
//! void uart_putc(char c) {
//!     while (*UART_STATUS & 0x01) { /* wait */ }
//!     *UART_DATA = c;
//! }
//! ```
//!

#![no_std]
