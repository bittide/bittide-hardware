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
//! Types are generated in `generated/types/` with names like:
//! - `callisto_config.h`
//! - `maybe.h`
//!
//! Devices that are shared between different hardware setups are generated
//! in the `generated/shared_devices` directory with names like:
//! - `uart.h`
//! - `freeze.h`
//! - etc.
//!
//! For each hardware setup there is a directory in `generated/hals`.
//! For each of those, a `device_instances.h` file exists, for example
//! `generated/hals/vex_riscv/device_instances.h`.
//!
//! Devices that are not shared between hardware setups are placed in the
//! "hardware setup directory" under a `devices` directory, like
//! `generated/hals/scatter_gather_pe/devices/scatter_calendar.h`
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
//!
//!     // Use C headers from auto-generated bittide-hal-c
//!     let hal_c_headers = bittide_hal_c::generated_dir();
//!
//!     // Include directory for the Bittide HAL
//!     let hal_c_include = bittide_hal_c::manual_include_dir();
//!
//!     // Source directory for the Bittide HAL
//!     let hal_c_src = bittide_hal_c::manual_source_dir();
//!
//!     // Compile C code with clang (has built-in RISC-V support)
//!     cc::Build::new()
//!         .file("src/main.c")
//!         .files(bittide_hal_c::manual_source_files())
//!         .compiler("clang")
//!         .include("src") // Add src directory for our custom stdint.h
//!         .include(&hal_c_headers)
//!         .include(&hal_c_include) // Add HAL include directory
//!         .flag("--target=riscv32-unknown-none-elf") // RISC-V target
//!         .flag("-march=rv32imc")
//!         .flag("-mabi=ilp32")
//!         .flag("-Os") // Optimize for size
//!         .flag("-ffreestanding")
//!         .flag("-nostdlib")
//!         .compile("c_main");
//!
//!     println!("cargo:rerun-if-changed=build.rs");
//!     println!("cargo:rerun-if-changed=src/main.c");
//!     println!("cargo:rerun-if-changed={}", hal_c_headers.display());
//!     println!("cargo:rerun-if-changed={}", hal_c_include.display());
//!     println!("cargo:rerun-if-changed={}", hal_c_src.display());
//!
//! }
//! ```
//!
//! Then in your C code:
//!
//! ```c
//! #include "hals/vex_riscv/device_instances.h"
//! #include "bittide_uart.h"
//! #include "bittide_timer.h"
//!
//! void main(void) {
//!     // Initialize UART
//!     Uart uart = hal.uart;
//!     uart_puts(uart, "Hello, World!\r\n");
//!
//!     // Initialize timer
//!     Timer timer = hal.timer;
//!
//!     // Get current time
//!     Instant now = timer_now(timer);
//!     uart_putdec(uart, instant_micros(&now));
//!     uart_puts(uart, " microseconds\r\n");
//!
//!     // Wait for 1 second
//!     Duration one_sec = duration_from_secs(1);
//!     timer_wait(timer, one_sec);
//! }
//! ```
//!

pub fn generated_dir() -> std::path::PathBuf {
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("generated")
}

pub fn manual_include_dir() -> std::path::PathBuf {
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("include")
}

pub fn manual_source_dir() -> std::path::PathBuf {
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("c_src")
}

pub fn manual_source_files() -> impl Iterator<Item = std::path::PathBuf> {
    manual_source_dir()
        .read_dir()
        .expect("cannot open `c_src`")
        .filter_map(|entry| entry.ok())
        .filter_map(|entry| {
            let path = entry.path();
            let ext = path.extension()?.to_str()?;
            if ext == "c" {
                Some(path)
            } else {
                None
            }
        })
}
