// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use memmap_generate::build_utils::standard_memmap_build;
use std::path::PathBuf;

fn main() {
    // Generate memory.x linker script
    standard_memmap_build("ScatterGatherPe.json", "DataMemory", "InstructionMemory");

    // Use C headers from auto-generated bittide-hal-c
    let hal_c_headers = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../../firmware-support/bittide-hal-c/generated");

    // Include directory for the Bittide HAL
    let hal_c_include = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../../firmware-support/bittide-hal-c/include");

    // Source directory for the Bittide HAL
    let hal_c_src = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../../firmware-support/bittide-hal-c/src");

    // Compile C code with clang (has built-in RISC-V support)
    cc::Build::new()
        .file("src/main.c")
        .file(hal_c_src.join("bittide_uart.c"))
        .file(hal_c_src.join("bittide_scatter.c"))
        .file(hal_c_src.join("bittide_gather.c"))
        .compiler("clang")
        .include(hal_c_headers)
        .include(hal_c_include) // Add HAL include directory
        .flag("--target=riscv32-unknown-none-elf") // RISC-V target
        .flag("-march=rv32imc")
        .flag("-mabi=ilp32")
        .flag("-Os") // Optimize for size
        .flag("-ffreestanding")
        .flag("-nostdlib")
        .compile("c_main");

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/main.c");
    println!(
        "cargo:rerun-if-changed=../../../firmware-support/bittide-hal-c/include/bittide_uart.h"
    );
    println!(
        "cargo:rerun-if-changed=../../../firmware-support/bittide-hal-c/include/bittide_scatter.h"
    );
    println!(
        "cargo:rerun-if-changed=../../../firmware-support/bittide-hal-c/include/bittide_gather.h"
    );
    println!("cargo:rerun-if-changed=../../../firmware-support/bittide-hal-c/src/bittide_uart.c");
    println!(
        "cargo:rerun-if-changed=../../../firmware-support/bittide-hal-c/src/bittide_scatter.c"
    );
    println!("cargo:rerun-if-changed=../../../firmware-support/bittide-hal-c/src/bittide_gather.c");
}
