// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use memmap_generate::build_utils::standard_memmap_build;
use std::path::PathBuf;

fn main() {
    // Generate memory.x linker script
    standard_memmap_build("VexRiscv.json", "DataMemory", "InstructionMemory");

    // Use C headers from auto-generated bittide-hal-c
    let hal_c_headers = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../../firmware-support/bittide-hal-c/generated");

    // Compile C code with clang (has built-in RISC-V support)
    cc::Build::new()
        .file("src/main.c")
        .compiler("clang")
        .include("src") // Add src directory for our custom stdint.h
        .include(hal_c_headers)
        .flag("--target=riscv32-unknown-none-elf") // RISC-V target
        .flag("-march=rv32imc")
        .flag("-mabi=ilp32")
        .flag("-Os") // Optimize for size
        .flag("-ffreestanding")
        .flag("-nostdlib")
        .compile("c_main");

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/main.c");
    println!("cargo:rerun-if-changed=src/stdint.h");
}
