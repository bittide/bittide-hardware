// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use memmap_generate::build_utils::standard_memmap_build;

fn main() {
    // Generate memory.x linker script
    standard_memmap_build("VexRiscv.json", "DataMemory", "InstructionMemory");

    // Compile C code with clang (has built-in RISC-V support)
    cc::Build::new()
        .file("src/main.c")
        .compiler("clang")
        .include("src") // Add src directory for our custom stdint.h
        .flag("--target=riscv32-unknown-none-elf") // RISC-V target
        .flag("-march=rv32imc")
        .flag("-mabi=ilp32")
        .flag("-Os") // Optimize for size
        .flag("-ffreestanding")
        .flag("-nostdlib")
        .compile("c_main");

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/main.c");
    println!("cargo:rerun-if-changed=src/memmap.h");
    println!("cargo:rerun-if-changed=src/stdint.h");
}
