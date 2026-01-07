// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use memmap_generate::build_utils::standard_memmap_build;

fn main() {
    standard_memmap_build("SoftUgnDemoGppe.json", "DataMemory", "InstructionMemory");
    // Use C headers from auto-generated bittide-hal-c
    let hal_c_headers = bittide_hal_c::generated_dir();

    // Include directory for the Bittide HAL
    let hal_c_include = bittide_hal_c::manual_include_dir();

    // Source directory for the Bittide HAL
    let hal_c_src = bittide_hal_c::manual_source_dir();

    // Compile C code with clang (has built-in RISC-V support)
    cc::Build::new()
        .file("src/main.c")
        .file("src/ringbuffer_align.c")
        .files(bittide_hal_c::manual_source_files())
        .compiler("clang")
        .include("src") // Add src directory for our custom stdint.h
        .include(&hal_c_headers)
        .include(&hal_c_include) // Add HAL include directory
        .flag("--target=riscv32-unknown-none-elf") // RISC-V target
        .flag("-march=rv32imc")
        .flag("-mabi=ilp32")
        .flag("-O2") // Optimize for size
        .flag("-ffreestanding")
        .flag("-nostdlib")
        .compile("c_main");

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src");
    println!("cargo:rerun-if-changed={}", hal_c_headers.display());
    println!("cargo:rerun-if-changed={}", hal_c_include.display());
    println!("cargo:rerun-if-changed={}", hal_c_src.display());
}
