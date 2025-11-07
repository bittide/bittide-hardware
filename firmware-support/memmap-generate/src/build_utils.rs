// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Common utilities for build.rs scripts

use std::env;
use std::fs;
use std::path::{Path, PathBuf};

use crate::{generate_c_header, memory_x_from_memmap, parse};

/// Standard build script setup for RISC-V targets.
///
/// This function handles:
/// - Setting up cargo link arguments for RISC-V
/// - Setting up the linker search path
fn setup_riscv_linker(out_dir: &str) {
    if env::var("CARGO_CFG_TARGET_ARCH").unwrap() == "riscv32" {
        println!("cargo:rustc-link-arg=-Tmemory.x");
        println!("cargo:rustc-link-arg=-Tlink.x"); // linker script from riscv-rt
    }
    println!("cargo:rustc-link-search={out_dir}");
}

/// Get the path to the memory maps directory using git to find the project root.
pub fn memmap_dir() -> PathBuf {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    gix::ThreadSafeRepository::discover(&manifest_dir)
        .expect("Failed to find `.git`.")
        .work_dir()
        .expect("Failed to find repository working directory")
        .join("_build")
        .join("memory_maps")
}

/// Generates a memory.x file from a JSON memory map file and set up the linker
/// configuration for RISC-V targets.
///
/// # Arguments
/// * `memmap_json_name` - Name of the JSON file in the memory maps directory (e.g., "VexRiscv.json")
/// * `data_device_name` - Name of the data memory device (usually "DataMemory")
/// * `instr_device_name` - Name of the instruction memory device (usually "InstructionMemory")
pub fn standard_memmap_build(
    memmap_json_name: &str,
    data_device_name: &str,
    instr_device_name: &str,
) {
    let memmap_path = memmap_dir().join(memmap_json_name);
    let memory_x = memory_x_from_memmap(&memmap_path, data_device_name, instr_device_name);

    let out_dir = env::var("OUT_DIR").expect("No out dir");
    let dest_path = Path::new(&out_dir).join("memory.x");
    fs::write(dest_path, memory_x).expect("Could not write file");

    setup_riscv_linker(&out_dir);
    println!("cargo:rerun-if-changed={}", memmap_path.display());
}

/// Reads an existing memory.x file and copies it to the output directory, then
/// sets up the linker configuration for RISC-V targets.
///
/// # Arguments
/// * `source_file` - The name of the source file to read (e.g., "memory.x")
pub fn standard_static_memory_build(source_file: &str) {
    let memory_x_content =
        fs::read(source_file).unwrap_or_else(|_| panic!("Could not read file: {}", source_file));

    let out_dir = env::var("OUT_DIR").expect("No out dir");
    let dest_path = Path::new(&out_dir).join("memory.x");
    fs::write(dest_path, memory_x_content).expect("Could not write file");

    setup_riscv_linker(&out_dir);
    println!("cargo:rerun-if-changed={}", source_file);
}

#[allow(clippy::needless_doctest_main)]
/// Generates a C header file from a JSON memory map file.
///
/// The header file will contain `#define` macros for device base addresses
/// and register offsets, suitable for use in C firmware code.
///
/// # Arguments
/// * `memmap_json_name` - Name of the JSON file in the memory maps directory (e.g., `"VexRiscv.json"`)
/// * `output_path` - Path where the header file should be written (e.g., `"src/memmap.h"`)
/// * `header_name` - Name to use for the header guard (e.g., `"vexriscv_memmap"`)
///
/// # Example
/// ```no_run
/// use memmap_generate::build_utils::generate_c_header_from_memmap;
///
/// fn main() {
///     generate_c_header_from_memmap("VexRiscv.json", "src/memmap.h", "vexriscv_memmap");
/// }
/// ```
pub fn generate_c_header_from_memmap(
    memmap_json_name: &str,
    output_path: impl AsRef<Path>,
    header_name: &str,
) {
    let memmap_path = memmap_dir().join(memmap_json_name);
    let memmap_source = fs::read_to_string(&memmap_path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {e}", memmap_path.display()));

    let memmap =
        parse(&memmap_source).unwrap_or_else(|e| panic!("Failed to parse memory map: {e}"));

    let header_content = generate_c_header(&memmap, header_name);

    fs::write(output_path.as_ref(), header_content)
        .unwrap_or_else(|e| panic!("Failed to write header file: {e}"));

    println!("cargo:rerun-if-changed={}", memmap_path.display());
}
