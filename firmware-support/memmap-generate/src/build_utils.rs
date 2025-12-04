// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Common utilities for build.rs scripts

use std::env;
use std::fs;
use std::path::{Path, PathBuf};

use crate::memory_x_from_memmap;

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

    // Try to find git root from the manifest directory
    let output = std::process::Command::new("git")
        .args(["rev-parse", "--show-toplevel"])
        .current_dir(&manifest_dir)
        .output()
        .expect("Failed to execute git command - make sure git is installed and this is a git repository");

    if !output.status.success() {
        panic!(
            "Failed to find git root: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let git_root = String::from_utf8(output.stdout)
        .expect("Git output is not valid UTF-8")
        .trim()
        .to_string();

    PathBuf::from(git_root).join("_build").join("memory_maps")
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
