// SPDX-FileCopyrightText: 2026 Google LLC
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

/// RISC-V instruction extensions that can be checked
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RiscvExtension {
    /// Compressed instructions (C extension)
    C,
    /// Base integer instructions (I extension)
    I,
    /// Integer multiply/divide (MUL, DIV, REM, etc.)
    M,
    /// Atomics (LR/SC/AMO)
    A,
    /// Single-precision floating-point
    F,
    /// Double-precision floating-point
    D,
    /// Quad-precision floating-point
    Q,
    /// Zicsr standard extension
    Zicsr,
    /// Zifencei standard extension
    Zifencei,
    /// Zawrs standard extension
    Zawrs,
    /// Zfh standard extension
    Zfh,
    /// Zba standard extension
    Zba,
    /// Zbb standard extension
    Zbb,
    /// Zbc standard extension
    Zbc,
    /// Zbkb standard extension
    Zbkb,
    /// Zbs standard extension
    Zbs,
}

/// Forbid specific RISC-V instruction extensions in a compiled binary.
///
/// This function should be called from build.rs scripts for firmware binaries
/// that target CPUs without certain instruction extensions.
///
/// # Arguments
/// * `profile` - Build profile ("debug" or "release")
/// * `forbidden_extensions` - Slice of extensions that should NOT appear in the binary
///
/// # Panics
/// Panics with a detailed error message if forbidden instructions are found.
///
pub fn forbid_riscv_instructions(profile: &str, forbidden_extensions: &[RiscvExtension]) {
    // Only check RISC-V targets
    if env::var("CARGO_CFG_TARGET_ARCH").unwrap() != "riscv32" {
        return;
    }

    // Get the binary path
    let package_name = env::var("CARGO_PKG_NAME").expect("CARGO_PKG_NAME not set");

    // Use the actual target directory from environment or find it via git root
    let target_dir = if let Ok(dir) = env::var("CARGO_TARGET_DIR") {
        PathBuf::from(dir)
    } else {
        // Find git root and use _build/cargo/firmware-binaries
        let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
        let output = std::process::Command::new("git")
            .args(["rev-parse", "--show-toplevel"])
            .current_dir(&manifest_dir)
            .output()
            .expect("Failed to execute git command");

        if !output.status.success() {
            panic!("Failed to find git root");
        }

        let git_root = String::from_utf8(output.stdout)
            .expect("Git output is not valid UTF-8")
            .trim()
            .to_string();

        PathBuf::from(git_root)
            .join("_build")
            .join("cargo")
            .join("firmware-binaries")
    };

    let binary_path = target_dir
        .join("riscv32imc-unknown-none-elf")
        .join(profile)
        .join(&package_name);

    // Binary might not exist yet during first build
    if !binary_path.exists() {
        println!(
            "cargo:warning=Binary not found at {}, skipping instruction check",
            binary_path.display()
        );
        return;
    }

    // Read and check the binary
    let binary_data = fs::read(&binary_path)
        .unwrap_or_else(|e| panic!("Failed to read binary {}: {}", binary_path.display(), e));

    let violations = check_elf_instructions(&binary_data, forbidden_extensions);

    if !violations.is_empty() {
        let extension_names: Vec<_> = forbidden_extensions
            .iter()
            .map(|ext| format!("{:?}", ext))
            .collect();

        panic!(
            "\n\nFORBIDDEN RISC-V INSTRUCTIONS DETECTED\n\
            Binary: {}\n\
            Forbidden extensions: {}\n\
            Found {} violations\n\n\
            {}\n\
            This CPU does not support these instruction extensions.\n\
            Please review the code to avoid using these instructions.\n",
            package_name,
            extension_names.join(", "),
            violations.len(),
            format_violations(&violations, 5)
        );
    }
}

/// Check a compiled RISC-V binary for expected instruction extensions.
///
/// This function verifies that a binary DOES contain instructions from the
/// specified extensions, which is useful for ensuring optimizations are working.
///
/// # Arguments
/// * `profile` - Build profile ("debug" or "release")
/// * `expected_extensions` - Slice of extensions that SHOULD appear in the binary
///
/// # Panics
/// Panics with a warning message if expected instructions are NOT found.
///
pub fn require_riscv_instructions(profile: &str, expected_extensions: &[RiscvExtension]) {
    // Only check RISC-V targets
    if env::var("CARGO_CFG_TARGET_ARCH").unwrap() != "riscv32" {
        return;
    }

    // Get the binary path (same logic as check_riscv_instructions)
    let package_name = env::var("CARGO_PKG_NAME").expect("CARGO_PKG_NAME not set");

    let target_dir = if let Ok(dir) = env::var("CARGO_TARGET_DIR") {
        PathBuf::from(dir)
    } else {
        let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
        let output = std::process::Command::new("git")
            .args(["rev-parse", "--show-toplevel"])
            .current_dir(&manifest_dir)
            .output()
            .expect("Failed to execute git command");

        if !output.status.success() {
            panic!("Failed to find git root");
        }

        let git_root = String::from_utf8(output.stdout)
            .expect("Git output is not valid UTF-8")
            .trim()
            .to_string();

        PathBuf::from(git_root)
            .join("_build")
            .join("cargo")
            .join("firmware-binaries")
    };

    let binary_path = target_dir
        .join("riscv32imc-unknown-none-elf")
        .join(profile)
        .join(&package_name);

    // Binary might not exist yet during first build
    if !binary_path.exists() {
        println!(
            "cargo:warning=Binary not found at {}, skipping instruction check",
            binary_path.display()
        );
        return;
    }

    // Read and check the binary
    let binary_data = fs::read(&binary_path)
        .unwrap_or_else(|e| panic!("Failed to read binary {}: {}", binary_path.display(), e));

    let found_extensions = find_extensions_in_elf(&binary_data);

    // Check which expected extensions are missing
    let missing: Vec<_> = expected_extensions
        .iter()
        .filter(|ext| !found_extensions.contains(ext))
        .collect();

    if !missing.is_empty() {
        let extension_names: Vec<_> = missing.iter().map(|ext| format!("{:?}", ext)).collect();

        println!(
            "cargo:warning=\nEXPECTED RISC-V INSTRUCTIONS NOT FOUND\n\
            Binary: {}\n\
            Expected extensions: {}\n\
            Missing: {}\n\n\
            This CPU supports these extensions but they are not being used.\n\
            This may indicate missed optimization opportunities.",
            package_name,
            expected_extensions
                .iter()
                .map(|e| format!("{:?}", e))
                .collect::<Vec<_>>()
                .join(", "),
            extension_names.join(", ")
        );
    }
}

/// Find which extensions are present in an ELF binary
fn find_extensions_in_elf(data: &[u8]) -> Vec<RiscvExtension> {
    let mut found = Vec::new();
    let mut has_c = false;
    let mut has_i = false;
    let mut has_m = false;
    let mut has_a = false;
    let mut has_f = false;
    let mut has_d = false;
    let mut has_q = false;
    let mut has_zicsr = false;
    let mut has_zifencei = false;
    let mut has_zawrs = false;
    let mut has_zfh = false;
    let mut has_zba = false;
    let mut has_zbb = false;
    let mut has_zbc = false;
    let mut has_zbkb = false;
    let mut has_zbs = false;

    for section in executable_sections(data) {
        let mut i = section.offset as usize;
        let end = (i + section.size as usize).min(data.len());

        while i + 2 <= end && i + 2 <= data.len() {
            let (ext, len) = crate::riscv_decode::classify_instruction(&data[i..end]);

            if let Some(ext) = ext {
                match ext {
                    RiscvExtension::C if !has_c => {
                        has_c = true;
                        found.push(RiscvExtension::C);
                    }
                    RiscvExtension::I if !has_i => {
                        has_i = true;
                        found.push(RiscvExtension::I);
                    }
                    RiscvExtension::M if !has_m => {
                        has_m = true;
                        found.push(RiscvExtension::M);
                    }
                    RiscvExtension::A if !has_a => {
                        has_a = true;
                        found.push(RiscvExtension::A);
                    }
                    RiscvExtension::F if !has_f => {
                        has_f = true;
                        found.push(RiscvExtension::F);
                    }
                    RiscvExtension::D if !has_d => {
                        has_d = true;
                        found.push(RiscvExtension::D);
                    }
                    RiscvExtension::Q if !has_q => {
                        has_q = true;
                        found.push(RiscvExtension::Q);
                    }
                    RiscvExtension::Zicsr if !has_zicsr => {
                        has_zicsr = true;
                        found.push(RiscvExtension::Zicsr);
                    }
                    RiscvExtension::Zifencei if !has_zifencei => {
                        has_zifencei = true;
                        found.push(RiscvExtension::Zifencei);
                    }
                    RiscvExtension::Zawrs if !has_zawrs => {
                        has_zawrs = true;
                        found.push(RiscvExtension::Zawrs);
                    }
                    RiscvExtension::Zfh if !has_zfh => {
                        has_zfh = true;
                        found.push(RiscvExtension::Zfh);
                    }
                    RiscvExtension::Zba if !has_zba => {
                        has_zba = true;
                        found.push(RiscvExtension::Zba);
                    }
                    RiscvExtension::Zbb if !has_zbb => {
                        has_zbb = true;
                        found.push(RiscvExtension::Zbb);
                    }
                    RiscvExtension::Zbc if !has_zbc => {
                        has_zbc = true;
                        found.push(RiscvExtension::Zbc);
                    }
                    RiscvExtension::Zbkb if !has_zbkb => {
                        has_zbkb = true;
                        found.push(RiscvExtension::Zbkb);
                    }
                    RiscvExtension::Zbs if !has_zbs => {
                        has_zbs = true;
                        found.push(RiscvExtension::Zbs);
                    }
                    _ => {}
                }
            }

            i += len;
        }
    }

    found
}

/// Format violation messages for display
fn format_violations(violations: &[(u64, u32, RiscvExtension)], max_show: usize) -> String {
    let mut output = String::new();
    for (addr, instr, ext) in violations.iter().take(max_show) {
        output.push_str(&format!(
            "  0x{:08x}: 0x{:08x}  [{:?} extension]\n",
            addr, instr, ext
        ));
    }
    if violations.len() > max_show {
        output.push_str(&format!("  ... and {} more\n", violations.len() - max_show));
    }
    output
}

/// Check ELF binary data for forbidden instructions
fn check_elf_instructions(
    data: &[u8],
    forbidden: &[RiscvExtension],
) -> Vec<(u64, u32, RiscvExtension)> {
    let mut violations = Vec::new();

    for section in executable_sections(data) {
        let mut i = section.offset as usize;
        let end = (i + section.size as usize).min(data.len());

        while i + 2 <= end && i + 2 <= data.len() {
            let (ext, len) = crate::riscv_decode::classify_instruction(&data[i..end]);

            if let Some(ext) = ext {
                if forbidden.contains(&ext) {
                    let instr = if len == 4 && i + 4 <= data.len() {
                        u32::from_le_bytes([data[i], data[i + 1], data[i + 2], data[i + 3]])
                    } else {
                        u32::from(u16::from_le_bytes([data[i], data[i + 1]]))
                    };
                    let addr = section.addr + (i as u64).saturating_sub(section.offset);
                    violations.push((addr, instr, ext));
                }
            }

            i += len;
        }
    }

    violations
}

struct ExecSection {
    offset: u64,
    size: u64,
    addr: u64,
}

fn executable_sections(data: &[u8]) -> Vec<ExecSection> {
    use goblin::elf::section_header::SHF_EXECINSTR;

    let elf = goblin::elf::Elf::parse(data)
        .unwrap_or_else(|err| panic!("Failed to parse ELF for instruction scan: {}", err));

    elf.section_headers
        .iter()
        .filter(|shdr| shdr.sh_flags & SHF_EXECINSTR as u64 != 0)
        .filter(|shdr| shdr.sh_size > 0)
        .map(|shdr| ExecSection {
            offset: shdr.sh_offset,
            size: shdr.sh_size,
            addr: shdr.sh_addr,
        })
        .collect()
}
