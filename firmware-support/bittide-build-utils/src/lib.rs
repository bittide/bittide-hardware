// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::env;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};

use memorymap_compiler::MemoryMapDesc;
use memorymap_compiler::input_language::MemoryMapTree;
use memorymap_compiler::input_language::PathComp;
use memorymap_compiler::parse;

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
        fs::read(source_file).unwrap_or_else(|_| panic!("Could not read file: {source_file}"));

    let out_dir = env::var("OUT_DIR").expect("No out dir");
    let dest_path = Path::new(&out_dir).join("memory.x");
    fs::write(dest_path, memory_x_content).expect("Could not write file");

    setup_riscv_linker(&out_dir);
    println!("cargo:rerun-if-changed={source_file}");
}
/// Generate a `memory.x` file from a memory map.
///
/// This takes the path to a memory map JSON file and the names of
/// the data and instruction memory devices or device instances.
///
/// # Panics
///
/// This function panics whenever something unexpected happens.
/// Since this function is meant to only be called from build scripts, this
/// makes the code simpler as errors probably wouldn't be handled anyway.
pub fn memory_x_from_memmap(
    path: impl AsRef<std::path::Path>,
    data_device_name: &str,
    instr_device_name: &str,
) -> Vec<u8> {
    let memmap_source =
        std::fs::read_to_string(path).expect("Memory map file could not be read to string");
    let memmap = parse(&memmap_source).expect("Memory map could not be parsed");
    let imem = device_singleton_instr_addr_and_size(&memmap, instr_device_name).unwrap();
    let dmem = device_singleton_instr_addr_and_size(&memmap, data_device_name).unwrap();

    memory_x_file(dmem, imem)
}

fn memory_x_file(
    (data_mem_address, data_mem_size): (u64, u64),
    (instr_mem_address, instr_mem_size): (u64, u64),
) -> Vec<u8> {
    let mut buf = Vec::<u8>::new();
    writeln!(buf, "MEMORY").unwrap();
    writeln!(buf, "{{").unwrap();
    writeln!(
        buf,
        "  IMEM : ORIGIN = 0x{instr_mem_address:X}, LENGTH = 0x{instr_mem_size:X}",
    )
    .unwrap();
    writeln!(
        buf,
        "  DMEM : ORIGIN = 0x{data_mem_address:X}, LENGTH = 0x{data_mem_size:X}",
    )
    .unwrap();
    writeln!(
        buf,
        r#"
}}

REGION_ALIAS("REGION_TEXT", IMEM);
REGION_ALIAS("REGION_RODATA", DMEM);
REGION_ALIAS("REGION_DATA", DMEM);
REGION_ALIAS("REGION_BSS", DMEM);
REGION_ALIAS("REGION_HEAP", DMEM);
REGION_ALIAS("REGION_STACK", DMEM);

"#
    )
    .unwrap();
    buf
}
fn device_singleton_instr_addr_and_size(desc: &MemoryMapDesc, name: &str) -> Option<(u64, u64)> {
    fn find_instance_or_device<'tree>(
        tree: &'tree MemoryMapTree,
        name: &str,
    ) -> Option<(u64, &'tree str)> {
        match tree {
            MemoryMapTree::Interconnect { components, .. } => {
                for comp in components {
                    if let Some(t) = find_instance_or_device(&comp.tree, name) {
                        return Some(t);
                    }
                }
                None
            }
            MemoryMapTree::DeviceInstance {
                path,
                device_name,
                absolute_address,
                ..
            } => {
                if device_name == name {
                    Some((*absolute_address, device_name))
                } else {
                    match path.last() {
                        Some(PathComp::Name {
                            name: path_name, ..
                        }) if path_name == name => Some((*absolute_address, device_name)),
                        _ => None,
                    }
                }
            }
        }
    }

    let (abs, device_name) = find_instance_or_device(&desc.tree, name)?;
    let device = desc.devices.get(device_name)?;
    let size = device
        .registers
        .iter()
        .map(|reg| reg.address + reg.size)
        .max()?;

    Some((abs, size))
}
