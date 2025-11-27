// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

pub mod backends;
pub mod input_language;

pub mod ir;
pub mod storage;

pub mod deprecated;

pub mod format;

pub mod build_utils;

use std::io::Write;

use input_language::{MemoryMapTree, PathComp};

pub use crate::input_language::{parse, MemoryMapDesc};

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
        "  IMEM : ORIGIN = 0x{:X}, LENGTH = 0x{:X}",
        instr_mem_address, instr_mem_size
    )
    .unwrap();
    writeln!(
        buf,
        "  DMEM : ORIGIN = 0x{:X}, LENGTH = 0x{:X}",
        data_mem_address, data_mem_size
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
