// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Generate C header files with device and register addresses from memory maps.
//!
//! This module generates `.h` files containing `#define` macros for:
//! - Device base addresses
//! - Register offsets within each device
//! - Volatile pointer macros for direct memory access

use crate::{
    deprecated::hal_set::DeviceInstance,
    input_language::{
        path_name, DeviceDesc, MemoryMapDesc, MemoryMapTree, RegisterAccess, TypeRef,
    },
};
use heck::ToShoutySnakeCase;
use std::collections::BTreeMap;
use std::fmt::Write;

/// Generate a C header file from a memory map description.
///
/// Returns the content of the header file as a String.
#[must_use]
pub fn generate_c_header(memmap: &MemoryMapDesc, header_name: &str) -> String {
    let mut output = String::new();

    // Header guard
    let guard = format!("{}_H", header_name.to_shouty_snake_case());
    write!(
        output,
        "#ifndef {guard}\n\
         #define {guard}\n\n\
         #include \"stdint.h\"\n\n\
         /* Device base addresses from {header_name} memory map */\n\n",
    )
    .unwrap();

    // Collect all device instances from the tree
    let instances = collect_device_instances(&memmap.tree);

    // Group instances by device type
    let mut devices_by_type: BTreeMap<&str, Vec<&DeviceInstance>> = BTreeMap::new();
    for instance in &instances {
        devices_by_type
            .entry(&instance.device_name)
            .or_default()
            .push(instance);
    }

    // Generate defines for each device type
    for (device_name, instances) in devices_by_type {
        let Some(device) = memmap.devices.get(device_name) else {
            continue;
        };

        // Skip devices tagged with "no-generate"
        if device.tags.iter().any(|t| t == "no-generate") {
            continue;
        }

        writeln!(output, "/* {device_name}: {} */", device.description).unwrap();

        // Determine if we need to number unnamed instances
        let has_multiple_unnamed = instances
            .iter()
            .filter(|inst| path_name(&inst.path).is_none())
            .count()
            > 1;

        // Generate base address for each instance
        let mut unnamed_idx = 0;
        for instance in &instances {
            let instance_name = get_instance_name_with_index(
                instance,
                device_name,
                if has_multiple_unnamed {
                    Some(&mut unnamed_idx)
                } else {
                    None
                },
            );
            writeln!(
                output,
                "#define {}_BASE           0x{:08X}UL",
                instance_name.to_shouty_snake_case(),
                instance.absolute_address
            )
            .unwrap();
        }

        output.push('\n');

        // Generate register defines for each instance
        let mut unnamed_idx = 0;
        for instance in &instances {
            let instance_name = get_instance_name_with_index(
                instance,
                device_name,
                if has_multiple_unnamed {
                    Some(&mut unnamed_idx)
                } else {
                    None
                },
            );
            let base_name = instance_name.to_shouty_snake_case();

            generate_register_defines(&mut output, device, &base_name);
        }

        output.push('\n');
    }

    // Add header guard close
    writeln!(output, "#endif /* {guard} */").unwrap();

    output
}

/// Recursively collect all device instances from the memory map tree.
fn collect_device_instances(tree: &MemoryMapTree) -> Vec<DeviceInstance> {
    let mut instances = Vec::new();

    match tree {
        MemoryMapTree::Interconnect { components, .. } => {
            for component in components {
                instances.extend(collect_device_instances(&component.tree));
            }
        }
        MemoryMapTree::DeviceInstance {
            path,
            device_name,
            absolute_address,
            tags,
            src_location,
            ..
        } => {
            instances.push(DeviceInstance {
                path: path.clone(),
                device_name: device_name.clone(),
                absolute_address: *absolute_address,
                tags: tags.clone(),
                src_location: src_location.clone(),
            });
        }
    }

    instances
}

/// Get a readable name for a device instance from its path.
/// If `unnamed_idx` is Some and the instance has no name, append an index.
fn get_instance_name_with_index(
    instance: &DeviceInstance,
    device_name: &str,
    unnamed_idx: Option<&mut usize>,
) -> String {
    if let Some((_loc, name)) = path_name(&instance.path) {
        name.to_string()
    } else if let Some(idx) = unnamed_idx {
        let current = *idx;
        *idx += 1;
        format!("{device_name}_{current}")
    } else {
        device_name.to_string()
    }
}

/// Format a Type as a human-readable string for documentation.
fn format_type(ty: &TypeRef) -> String {
    match ty {
        TypeRef::TypeReference { base, args } => match (base.base.as_str(), base.module.as_str()) {
            ("BitVector", "Clash.Sized.Internal.BitVector") => {
                format!("BitVector({})", format_type(&args[0]))
            }
            ("Signed", "Clash.Sized.Internal.Signed") => {
                format!("Signed({})", format_type(&args[0]))
            }
            ("Unsigned", "Clash.Sized.Internal.Unsigned") => {
                format!("Unsigned({})", format_type(&args[0]))
            }
            ("Index", "Clash.Sized.Internal.Index") => {
                format!("Index({})", format_type(&args[0]))
            }
            ("Float", "GHC.Types") => "float".to_string(),
            ("Double", "GHC.Types") => "double".to_string(),
            ("Bool", "GHC.Types") => "bool".to_string(),
            ("Vec", "Clash.Sized.Vector") => {
                format!("Vec({}, {})", format_type(&args[0]), format_type(&args[1]))
            }
            ("Unit", "GHC.Tuple") => "unit".to_string(),
            (name, "GHC.Tuple") if name.starts_with("Tuple") => {
                let inner = args.iter().map(format_type).collect::<Vec<_>>().join(", ");
                format!("({inner})")
            }
            (_, _) => {
                if args.is_empty() {
                    base.base.clone()
                } else {
                    format!(
                        "{}<{}>",
                        base.base,
                        args.iter().map(format_type).collect::<Vec<_>>().join(", ")
                    )
                }
            }
        },
        TypeRef::Variable(name) => name.clone(),
        TypeRef::Nat(n) => format!("{n}"),
        TypeRef::Tuple(type_refs) => {
            let inner = type_refs
                .iter()
                .map(format_type)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({inner})")
        }
    }
}

/// Generate register offset defines and pointer macros for a device.
fn generate_register_defines(output: &mut String, device: &DeviceDesc, base_name: &str) {
    for register in &device.registers {
        // Skip registers with "no-generate" tag
        if register.tags.iter().any(|t| t == "no-generate") {
            continue;
        }

        let reg_name = format!("{base_name}_{}", register.name.to_shouty_snake_case());

        // Generate documentation comment
        output.push_str("/**\n");
        writeln!(output, " * Name: {}", register.name).unwrap();
        if !register.description.is_empty() {
            writeln!(output, " * Description: {}", register.description).unwrap();
        }

        // Add type information
        writeln!(output, " * Type: {}", format_type(&register.reg_type)).unwrap();

        // Add access information
        let access_str = match register.access {
            RegisterAccess::ReadWrite => "read-write",
            RegisterAccess::ReadOnly => "read-only",
            RegisterAccess::WriteOnly => "write-only",
        };
        writeln!(output, " * Access: {access_str}").unwrap();

        // Add size information
        writeln!(
            output,
            " * Size: {} byte{}",
            register.size,
            if register.size == 1 { "" } else { "s" }
        )
        .unwrap();
        output.push_str(" */\n");

        // Determine the C type based on register size
        let c_type = match register.size {
            1 => "uint8_t",
            2 => "uint16_t",
            4 => "uint32_t",
            8 => "uint64_t",
            _ => {
                // For larger registers, use uint8_t array
                write!(
                    output,
                    "#define {reg_name}           ((volatile uint8_t*)({base_name}_BASE + 0x{:02X}))\n\n",
                    register.address
                )
                .unwrap();
                continue;
            }
        };

        // Generate volatile pointer macro
        write!(
            output,
            "#define {reg_name}           ((volatile {c_type}*)({base_name}_BASE + 0x{:02X}))\n\n",
            register.address
        )
        .unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_header_generation() {
        let json = r#"{
            "devices": {
                "UART": {
                    "name": "UART",
                    "description": "Universal Asynchronous Receiver/Transmitter",
                    "registers": [
                        {
                            "name": "data",
                            "description": "Data register",
                            "access": "read_write",
                            "address": 0,
                            "size": 1,
                            "type": ["bitvector", 8],
                            "src_location": 0,
                            "tags": []
                        },
                        {
                            "name": "status",
                            "description": "Status register",
                            "access": "read_only",
                            "address": 4,
                            "size": 1,
                            "type": ["bitvector", 8],
                            "src_location": 0,
                            "tags": []
                        }
                    ],
                    "src_location": 0,
                    "tags": []
                }
            },
            "types": {},
            "tree": {
                "interconnect": {
                    "path": [],
                    "tags": [],
                    "absolute_address": 0,
                    "components": [
                        {
                            "relative_address": 1610612736,
                            "tree": {
                                "device_instance": {
                                    "path": [{"name": "uart", "src_location": 0}],
                                    "tags": [],
                                    "device_name": "UART",
                                    "src_location": 0,
                                    "absolute_address": 1610612736
                                }
                            }
                        }
                    ],
                    "src_location": 0
                }
            },
            "src_locations": []
        }"#;

        let memmap: MemoryMapDesc = serde_json::from_str(json).unwrap();
        let header = generate_c_header(&memmap, "test");

        assert!(header.contains("#ifndef TEST_H"));
        assert!(header.contains("#define TEST_H"));
        assert!(header.contains("#define UART_BASE"));
        assert!(header.contains("0x60000000UL"));
        assert!(header.contains("#define UART_DATA"));
        assert!(header.contains("#define UART_STATUS"));
        assert!(header.contains("volatile uint8_t*"));
    }
}
