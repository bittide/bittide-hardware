// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Deduplicate device definition across multiple memory maps.

use std::collections::{BTreeMap, HashMap};

use crate::{
    ir::{
        input_to_ir::IrInputMapping,
        types::{
            DeviceDescription, HalHandles, IrCtx, TreeElem, TypeDescription, TypeName, TypeRef,
        },
    },
    storage::{Handle, HandleRange},
};

/// Handles for types and devices that are shared across a set of memory maps.
#[derive(Debug)]
pub struct HalShared {
    pub deduped_types: Vec<Handle<TypeDescription>>,
    pub type_mapping: HashMap<Handle<TypeDescription>, Handle<TypeDescription>>,

    pub deduped_devices: Vec<Handle<DeviceDescription>>,
    pub device_mappings: HashMap<Handle<DeviceDescription>, Handle<DeviceDescription>>,
}

/// Handles for a HAL/memory map that are not shared across other memory maps.
///
/// Elements that are listed here are considered "unique" across all memory maps.
#[derive(Debug)]
pub struct DedupelicatedHal {
    pub tree: Handle<TreeElem>,
    pub tree_elem_range: HandleRange<TreeElem>,

    pub devices: Vec<Handle<DeviceDescription>>,
    pub device_mappings: HashMap<Handle<DeviceDescription>, Handle<DeviceDescription>>,
}

#[derive(Debug, Clone, Copy)]
pub enum DeduplicationError {
    NonMatchingTypesFound {
        expected: Handle<TypeDescription>,
        found: Handle<TypeDescription>,
        found_in_hal: usize,
    },
}

/// Deduplicate a data across multiple memory maps.
pub fn deduplicate<'ctx, 'hals>(
    ctx: &'ctx IrCtx,
    mapping: &IrInputMapping<'ctx>,
    hals: impl Iterator<Item = &'hals HalHandles>,
) -> Result<(HalShared, Vec<DedupelicatedHal>), DeduplicationError> {
    let mut dedups = vec![];

    let mut all_devices = BTreeMap::<&'ctx str, Vec<_>>::new();
    let mut all_types = BTreeMap::<&'ctx TypeName, Vec<_>>::new();

    for (hal_idx, hal) in hals.enumerate() {
        let devices = hal
            .devices
            .handles()
            .map(|handle| (handle, &ctx.device_descs[handle]));

        for (handle, dev) in devices {
            all_devices
                .entry(&ctx.identifiers[dev.name])
                .or_default()
                .push((hal_idx, handle, dev));
        }

        let types = hal
            .types
            .handles()
            .map(|handle| (handle, &ctx.type_descs[handle]));

        for (handle, ty_desc) in types {
            all_types
                .entry(&ctx.type_names[ty_desc.name])
                .or_default()
                .push((hal_idx, handle, ty_desc));
        }

        let dedup = DedupelicatedHal {
            tree: hal.tree,
            tree_elem_range: hal.tree_elem_range,
            devices: vec![],
            device_mappings: Default::default(),
        };
        dedups.push(dedup);
    }

    let mut deduped_devices = vec![];
    let mut device_mappings = HashMap::new();

    for versions in all_devices.values() {
        let mut desc_iter = versions.iter().map(|(_hal, handle, _desc)| handle);
        let Some(first_handle) = desc_iter.next() else {
            continue;
        };

        let first_desc = mapping.device_descs[first_handle];

        let all_same = desc_iter.all(|handle| mapping.device_descs[handle] == first_desc);
        if all_same {
            // create mapping!
            for (_hal, handle, _) in versions.iter() {
                if handle != first_handle {
                    device_mappings.insert(*handle, *first_handle);
                }
            }
            deduped_devices.push(*first_handle);
        }
    }

    let mut deduped_types = vec![];
    let mut type_mapping = HashMap::new();

    for versions in all_types.values() {
        let mut desc_iter = versions.iter().copied();
        let Some((_, first_handle, _)) = desc_iter.next() else {
            continue;
        };

        let all_same = desc_iter
            .clone()
            .all(|(_, handle, _)| mapping.type_descs[&handle] == mapping.type_descs[&first_handle]);
        if all_same {
            // create mapping!
            for (_, handle, _) in desc_iter {
                if handle != first_handle {
                    type_mapping.insert(handle, first_handle);
                }
            }

            deduped_types.push(first_handle);
        } else {
            let (found_in_hal, found, _) = versions
                .iter()
                .find(|(_hal, handle, _desc)| {
                    mapping.type_descs[handle] != mapping.type_descs[&first_handle]
                })
                .unwrap();
            return Err(DeduplicationError::NonMatchingTypesFound {
                expected: first_handle,
                found: *found,
                found_in_hal: *found_in_hal,
            });
        }
    }

    for shared_handle in &deduped_devices {
        let dev = &ctx.device_descs[*shared_handle];
        let name = &ctx.identifiers[dev.name];

        if let Some((_, variants)) = all_devices.remove_entry(name.as_str()) {
            for (hal_idx, original_handle, _desc) in variants {
                if original_handle != *shared_handle {
                    dedups[hal_idx]
                        .device_mappings
                        .insert(original_handle, *shared_handle);
                }
            }
        }
    }

    // these are all the non-shared devices left!
    // tell the different HALs about them
    for (_name, variants) in all_devices {
        for (hal_idx, handle, _desc) in variants {
            dedups[hal_idx].devices.push(handle);
        }
    }

    let shared = HalShared {
        deduped_types,
        type_mapping,
        deduped_devices,
        device_mappings,
    };

    Ok((shared, dedups))
}

/// Deduplicate type names across the whole [IrCtx]
pub fn deduplicate_type_names<'ctx>(ctx: &'ctx mut IrCtx, shared: &HalShared) {
    let mut type_name_indices = BTreeMap::<&'ctx TypeName, Handle<TypeName>>::new();

    for ty in &shared.deduped_types {
        let ty_desc = &ctx.type_descs[*ty];
        let name = &ctx.type_names[ty_desc.name];
        type_name_indices.insert(name, ty_desc.name);
    }

    for ty in ctx.type_refs.values_mut() {
        if let TypeRef::Reference { name, args: _ } = ty {
            let ty_name = &ctx.type_names[*name];
            if let Some(ty_handle) = type_name_indices.get(ty_name) {
                *name = *ty_handle;
            } else {
                panic!(
                    "A type was referenced that has no definition:  {}, known types: {:?}",
                    ty_name.base,
                    type_name_indices
                        .keys()
                        .map(|n| &n.base)
                        .collect::<Vec<_>>()
                );
            }
        }
    }
}
