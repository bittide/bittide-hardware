// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

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

#[derive(Debug)]
pub struct HalShared {
    pub deduped_types: Vec<Handle<TypeDescription>>,
    pub type_mapping: HashMap<Handle<TypeDescription>, Handle<TypeDescription>>,

    pub deduped_devices: Vec<Handle<DeviceDescription>>,
    pub device_mappings: HashMap<Handle<DeviceDescription>, Handle<DeviceDescription>>,
}

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
        } else {
            println!("Hey look, a non-shared device!! {:?}", first_desc);
        }
    }

    let mut deduped_types = vec![];
    let mut type_mapping = HashMap::new();

    for versions in all_types.values() {
        let mut desc_iter = versions.iter().map(|(_hal, handle, _desc)| handle);
        let Some(first_handle) = desc_iter.next() else {
            continue;
        };
        let first_desc = mapping.type_descs[first_handle];

        let all_same = desc_iter.all(|handle| mapping.type_descs[handle] == first_desc);
        if all_same {
            // create mapping!
            for (_hal, handle, _) in versions.iter() {
                if handle != first_handle {
                    type_mapping.insert(*handle, *first_handle);
                }
            }
            deduped_types.push(*first_handle);
        } else {
            let (found_in_hal, found, _) = versions
                .iter()
                .find(|(_hal, handle, _desc)| mapping.type_descs[handle] != first_desc)
                .unwrap();
            return Err(DeduplicationError::NonMatchingTypesFound {
                expected: *first_handle,
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
            let ty_handle = type_name_indices[ty_name];
            *name = ty_handle;
        }
    }
}
