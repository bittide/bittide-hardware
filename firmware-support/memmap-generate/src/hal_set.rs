// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::collections::{BTreeMap, BTreeSet};

use crate::{
    parse::{DeviceDesc, MemoryMapTree, Path, Tag, TypeDefinition},
    MemoryMapDesc,
};

#[derive(Clone)]
pub struct DeviceInstance {
    pub path: Path,
    pub tags: Vec<Tag>,
    pub device_name: String,
    pub src_location: u64,
    pub absolute_address: u64,
}

/// Annotations of a device definition, fields will be passed as they are
#[derive(Debug, Clone, Default)]
pub struct DeviceDescAnnotations {
    pub derives: Vec<proc_macro2::TokenStream>,
    pub imports: Vec<proc_macro2::TokenStream>,
    pub tags: BTreeSet<String>,
}

/// Annotations of a type definition, fields will be passed as they are
#[derive(Debug, Clone, Default)]
pub struct TypeDefAnnotations {
    pub derives: Vec<proc_macro2::TokenStream>,
    pub imports: Vec<proc_macro2::TokenStream>,
    pub tags: BTreeSet<String>,
}

#[derive(Debug, Clone, Default)]
pub struct DeviceInstanceAnnotations {
    pub tags: Vec<String>,
}

#[derive(Default, Clone)]
pub struct HalGenData {
    pub devices: BTreeMap<String, (DeviceDescAnnotations, DeviceDesc)>,
    pub types: BTreeMap<String, (TypeDefAnnotations, TypeDefinition)>,
    pub instances: BTreeMap<String, Vec<(DeviceInstanceAnnotations, DeviceInstance)>>,
    // TODO how to deal with locations for shared types?
}

pub struct MemoryMapSet {
    shared: HalGenData,
    non_shared: BTreeMap<String, HalGenData>,
}

impl MemoryMapSet {
    pub fn new_no_shared(hals: BTreeMap<String, MemoryMapDesc>) -> Self {
        let non_shared = hals
            .into_iter()
            .map(|(hal_name, mem_desc)| {
                (
                    hal_name,
                    HalGenData {
                        devices: mem_desc
                            .devices
                            .into_iter()
                            .map(|(key, x)| (key, (Default::default(), x)))
                            .collect(),
                        types: mem_desc
                            .types
                            .into_iter()
                            .map(|(key, x)| (key, (Default::default(), x)))
                            .collect(),
                        instances: BTreeMap::default(),
                    },
                )
            })
            .collect();

        Self {
            shared: HalGenData::default(),
            non_shared,
        }
    }

    pub fn new(hals: BTreeMap<String, MemoryMapDesc>) -> Self {
        let mut instances_per_hal =
            BTreeMap::<String, BTreeMap<String, Vec<DeviceInstance>>>::new();

        let mut all_devices = BTreeMap::<String, Vec<(String, DeviceDesc)>>::new();
        let mut all_types = BTreeMap::<String, Vec<(String, TypeDefinition)>>::new();

        for (hal_name, hal) in hals {
            let instances = instances_per_hal.entry(hal_name.clone()).or_default();
            gather_device_instances(&hal.tree, instances);

            for (dev_name, dev) in hal.devices {
                all_devices
                    .entry(dev_name)
                    .or_default()
                    .push((hal_name.clone(), dev));
            }

            for (ty_name, ty) in hal.types {
                all_types
                    .entry(ty_name)
                    .or_default()
                    .push((hal_name.clone(), ty));
            }
        }

        let mut shared_devices = BTreeMap::<String, DeviceDesc>::new();
        let mut shared_types = BTreeMap::<String, TypeDefinition>::new();

        for (name, idxs) in &all_devices {
            let mut idx_iter = idxs.iter().map(|(_hal, desc)| desc);
            let Some(first) = idx_iter.next() else {
                continue;
            };
            let all_same = idx_iter.all(|el| el == first);
            if all_same {
                shared_devices.insert(name.to_string(), first.clone());
            }
        }

        for (name, idxs) in &all_types {
            let mut idx_iter = idxs.iter().map(|(_hal, def)| def);
            let Some(first) = idx_iter.next() else {
                continue;
            };
            let all_same = idx_iter.all(|el| el == first);
            if all_same {
                shared_types.insert(name.to_string(), first.clone());
            }
        }

        all_devices.retain(|name, _descs| !shared_devices.contains_key(name.as_str()));
        all_types.retain(|name, _defs| !shared_types.contains_key(name.as_str()));

        let mut hals: BTreeMap<String, HalGenData> = BTreeMap::new();

        for (device_name, defs) in all_devices {
            for (hal_name, def) in defs {
                let hal_data = hals.entry(hal_name).or_default();
                hal_data
                    .devices
                    .insert(device_name.clone(), (Default::default(), def));
            }
        }

        for (type_name, defs) in all_types {
            for (hal_name, def) in defs {
                let hal_data = hals.entry(hal_name).or_default();
                hal_data
                    .types
                    .insert(type_name.clone(), (Default::default(), def));
            }
        }

        for (hal_name, instances) in instances_per_hal {
            let hal_data = hals.entry(hal_name).or_default();
            hal_data.instances = instances
                .into_iter()
                .map(|(key, instances)| {
                    (
                        key,
                        instances
                            .into_iter()
                            .map(|x| (Default::default(), x))
                            .collect(),
                    )
                })
                .collect();
        }

        // build one HalGenData for shared types and devices
        let mut shared_data = HalGenData::default();

        for (name, desc) in shared_devices {
            shared_data
                .devices
                .insert(name.to_string(), (Default::default(), desc));
        }
        for (name, def) in shared_types {
            shared_data
                .types
                .insert(name.to_string(), (Default::default(), def));
        }

        Self {
            shared: shared_data,
            non_shared: hals,
        }
    }

    pub fn filter_devices_by_tag(&mut self, mut f: impl FnMut(&str) -> bool) {
        let mut shared_filtered_out = BTreeSet::<String>::new();

        self.shared.devices.retain(|name, (_, desc)| {
            let any_filtered_out = desc.tags.as_slice().iter().any(|n| !f(n));
            if any_filtered_out {
                shared_filtered_out.insert(name.to_string());
            }
            !any_filtered_out
        });

        for (_hal, hal_data) in self.non_shared.iter_mut() {
            let mut local_filtered_out = BTreeSet::<String>::new();
            hal_data.devices.retain(|name, (ann, desc)| {
                let any_filtered_out = desc.tags.as_slice().iter().any(|n| !f(n));
                let any_filtered_out_ann = ann.tags.iter().any(|n| !f(n));
                if any_filtered_out || any_filtered_out_ann {
                    local_filtered_out.insert(name.to_string());
                }
                !any_filtered_out
            });

            hal_data.instances.retain(|name, instances| {
                let device_filtered_out =
                    local_filtered_out.contains(name) || shared_filtered_out.contains(name);

                if !device_filtered_out {
                    instances.retain(|(ann, instance)| {
                        let any_filtered_out = instance.tags.iter().any(|tag| !f(&tag.tag));
                        let any_filtered_out_ann = ann.tags.iter().any(|tag| !f(tag));
                        !(any_filtered_out || any_filtered_out_ann)
                    });

                    if instances.is_empty() {
                        // if there's no instances we can remove the whole
                        // reference to the device-instance-list anyway
                        false
                    } else {
                        true
                    }
                } else {
                    false
                }
            });
        }
    }

    /// Run a function for all type definitions, allowing to change annotations.
    pub fn annotate_types(&mut self, mut f: impl FnMut(&TypeDefinition, &mut TypeDefAnnotations)) {
        for hal_data in self.non_shared.values_mut() {
            for (ann, type_def) in hal_data.types.values_mut() {
                f(type_def, ann);
            }
        }

        for (ann, type_def) in self.shared.types.values_mut() {
            f(type_def, ann);
        }
    }

    /// Run a function for all device definitions, allowing to change annotations.
    pub fn annotate_devices(&mut self, mut f: impl FnMut(&DeviceDesc, &mut DeviceDescAnnotations)) {
        for hal_data in self.non_shared.values_mut() {
            for (ann, device_def) in hal_data.devices.values_mut() {
                f(device_def, ann);
            }
        }

        for (ann, device_def) in self.shared.devices.values_mut() {
            f(device_def, ann);
        }
    }

    pub fn shared(&self) -> &HalGenData {
        &self.shared
    }

    pub fn non_shared(&self) -> &BTreeMap<String, HalGenData> {
        &self.non_shared
    }
}

fn gather_device_instances(
    tree: &MemoryMapTree,
    instances: &mut BTreeMap<String, Vec<DeviceInstance>>,
) {
    match tree {
        MemoryMapTree::Interconnect {
            absolute_address: _,
            components,
            src_location: _,
            path: _,
            tags: _,
        } => {
            for comp in components {
                gather_device_instances(&comp.tree, instances)
            }
        }

        MemoryMapTree::DeviceInstance {
            path,
            tags,
            absolute_address,
            device_name,
            src_location,
        } => {
            let item = DeviceInstance {
                path: path.clone(),
                tags: tags.clone(),
                absolute_address: *absolute_address,
                device_name: device_name.clone(),
                src_location: *src_location,
            };
            instances.entry(device_name.clone()).or_default().push(item);
        }
    }
}
