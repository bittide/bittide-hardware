// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::collections::{BTreeMap, BTreeSet};

use crate::{
    deprecated::generators::types::PrimitiveType,
    input_language::{
        DeviceDesc, LocationRef, MemoryMapTree, Path, SourceLocation, Tag, TypeDescription,
        TypeName,
    },
    MemoryMapDesc,
};

#[derive(Clone)]
pub struct DeviceInstance {
    pub path: Path,
    pub tags: Vec<Tag>,
    pub device_name: String,
    pub src_location: LocationRef,
    pub absolute_address: u64,
}

/// Annotations of a device definition, fields will be passed as they are
#[derive(Debug, Clone, Default)]
pub struct DeviceDescAnnotations {
    pub referenced_types: BTreeSet<TypeName>,
    pub referenced_primitives: BTreeSet<PrimitiveType>,
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
pub struct HalData {
    pub devices: BTreeMap<String, (DeviceDescAnnotations, DeviceDesc)>,
    pub instances: BTreeMap<String, Vec<(DeviceInstanceAnnotations, DeviceInstance)>>,
    pub locations: Vec<SourceLocation>,
}

pub struct MemoryMapSet {
    types: BTreeMap<String, (TypeDefAnnotations, TypeDescription)>,
    shared_devices: BTreeMap<String, (DeviceDescAnnotations, DeviceDesc)>,
    hals: BTreeMap<String, HalData>,
}

impl MemoryMapSet {
    pub fn new(hals: BTreeMap<String, MemoryMapDesc>) -> Self {
        let mut instances_per_hal =
            BTreeMap::<String, BTreeMap<String, Vec<DeviceInstance>>>::new();

        let mut all_devices = BTreeMap::<String, Vec<(String, DeviceDesc)>>::new();
        let mut all_types = BTreeMap::<String, Vec<(String, TypeDescription)>>::new();

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
        let mut shared_types = BTreeMap::<String, TypeDescription>::new();

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
            } else {
                todo!("Types don't match, better error message here")
            }
        }

        all_devices.retain(|name, _descs| !shared_devices.contains_key(name.as_str()));

        let mut hals: BTreeMap<String, HalData> = BTreeMap::new();

        for (device_name, defs) in all_devices {
            for (hal_name, def) in defs {
                let hal_data = hals.entry(hal_name).or_default();
                hal_data
                    .devices
                    .insert(device_name.clone(), (Default::default(), def));
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

        Self {
            types: shared_types
                .into_iter()
                .map(|(key, desc)| (key, (Default::default(), desc)))
                .collect(),
            shared_devices: shared_devices
                .into_iter()
                .map(|(key, desc)| (key, (Default::default(), desc)))
                .collect(),
            hals,
        }
    }

    pub fn filter_builtin_types(&mut self) {
        self.filter_types(|desc, _ann| match &desc.definition {
            crate::input_language::TypeDefinition::Builtin(_builtin_type) => false,
            _ => true,
        });
    }

    pub fn filter_devices_by_tag(&mut self, mut f: impl FnMut(&str) -> bool) {
        let mut shared_filtered_out = BTreeSet::<String>::new();

        self.shared_devices.retain(|name, (_, desc)| {
            let any_filtered_out = desc.tags.as_slice().iter().any(|n| !f(n));
            if any_filtered_out {
                shared_filtered_out.insert(name.to_string());
            }
            !any_filtered_out
        });

        for (_hal, hal_data) in self.hals.iter_mut() {
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

    pub fn filter_types(
        &mut self,
        mut f: impl FnMut(&TypeDescription, &TypeDefAnnotations) -> bool,
    ) {
        self.types.retain(|_name, (ann, desc)| f(desc, ann));
    }

    /// Run a function for all type definitions, allowing to change annotations.
    pub fn annotate_types(&mut self, mut f: impl FnMut(&TypeDescription, &mut TypeDefAnnotations)) {
        for (ann, type_def) in self.types.values_mut() {
            f(type_def, ann);
        }
    }

    /// Run a function for all device definitions, allowing to change annotations.
    pub fn annotate_devices(&mut self, mut f: impl FnMut(&DeviceDesc, &mut DeviceDescAnnotations)) {
        for hal_data in self.hals.values_mut() {
            for (ann, device_def) in hal_data.devices.values_mut() {
                f(device_def, ann);
            }
        }

        for (ann, device_def) in self.shared_devices.values_mut() {
            f(device_def, ann);
        }
    }

    pub fn types(&self) -> &BTreeMap<String, (TypeDefAnnotations, TypeDescription)> {
        &self.types
    }

    pub fn shared_devices(&self) -> &BTreeMap<String, (DeviceDescAnnotations, DeviceDesc)> {
        &self.shared_devices
    }

    pub fn hals(&self) -> &BTreeMap<String, HalData> {
        &self.hals
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
                src_location: src_location.clone(),
            };
            instances.entry(device_name.clone()).or_default().push(item);
        }
    }
}
