// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::collections::{BTreeMap, HashMap};
use std::str::FromStr;

use proc_macro2::Literal;
use quote::quote;

use crate::{
    generators::{generate_tag_docs, ident, IdentType},
    hal_set::{DeviceInstance, DeviceInstanceAnnotations},
    parse::path_name,
};

/// Generate a unique instance name, handling deduplication for both named and unnamed instances.
///
/// Returns the final deduplicated name for the instance.
fn generate_instance_name(
    instance: &DeviceInstance,
    device_name: &str,
    name_counts: &HashMap<String, usize>,
    name_usage: &mut HashMap<String, usize>,
    unnamed_counter: &mut u64,
) -> String {
    if let Some((_loc, base_name)) = path_name(&instance.path) {
        // Named instance - check if we need deduplication suffix
        let count = name_counts.get(base_name).copied().unwrap_or(0);
        if count > 1 {
            let usage = name_usage.entry(base_name.to_string()).or_insert(0);
            let final_name = format!("{}_{}", base_name, usage);
            *usage += 1;
            final_name
        } else {
            base_name.to_string()
        }
    } else {
        // Unnamed instance - use device name with counter
        let final_name = if *unnamed_counter == 0 {
            device_name.to_string()
        } else {
            format!("{}_{}", device_name, unnamed_counter)
        };
        *unnamed_counter += 1;
        final_name
    }
}

pub fn generate_device_instances_struct(
    instances: &BTreeMap<String, Vec<(DeviceInstanceAnnotations, DeviceInstance)>>,
) -> proc_macro2::TokenStream {
    // First pass: collect all instances and count name occurrences
    let all_instances: Vec<_> = instances
        .iter()
        .flat_map(|(device_name, device_instances)| {
            device_instances
                .iter()
                .map(move |(ann, instance)| (device_name.as_str(), ann, instance))
        })
        .collect();

    // Count occurrences of each base name (for named instances only)
    let name_counts: HashMap<String, usize> = all_instances
        .iter()
        .filter_map(|(_, _, instance)| path_name(&instance.path).map(|(_, name)| name.to_string()))
        .fold(HashMap::new(), |mut counts, name| {
            *counts.entry(name).or_insert(0) += 1;
            counts
        });

    // Second pass: generate deduplicated names
    let mut name_usage: HashMap<String, usize> = HashMap::new();
    let mut device_unnamed_counters: HashMap<&str, u64> = HashMap::new();

    let instance_names: Vec<String> = all_instances
        .iter()
        .map(|(device_name, _, instance)| {
            let counter = device_unnamed_counters.entry(device_name).or_insert(0);
            generate_instance_name(
                instance,
                device_name,
                &name_counts,
                &mut name_usage,
                counter,
            )
        })
        .collect();

    // Generate field definitions
    let field_defs: Vec<_> = all_instances
        .iter()
        .zip(&instance_names)
        .map(|((device_name, ann, instance), final_name)| {
            let dev_name_ident = ident(IdentType::Device, device_name);
            let instance_name_ident = ident(IdentType::Instance, final_name);
            let tags = generate_tag_docs(
                instance.tags.iter().map(|t| t.tag.as_str()),
                ann.tags.iter().map(String::as_str),
            );
            quote! {
                #tags
                pub #instance_name_ident: #dev_name_ident,
            }
        })
        .collect();

    // Generate field initializations
    let field_inits: Vec<_> = all_instances
        .iter()
        .zip(&instance_names)
        .map(|((device_name, _, instance), final_name)| {
            let dev_name_ident = ident(IdentType::Device, device_name);
            let instance_name_ident = ident(IdentType::Instance, final_name);
            let addr = Literal::from_str(&format!("0x{:X}", instance.absolute_address)).unwrap();
            quote! {
                #instance_name_ident: unsafe { #dev_name_ident::new(#addr as *mut u8) },
            }
        })
        .collect();

    quote! {
        pub struct DeviceInstances {
            #(#field_defs)*
        }

        impl DeviceInstances {
            pub const unsafe fn new() -> Self {
                DeviceInstances {
                    #(#field_inits)*
                }
            }
        }
    }
}
