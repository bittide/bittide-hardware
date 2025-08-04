// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::{collections::BTreeMap, str::FromStr};

use proc_macro2::Literal;
use quote::quote;

use crate::{
    deprecated::generators::{generate_tag_docs, ident, IdentType},
    deprecated::hal_set::{DeviceInstance, DeviceInstanceAnnotations},
    input_language::path_name,
};

pub fn generate_device_instances_struct(
    instances: &BTreeMap<String, Vec<(DeviceInstanceAnnotations, DeviceInstance)>>,
) -> proc_macro2::TokenStream {
    let field_defs = instances
        .iter()
        .flat_map(|(device_name, instances)| {
            let dev_name_ident = ident(IdentType::Device, device_name);

            let mut name_idx = None::<u64>;

            instances.iter().map(move |(ann, instance)| {
                let tags = generate_tag_docs(
                    instance.tags.iter().map(|t| t.tag.as_str()),
                    ann.tags.iter().map(String::as_str),
                );
                if let Some((_loc_idx, name)) = path_name(&instance.path) {
                    let instance_name_ident = ident(IdentType::Instance, name);
                    quote! {
                        #tags
                        pub #instance_name_ident: #dev_name_ident,
                    }
                } else {
                    let instance_name = match name_idx {
                        Some(n) => {
                            name_idx = Some(n + 1);
                            &format!("{}_{}", device_name, n)
                        }
                        None => {
                            name_idx = Some(1);
                            device_name
                        }
                    };
                    let instance_name_ident = ident(IdentType::Instance, instance_name);
                    quote! {
                        #tags
                        pub #instance_name_ident: #dev_name_ident,
                    }
                }
            })
        })
        .collect::<Vec<_>>();

    let field_inits = instances
        .iter()
        .flat_map(|(device_name, instances)| {
            let dev_name_ident = ident(IdentType::Device, device_name);
            let mut name_idx = None::<u64>;

            instances.iter().map(move |(_ann, instance)| {
                let instance_name_ident = if let Some((_loc_idx, name)) = path_name(&instance.path)
                {
                    ident(IdentType::Instance, name)
                } else {
                    let instance_name = match name_idx {
                        Some(n) => {
                            name_idx = Some(n + 1);
                            &format!("{}_{}", device_name, n)
                        }
                        None => {
                            name_idx = Some(1);
                            device_name
                        }
                    };
                    ident(IdentType::Instance, instance_name)
                };

                let addr =
                    Literal::from_str(&format!("0x{:X}", instance.absolute_address)).unwrap();
                quote! {
                    #instance_name_ident: unsafe { #dev_name_ident::new(#addr as *mut u8) },
                }
            })
        })
        .collect::<Vec<_>>();

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
