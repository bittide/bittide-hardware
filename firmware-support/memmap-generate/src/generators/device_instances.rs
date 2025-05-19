// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::{collections::BTreeMap, str::FromStr};

use heck::{ToPascalCase, ToSnakeCase};
use proc_macro2::Literal;
use quote::quote;

use crate::{generators::ident, hal_set::DeviceInstance, parse::path_name};

pub fn generate_device_instances_struct(
    instances: &BTreeMap<String, Vec<DeviceInstance>>,
) -> proc_macro2::TokenStream {
    let field_defs = instances
        .iter()
        .flat_map(|(device_name, instances)| {
            let device_name = device_name.to_pascal_case();
            let dev_name_ident = ident(&device_name);

            let mut name_idx = None::<u64>;

            instances.iter().map(move |instance| {
                if let Some((_loc_idx, name)) = path_name(&instance.path) {
                    let instance_name_ident = ident(name.to_snake_case());
                    quote! {
                        pub #instance_name_ident: #dev_name_ident,
                    }
                } else {
                    // TODO better name case handling
                    let instance_name = match name_idx {
                        Some(n) => {
                            name_idx = Some(n + 1);
                            format!("{}_{}", device_name.to_snake_case(), n)
                        }
                        None => {
                            name_idx = Some(1);
                            device_name.to_snake_case()
                        }
                    };
                    let instance_name_ident = ident(instance_name);
                    quote! {
                        pub #instance_name_ident: #dev_name_ident,
                    }
                }
            })
        })
        .collect::<Vec<_>>();

    let field_inits = instances
        .iter()
        .flat_map(|(device_name, instances)| {
            let device_name = device_name.to_pascal_case();
            let dev_name_ident = ident(&device_name);
            let mut name_idx = None::<u64>;

            instances.iter().map(move |instance| {
                let instance_name_ident = if let Some((_loc_idx, name)) = path_name(&instance.path)
                {
                    ident(name.to_snake_case())
                } else {
                    // TODO better name case handling
                    let instance_name = match name_idx {
                        Some(n) => {
                            name_idx = Some(n + 1);
                            format!("{}_{}", device_name.to_snake_case(), n)
                        }
                        None => {
                            name_idx = Some(1);
                            device_name.to_snake_case()
                        }
                    };
                    ident(instance_name)
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
