// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use quote::quote;

use crate::{
    generators::ident,
    parse::{MemoryMapDesc, MemoryMapTree, SourceLocation},
};

pub fn generate_device_instances_struct(desc: &MemoryMapDesc) -> proc_macro2::TokenStream {
    let mut instances: Vec<(u64, &str, &str, &SourceLocation)> = vec![];
    gather_device_instances(&desc.tree, &mut instances);

    let field_defs = instances
        .iter()
        .map(|(_, device_name, instance_name, _)| {
            let dev_name_ident = ident(*device_name);
            let instance_name_ident = ident(*instance_name);
            quote! {
                pub #instance_name_ident: #dev_name_ident,
            }
        })
        .collect::<Vec<_>>();

    let field_inits = instances
        .iter()
        .map(|(addr, device_name, instance_name, _src_loc)| {
            let dev_name_ident = ident(*device_name);
            let instance_name_ident = ident(*instance_name);

            quote! {
                #instance_name_ident: unsafe { #dev_name_ident::new(#addr as *mut u8) },
            }
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

fn gather_device_instances<'tree, 'vec>(
    tree: &'tree MemoryMapTree,
    instances: &'vec mut Vec<(u64, &'tree str, &'tree str, &'tree SourceLocation)>,
) {
    match tree {
        MemoryMapTree::Interconnect {
            absolute_address: _,
            components,
            src_location: _,
        } => {
            for comp in components {
                gather_device_instances(&comp.tree, instances)
            }
        }

        MemoryMapTree::DeviceInstance {
            absolute_address,
            device_name,
            instance_name,
            src_location,
        } => {
            let item = (
                *absolute_address,
                device_name.as_str(),
                instance_name.as_str(),
                src_location,
            );
            instances.push(item);
        }
    }
}
