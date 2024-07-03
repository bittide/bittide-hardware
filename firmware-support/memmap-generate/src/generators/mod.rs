// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::collections::BTreeMap;

use device_instances::generate_device_instances_struct;
use device_types::generate_device_type;
use proc_macro2::{Ident, Span, TokenStream};
use types::generate_type_def;

use crate::parse::MemoryMapDesc;

pub mod device_instances;
pub mod device_types;
pub mod types;

pub(crate) fn ident(n: impl AsRef<str>) -> Ident {
    // TODO handle things like CamelCase/snake_case conversion?
    let s = match n.as_ref() {
        "(,)" => "Pair",
        "(,,)" => "Thruple",
        "(,,,)" => "FourTuple",
        "(,,,,)" => "FiveTuple",
        s => s,
    };

    Ident::new(s, Span::call_site())
}

pub(crate) fn generic_name(idx: u64) -> Ident {
    ident(format!("{}", 'A' as u64 + idx))
}

pub struct RustWrappers {
    pub device_defs: BTreeMap<String, TokenStream>,
    pub type_defs: BTreeMap<String, TokenStream>,
    pub device_instances_struct: TokenStream,
}

pub enum ItemScopeMode {
    OneFile,
    // SeparateModules, // TODO
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum DebugDerive {
    None,
    Std,
    Ufmt,
}

pub struct GenerateConfig {
    pub debug_derive_mode: DebugDerive,
    pub item_scope_mode: ItemScopeMode,
}

pub fn generate_rust_wrappers(desc: &MemoryMapDesc, config: &GenerateConfig) -> RustWrappers {
    let device_defs = desc
        .devices
        .iter()
        .map(|(name, device_desc)| (name.clone(), generate_device_type(device_desc)))
        .collect();

    let type_defs = desc
        .types
        .iter()
        .map(|(name, type_desc)| {
            (
                name.clone(),
                generate_type_def(type_desc, config.debug_derive_mode),
            )
        })
        .collect();

    let device_instances_struct = generate_device_instances_struct(desc);

    RustWrappers {
        device_defs,
        type_defs,
        device_instances_struct,
    }
}
