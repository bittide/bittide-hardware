// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::collections::BTreeMap;

use device_instances::generate_device_instances_struct;
use device_types::DeviceGenerator;
use proc_macro2::{Ident, Span, TokenStream};
use types::TypeGenerator;

use crate::hal_set::HalGenData;

pub mod device_instances;
pub mod device_types;
pub mod types;

pub(crate) fn ident(n: impl AsRef<str>) -> Ident {
    // TODO handle things like CamelCase/snake_case conversion?
    let s = match n.as_ref() {
        "(,)" => "Pair",
        "(,,)" => "Triple",
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

pub fn generate_rust_wrappers(hal_data: &HalGenData, config: &GenerateConfig) -> RustWrappers {
    let mut ty_gen = TypeGenerator::new();
    let mut device_gen = DeviceGenerator::new();

    let type_defs = hal_data
        .types
        .iter()
        .map(|(name, type_desc)| {
            (
                name.clone(),
                ty_gen.generate_type_def(type_desc, config.debug_derive_mode),
            )
        })
        .collect();

    let device_defs = hal_data
        .devices
        .iter()
        .map(|(name, device_desc)| {
            (
                name.clone(),
                device_gen.generate_device_type(device_desc, &mut ty_gen),
            )
        })
        .collect();

    let device_instances_struct = generate_device_instances_struct(&hal_data.instances);

    RustWrappers {
        device_defs,
        type_defs,
        device_instances_struct,
    }
}
