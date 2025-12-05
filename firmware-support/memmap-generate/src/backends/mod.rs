// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Different backends for code generation, creating HALs.

use std::collections::BTreeMap;

use crate::{
    ir::types::{IrCtx, PathComp, TreeElem, TreeElemType},
    storage::Handle,
};

pub mod c;

pub mod rust;

/// Helper function to collect all names of device instances in a HAL
/// to generate proper unique names on a best-fit basis.
///
/// This is done by counting all the instance names and creating a
/// mapping from handles to names for convenience.
pub(crate) fn all_instance_names(
    ctx: &'_ IrCtx,
    tree_elems: impl Iterator<Item = Handle<TreeElem>>,
) -> (
    BTreeMap<Handle<TreeElem>, &'_ str>,
    BTreeMap<&'_ str, usize>,
) {
    let mut names = BTreeMap::new();
    let mut mapping = BTreeMap::new();

    for elem_handle in tree_elems {
        let device_name = match &ctx.tree_elem_types[elem_handle.cast()] {
            TreeElemType::DeviceInstance { device_name } => &ctx.identifiers[*device_name],
            TreeElemType::Interconnect { .. } => continue,
        };
        let elem = &ctx.tree_elems[elem_handle];

        if ctx.tags[elem.tags].iter().any(|tag| tag == "no-generate") {
            continue;
        }

        if let Some(name) = path_name(&ctx.paths[elem.path]) {
            *names.entry(name).or_default() += 1;
            mapping.insert(elem_handle, name);
        } else {
            *names.entry(device_name.as_str()).or_default() += 1;
            mapping.insert(elem_handle, device_name.as_str());
        }
    }

    (mapping, names)
}

/// Helper to get the last named component of a path, if it exists.
fn path_name(path: &[PathComp]) -> Option<&str> {
    let comp = path.last()?;
    match comp {
        PathComp::Named { loc: _, name } => Some(name.as_str()),
        PathComp::Unnamed(_) => None,
    }
}
