// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use std::collections::{BTreeMap, BTreeSet};

use std::fmt::Write;

use crate::ir::types::TreeElemType;
use crate::{
    backends::c::{ident, IdentType},
    ir::{
        deduplicate::HalShared,
        types::{IrCtx, PathComp, TreeElem},
    },
    storage::Handle,
};

pub fn generate_device_instances(
    ctx: &IrCtx,
    shared: &HalShared,
    hal_name: &str,
    tree_elems: impl Iterator<Item = Handle<TreeElem>>,
) -> String {
    let mut device_type = String::new();
    let mut device_instance = String::new();
    let mut imports_shared = BTreeSet::new();
    let mut imports_local = BTreeSet::new();

    let mut name_idx = BTreeMap::<_, usize>::new();
    let hal_ident = ident(IdentType::Module, hal_name);

    writeln!(device_type, "typedef struct DeviceInstances {{").unwrap();

    writeln!(device_instance, "static const DeviceInstances hal = {{").unwrap();

    for handle in tree_elems {
        let (name, addr) = match ctx.tree_elem_types[handle.cast()] {
            TreeElemType::DeviceInstance { device_name } => {
                let elem = &ctx.tree_elems[handle];
                let addr = elem.absolute_addr;
                if ctx.tags[elem.tags].iter().any(|tag| tag == "no-generate") {
                    continue;
                }
                (device_name, addr)
            }
            TreeElemType::Interconnect {
                rel_addrs: _,
                components: _,
            } => continue,
        };
        let name = &ctx.identifiers[name];
        let elem = &ctx.tree_elems[handle];
        let dev_ident = ident(IdentType::Device, name);

        let path = &ctx.paths[elem.path];

        let instance_name_ident = if let Some(name) = path_name(path) {
            ident(IdentType::Instance, name)
        } else {
            let instance_name = match name_idx.entry(name) {
                std::collections::btree_map::Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(0);
                    &dev_ident.to_string()
                }
                std::collections::btree_map::Entry::Occupied(mut occupied_entry) => {
                    *occupied_entry.get_mut() += 1;
                    &format!("{dev_ident}_{}", occupied_entry.get())
                }
            };
            ident(IdentType::Instance, instance_name)
        };

        let hex_addr = format!("0x{:X}", addr);

        writeln!(device_type, "  {dev_ident} {instance_name_ident};").unwrap();

        writeln!(
            device_instance,
            "  .{instance_name_ident} = {{ .base = (volatile uint8_t *) {hex_addr} }},"
        )
        .unwrap();

        if shared.deduped_devices.iter().any(|dev| {
            let device_desc = &ctx.device_descs[*dev];
            let found = &ctx.identifiers[device_desc.name];
            found == name
        }) {
            imports_shared.insert(dev_ident.to_string());
        } else {
            imports_local.insert(dev_ident.to_string());
        };
    }

    writeln!(device_type, "}} DeviceInstances;").unwrap();

    writeln!(device_instance, "}};").unwrap();

    let mut code = String::new();
    for shared_dev in imports_shared {
        let dev_name = ident(IdentType::Module, shared_dev);
        writeln!(code, "#include \"shared_devices/{dev_name}.h\"").unwrap();
    }
    for local_dev in imports_local {
        let dev_name = ident(IdentType::Module, local_dev);
        writeln!(code, "#include \"hals/{hal_ident}/devices/{dev_name}.h\"").unwrap();
    }

    writeln!(code).unwrap();
    writeln!(code, "{device_type}").unwrap();
    writeln!(code, "{device_instance}").unwrap();

    code
}
fn path_name(path: &[PathComp]) -> Option<&str> {
    let comp = path.last()?;
    match comp {
        PathComp::Named { loc: _, name } => Some(name.as_str()),
        PathComp::Unnamed(_) => None,
    }
}
