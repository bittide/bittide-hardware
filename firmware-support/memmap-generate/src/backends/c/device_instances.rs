// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use std::collections::{BTreeMap, BTreeSet};

use std::fmt::Write;

use crate::backends::all_instance_names;
use crate::ir::types::TreeElemType;
use crate::{
    backends::c::{ident, IdentType},
    ir::{
        deduplicate::HalShared,
        types::{IrCtx, TreeElem},
    },
    storage::Handle,
};

pub fn generate_device_instances(
    ctx: &IrCtx,
    shared: &HalShared,
    hal_name: &str,
    tree_elems: impl Iterator<Item = Handle<TreeElem>> + Clone,
) -> String {
    let mut device_type = String::new();
    let mut device_instance = String::new();
    let mut imports_shared = BTreeSet::new();
    let mut imports_local = BTreeSet::new();

    let hal_ident = ident(IdentType::Module, hal_name);

    writeln!(device_type, "typedef struct DeviceInstances {{").unwrap();

    writeln!(device_instance, "static const DeviceInstances hal = {{").unwrap();

    let (name_mapping, name_counts) = all_instance_names(ctx, tree_elems.clone());
    let mut names_used = BTreeMap::new();

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
        let dev_ident = ident(IdentType::Device, name);

        let instance_name = {
            let raw_name = name_mapping[&handle];
            if name_counts[raw_name] > 1 {
                // if there's more than one instance with the same name,
                // generate suffixes, `_0`, `_1` etc
                let count = names_used
                    .entry(raw_name)
                    .and_modify(|n| *n += 1)
                    .or_insert(0);
                ident(IdentType::Instance, format!("{raw_name}_{count}"))
            } else {
                ident(IdentType::Instance, raw_name)
            }
        };

        let hex_addr = format!("0x{:X}", addr);

        writeln!(device_type, "  {dev_ident} {instance_name};").unwrap();

        writeln!(
            device_instance,
            "  .{instance_name} = {{ .base = (volatile uint8_t *) {hex_addr} }},"
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
