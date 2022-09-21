// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use fdt::node::FdtNode;

pub fn matches_fdt_name(node: &FdtNode, name: &str) -> bool {
    if let Some(node_name) = node.name.split('@').next() {
        node_name == name
    } else {
        false
    }
}
