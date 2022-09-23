// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use fdt::node::FdtNode;

use crate::ComponentLoadError;

pub fn matches_fdt_name(node: &FdtNode, name: &str) -> bool {
    if let Some(node_name) = node.name.split('@').next() {
        node_name == name
    } else {
        false
    }
}

pub trait FdtNodeExt: Sized {
    fn get_node(&self, path: &'static str) -> Result<Self, ComponentLoadError>;

    fn get_reg(
        &self,
        component_name: &'static str,
    ) -> Result<fdt::standard_nodes::MemoryRegion, ComponentLoadError>;
}

impl<'b, 'a> FdtNodeExt for FdtNode<'b, 'a> {
    fn get_node(&self, path: &'static str) -> Result<Self, ComponentLoadError> {
        self.children()
            .find(|child| matches_fdt_name(child, path))
            .ok_or(ComponentLoadError::FdtNodeNotFound(path))
    }

    fn get_reg(
        &self,
        component_name: &'static str,
    ) -> Result<fdt::standard_nodes::MemoryRegion, ComponentLoadError> {
        self.reg()
            .ok_or(ComponentLoadError::RegNotFound {
                component: component_name,
            })?
            .next()
            .ok_or(ComponentLoadError::RegNotFound {
                component: component_name,
            })
    }
}
