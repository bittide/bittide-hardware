// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use fdt::node::FdtNode;

use crate::{utils::matches_fdt_name, ComponentLoadError};

pub struct GatherUnit<const FRAME_SIZE: usize> {
    memory: *mut u8,
    metacycle_register: *const u8,
}

impl<const FRAME_SIZE: usize> GatherUnit<FRAME_SIZE> {
    /// Load the Gather-Unit information from a flattened-devicetree.
    ///
    /// # Safety
    ///
    /// The FDT must be a valid description of the hardware components that this
    /// code is running on.
    pub unsafe fn from_fdt_node(node: &FdtNode) -> Result<Self, ComponentLoadError> {
        let get_node = |path| {
            node.children()
                .find(|child| matches_fdt_name(child, path))
                .ok_or(ComponentLoadError::FdtNodeNotFound(path))
        };

        let get_reg = |node: &fdt::node::FdtNode, component| {
            node.reg()
                .ok_or(ComponentLoadError::RegNotFound { component })?
                .next()
                .ok_or(ComponentLoadError::RegNotFound { component })
        };

        let memory_node = get_node("gather-memory")?;
        let metacycle_register_node = get_node("metacycle-reg")?;

        let memory = get_reg(&memory_node, "memory")?;

        if let Some(size) = memory.size {
            if size != FRAME_SIZE {
                return Err(ComponentLoadError::SizeMismatch {
                    property: "memory frame size",
                    expected: FRAME_SIZE,
                    found: size,
                });
            }
        }

        let metacycle_register = {
            let reg = get_reg(&metacycle_register_node, "metacycle_register")?;
            if let Some(size) = reg.size {
                if size != 1 {
                    return Err(ComponentLoadError::SizeMismatch {
                        property: "metacycle register size",
                        expected: 1,
                        found: size,
                    });
                }
            }
            reg.starting_address
        };

        Ok(GatherUnit {
            memory: memory.starting_address as *mut u8,
            metacycle_register,
        })
    }

    pub const fn frame_size(&self) -> usize {
        FRAME_SIZE
    }

    pub fn write_frame_memory(&mut self, data: &[u8; FRAME_SIZE]) {
        for (i, d) in data.iter().copied().enumerate() {
            unsafe {
                self.memory.add(i).write_volatile(d);
            }
        }
    }

    /// Wait for the start of a new metacycle.
    ///
    /// Execution will stall until the start of a new metacycle.
    pub fn wait_for_new_metacycle(&self) {
        unsafe {
            // reading from the register will cause a stall until the end of the
            // metacycle, the read value is not actually relevant, so it's safe
            // to discard.
            let _val = self.metacycle_register.read_volatile();
        }
    }
}
