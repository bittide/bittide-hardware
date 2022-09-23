// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use fdt::node::FdtNode;

use crate::{utils::FdtNodeExt, ComponentLoadError};

pub struct ScatterUnit<const FRAME_SIZE: usize> {
    memory: *const u8,
    metacycle_register: *const u8,
}

impl<const FRAME_SIZE: usize> ScatterUnit<FRAME_SIZE> {
    /// Load the Scatter-Unit information from a flattened-devicetree.
    ///
    /// # Safety
    ///
    /// The FDT must be a valid description of the hardware components that this
    /// code is running on.
    pub unsafe fn from_fdt_node(node: &FdtNode) -> Result<Self, ComponentLoadError> {
        let memory_node = node.get_node("scatter-memory")?;
        let metacycle_register_node = node.get_node("metacycle-reg")?;
        let memory = memory_node.get_reg("memory")?;

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
            let reg = metacycle_register_node.get_reg("metacycle_register")?;
            if let Some(size) = reg.size {
                if size != 1 {
                    return Err(ComponentLoadError::SizeMismatch {
                        property: "metacycle register size",
                        expected: 1,
                        found: size,
                    });
                }
            }
            reg.starting_address as *const u8
        };

        Ok(ScatterUnit {
            memory: memory.starting_address,
            metacycle_register,
        })
    }

    pub const fn frame_size(&self) -> usize {
        FRAME_SIZE
    }

    pub fn read_frame_memory(&self, data: &mut [u8; FRAME_SIZE]) {
        for (i, d) in data.iter_mut().enumerate() {
            unsafe {
                *d = self.memory.add(i).read_volatile();
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
