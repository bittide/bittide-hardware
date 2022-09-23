// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bytemuck::{bytes_of, Pod};
use fdt::node::FdtNode;

use crate::{utils::FdtNodeExt, ComponentLoadError};

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
        let memory_node = node.get_node("gather-memory")?;
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

        Ok(GatherUnit {
            memory: memory.starting_address as *mut u8,
            metacycle_register,
        })
    }

    pub const fn frame_size(&self) -> usize {
        FRAME_SIZE
    }

    pub fn write_frame_memory_bytes_full(&mut self, data: &[u8; FRAME_SIZE]) {
        for (i, d) in data.iter().copied().enumerate() {
            unsafe {
                self.memory.add(i).write_volatile(d);
            }
        }
    }

    pub fn write_frame_memory_full<T: Pod>(&mut self, data: T) {
        let data_bytes = bytes_of(&data);

        self.write_frame_memory_bytes(0, data_bytes);
    }

    pub fn write_frame_memory_bytes(&mut self, byte_idx: usize, data: &[u8]) {
        assert!(byte_idx + data.len() < FRAME_SIZE);

        unsafe {
            let ptr = self.memory.add(byte_idx);
            for (i, d) in data.iter().copied().enumerate() {
                ptr.add(i).write_volatile(d);
            }
        }
    }

    pub fn write_frame_memory<T: Pod>(&mut self, byte_idx: usize, data: T) {
        let data_bytes = bytes_of(&data);

        self.write_frame_memory_bytes(byte_idx, data_bytes);
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
