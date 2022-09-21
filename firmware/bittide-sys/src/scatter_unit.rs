// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use fdt::node::FdtNode;

use crate::{utils::matches_fdt_name, ComponentLoadError};

pub struct ScatterTimingOracle {
    local_sequence_counter: *const u64,
    record_remote_sequence_counter: *mut u8,
    remote_sequence_counter: *const u64,
}

impl ScatterTimingOracle {
    pub fn local_sequence_counter(&self) -> u64 {
        unsafe { self.local_sequence_counter.read_volatile() }
    }

    pub fn record_remote_sequence_counter(&mut self, enable: bool) {
        unsafe {
            self.record_remote_sequence_counter
                .write_volatile(u8::from(enable));
        }
    }

    pub fn remote_sequence_counter(&self) -> u64 {
        unsafe { self.remote_sequence_counter.read_volatile() }
    }
}

pub struct ScatterUnit<const FRAME_SIZE: usize> {
    memory: *const u8,
    timing_oracle: ScatterTimingOracle,
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

        let memory_node = get_node("scatter-memory")?;
        let local_sequence_counter_node = get_node("local-sequence-counter-reg")?;
        let record_remote_sequence_counter_node = get_node("record-remote-sequence-counter-reg")?;
        let remote_sequence_counter_node = get_node("remote-sequence-counter-reg")?;
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

        let local_sequence_counter = {
            let reg = get_reg(&local_sequence_counter_node, "local_sequence_counter")?;
            if let Some(size) = reg.size {
                if size != 8 {
                    return Err(ComponentLoadError::SizeMismatch {
                        property: "local sequence counter register size",
                        expected: 8,
                        found: size,
                    });
                }
            }
            reg.starting_address as *const u64
        };

        let record_remote_sequence_counter = {
            let reg = get_reg(
                &record_remote_sequence_counter_node,
                "record_remote_sequence_counter",
            )?;
            if let Some(size) = reg.size {
                if size != 1 {
                    return Err(ComponentLoadError::SizeMismatch {
                        property: "record remote sequence counter register size",
                        expected: 1,
                        found: size,
                    });
                }
            }
            reg.starting_address as *mut u8
        };

        let remote_sequence_counter = {
            let reg = get_reg(&remote_sequence_counter_node, "remote_sequence_counter")?;
            if let Some(size) = reg.size {
                if size != 8 {
                    return Err(ComponentLoadError::SizeMismatch {
                        property: "remote sequence counter register size",
                        expected: 8,
                        found: size,
                    });
                }
            }
            reg.starting_address as *const u64
        };

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
            reg.starting_address as *const u8
        };

        Ok(ScatterUnit {
            memory: memory.starting_address,
            timing_oracle: ScatterTimingOracle {
                local_sequence_counter,
                record_remote_sequence_counter,
                remote_sequence_counter,
            },
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

    pub fn timing_oracle(&self) -> &ScatterTimingOracle {
        &self.timing_oracle
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
