// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use crate::FdtLoadError;

pub struct GatherTimingOracle {
    sequence_counter: *const u64,
    send_sequence_counter: *mut u8,
}

impl GatherTimingOracle {
    pub fn sequence_counter(&self) -> u64 {
        unsafe { self.sequence_counter.read_volatile() }
    }

    pub fn send_sequence_counter(&mut self, enable: bool) {
        unsafe {
            self.send_sequence_counter.write_volatile(u8::from(enable));
        }
    }
}

pub struct GatherUnit<const FRAME_SIZE: usize> {
    memory: *mut u8,
    timing_oracle: GatherTimingOracle,
    metacycle_register: *const u8,
}

impl<const FRAME_SIZE: usize> GatherUnit<FRAME_SIZE> {
    /// Load the Gather-Unit information from a flattened-devicetree.
    ///
    /// # Safety
    ///
    /// The FDT must be a valid description of the hardware components that this
    /// code is running on.
    pub unsafe fn from_fdt(fdt: &fdt::Fdt) -> Result<Self, FdtLoadError> {
        let get_node = |path| {
            fdt.find_node(path)
                .ok_or(FdtLoadError::FdtNodeNotFound(path))
        };

        let get_reg = |node: &fdt::node::FdtNode, component| {
            node.reg()
                .ok_or(FdtLoadError::RegNotFound { component })?
                .next()
                .ok_or(FdtLoadError::RegNotFound { component })
        };

        let memory_node = get_node("/gather-unit/gather-memory")?;
        let sequence_counter_node = get_node("/gather-unit/sequence-counter-reg")?;
        let send_sequence_counter_node = get_node("/gather-unit/send-sequence-counter-reg")?;
        let metacycle_register_node = get_node("/gather-unit/metacycle-reg")?;

        let memory = get_reg(&memory_node, "memory")?;

        if let Some(size) = memory.size {
            if size != FRAME_SIZE {
                return Err(FdtLoadError::SizeMismatch {
                    property: "memory frame size",
                    expected: FRAME_SIZE,
                    found: size,
                });
            }
        }

        let sequence_counter = {
            let reg = get_reg(&sequence_counter_node, "sequence_counter")?;
            if let Some(size) = reg.size {
                if size != 8 {
                    return Err(FdtLoadError::SizeMismatch {
                        property: "sequence counter register size",
                        expected: 8,
                        found: size,
                    });
                }
            }
            reg.starting_address as *const u64
        };

        let send_sequence_counter = {
            let reg = get_reg(&send_sequence_counter_node, "send_sequence_counter")?;
            if let Some(size) = reg.size {
                if size != 1 {
                    return Err(FdtLoadError::SizeMismatch {
                        property: "send sequence counter register size",
                        expected: 1,
                        found: size,
                    });
                }
            }
            reg.starting_address as *mut u8
        };

        let metacycle_register = {
            let reg = get_reg(&metacycle_register_node, "metacycle_register")?;
            if let Some(size) = reg.size {
                if size != 1 {
                    return Err(FdtLoadError::SizeMismatch {
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
            timing_oracle: GatherTimingOracle {
                sequence_counter,
                send_sequence_counter,
            },
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

    pub fn timing_oracle(&self) -> &GatherTimingOracle {
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
