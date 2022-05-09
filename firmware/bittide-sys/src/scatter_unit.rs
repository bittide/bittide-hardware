// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use crate::FdtLoadError;

pub struct ScatterUnit<const FRAME_SIZE: usize> {
    memory: *const u8,
    sequence_counter: *const u64,
    record_sequence_counter: *mut u8,
}

impl<const FRAME_SIZE: usize> ScatterUnit<FRAME_SIZE> {
    /// Load the Scatter-Unit information from a flattened-devicetree.
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

        let memory_node = get_node("/scatter-unit/scatter-memory")?;
        let sequence_counter_node = get_node("/scatter-unit/sequence-counter-reg")?;
        let record_sequence_counter_node = get_node("/scatter-unit/record-sequence-counter-reg")?;

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

        let record_sequence_counter = {
            let reg = get_reg(&record_sequence_counter_node, "record_sequence_counter")?;
            if let Some(size) = reg.size {
                if size != 1 {
                    return Err(FdtLoadError::SizeMismatch {
                        property: "record sequence counter register size",
                        expected: 1,
                        found: size,
                    });
                }
            }
            reg.starting_address as *mut u8
        };

        Ok(ScatterUnit {
            memory: memory.starting_address,
            sequence_counter,
            record_sequence_counter,
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

    pub fn sequence_counter(&self) -> u64 {
        unsafe { self.sequence_counter.read_volatile() }
    }

    pub fn record_sequence_counter(&mut self, enable: bool) {
        unsafe {
            self.record_sequence_counter
                .write_volatile(u8::from(enable));
        }
    }
}
