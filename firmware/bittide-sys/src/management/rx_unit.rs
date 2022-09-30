// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use fdt::node::FdtNode;

use crate::utils::FdtNodeExt;
use crate::ComponentLoadError;

pub struct TimingOracle {
    local_sequence_counter: *const u64,
    record_remote_sequence_counter: *mut u8,
    remote_sequence_counter: *const u64,
}

impl TimingOracle {
    /// Load the rx-Unit information from a flattened-devicetree.
    ///
    /// # Safety
    ///
    /// The FDT must be a valid description of the hardware components that this
    /// code is running on.
    pub unsafe fn from_fdt_node(node: &FdtNode) -> Result<Self, ComponentLoadError> {
        let local_sequence_counter_node = node.get_node("local-sequence-counter-reg")?;
        let record_remote_sequence_counter_node =
            node.get_node("record-remote-sequence-counter-reg")?;
        let remote_sequence_counter_node = node.get_node("remote-sequence-counter-reg")?;

        let local_sequence_counter = {
            let reg = local_sequence_counter_node.get_reg("local_sequence_counter")?;
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
            let reg =
                record_remote_sequence_counter_node.get_reg("record_remote_sequence_counter")?;
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
            let reg = remote_sequence_counter_node.get_reg("remote_sequence_counter")?;
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

        Ok(Self {
            local_sequence_counter,
            record_remote_sequence_counter,
            remote_sequence_counter,
        })
    }

    pub fn sequence_counters(&self) -> Option<(u64, u64)> {
        let content = unsafe { self.record_remote_sequence_counter.read_volatile() };

        if content == 3 {
            let local = unsafe { self.remote_sequence_counter.read_volatile() };
            let remote = unsafe { self.local_sequence_counter.read_volatile() };
            Some((local, remote))
        } else {
            None
        }
    }

    pub fn record_remote_sequence_counter(&mut self, enable: bool) {
        unsafe {
            self.record_remote_sequence_counter
                .write_volatile(u8::from(enable));
        }
    }
}
