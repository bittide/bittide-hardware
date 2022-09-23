// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use fdt::node::FdtNode;

use crate::utils::FdtNodeExt;
use crate::ComponentLoadError;

pub struct TimingOracle {
    send_sequence_counter: *mut u8,
}

impl TimingOracle {
    /// Load the tx-Unit information from a flattened-devicetree.
    ///
    /// # Safety
    ///
    /// The FDT must be a valid description of the hardware components that this
    /// code is running on.
    pub unsafe fn from_fdt_node(node: &FdtNode) -> Result<Self, ComponentLoadError> {
        let send_sequence_counter_node = node.get_node("send-sequence-counter-reg")?;

        let send_sequence_counter = {
            let reg = send_sequence_counter_node.get_reg("send_sequence_counter")?;
            if let Some(size) = reg.size {
                if size != 1 {
                    return Err(ComponentLoadError::SizeMismatch {
                        property: "send sequence counter register size",
                        expected: 1,
                        found: size,
                    });
                }
            }
            reg.starting_address as *mut u8
        };

        Ok(TimingOracle {
            send_sequence_counter,
        })
    }

    pub fn send_sequence_counter(&mut self, enable: bool) {
        unsafe {
            self.send_sequence_counter.write_volatile(u8::from(enable));
        }
    }
}
