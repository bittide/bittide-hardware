// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::soft_ugn_demo_mu::devices::{ReceiveRingbuffer, TransmitRingbuffer};

impl TransmitRingbuffer {
    /// Write a slice to the transmit ringbuffer memory.
    ///
    /// # Panics
    ///
    /// The source memory size must be smaller or equal to the memory size of
    /// the `TransmitRingbuffer` memory.
    pub fn write_slice(&self, src: &[[u8; 8]], offset: usize) {
        assert!(src.len() + offset <= Self::DATA_LEN);
        let mut off = offset;
        for &val in src {
            unsafe {
                self.set_data_unchecked(off, val);
            }
            off += 1;
        }
    }
}

impl ReceiveRingbuffer {
    /// Read a slice from the receive ringbuffer memory.
    ///
    /// # Panics
    ///
    /// The destination memory size must be smaller or equal to the memory size
    ///  of the `ReceiveRingbuffer`.
    pub fn read_slice(&self, dst: &mut [[u8; 8]], offset: usize) {
        assert!(dst.len() + offset <= Self::DATA_LEN);
        let mut off = offset;
        for val in dst {
            unsafe {
                *val = self.data_unchecked(off);
            }
            off += 1;
        }
    }
}
