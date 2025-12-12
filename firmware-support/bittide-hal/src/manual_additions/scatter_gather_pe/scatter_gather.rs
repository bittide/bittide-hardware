// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::hals::scatter_gather_pe::devices::{GatherUnit, ScatterUnit};

impl GatherUnit {
    /// Write a slice to the gather memory.
    ///
    /// # Panics
    ///
    /// The source memory size must be smaller or equal to the memory size of
    /// the `GatherUnit` memory.
    pub fn write_slice(&self, src: &[[u8; 8]], offset: usize) {
        assert!(src.len() + offset <= Self::GATHER_MEMORY_LEN);
        let mut off = offset;
        for &val in src {
            unsafe {
                self.set_gather_memory_unchecked(off, val);
            }
            off += 1;
        }
    }

    /// Wait for the start of a new metacycle.
    ///
    /// Reading from the register will cause a stall until the end of the
    /// metacycle. The read value is not actually relevant, so it's safe
    /// to discard.
    pub fn wait_for_new_metacycle(&self) {
        let _ = self.metacycle_register();
    }
}

impl ScatterUnit {
    /// Read a slice from the scatter memory.
    ///
    /// # Panics
    ///
    /// The destination memory size must be smaller or equal to the memory size
    ///  of the `ScatterUnit`.
    pub fn read_slice(&self, dst: &mut [[u8; 8]], offset: usize) {
        assert!(dst.len() + offset <= Self::SCATTER_MEMORY_LEN);
        let mut off = offset;
        for val in dst {
            unsafe {
                *val = self.scatter_memory_unchecked(off);
            }
            off += 1;
        }
    }

    /// Wait for the start of a new metacycle.
    ///
    /// Reading from the register will cause a stall until the end of the
    /// metacycle. The read value is not actually relevant, so it's safe
    /// to discard.
    pub fn wait_for_new_metacycle(&self) {
        let _ = self.metacycle_register();
    }
}
