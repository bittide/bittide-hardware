// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::derive::uDebug;

use crate::dna_port_e2::DnaValue;

#[repr(C)]
#[derive(uDebug, PartialEq, Eq, Copy, Clone)]
pub struct NodeData {
    pub local_counter: u64,
    pub dna: DnaValue,
}

pub struct SwitchDemoProcessingElement<const BUFFER_SIZE: usize> {
    base_addr: *const u64,
}

impl<const BUFFER_SIZE: usize> SwitchDemoProcessingElement<BUFFER_SIZE> {
    const READ_START: usize = 0;
    const READ_CYCLES: usize = 1;
    const WRITE_START: usize = 2;
    const WRITE_CYCLES: usize = 3;
    const COUNTER: usize = 4;
    const BUFFER: usize = 5;

    /// Create a new [`SwitchDemoProcessingElement`] instance given a base
    /// address. The `BUFFER_SIZE` is the number of [`NodeData`] elements in its
    /// internal buffer.
    ///
    /// # Safety
    ///
    /// The `base_addr` pointer must be a valid pointer that is backed by
    /// a memory mapped switch demo processing element. The `BUFFER_SIZE` should
    /// match the `bufferSize` of the associated `swtichDemoPeWb` device.
    pub unsafe fn new(base_addr: *const ()) -> Self {
        let addr = base_addr as *const u64;
        Self { base_addr: addr }
    }

    pub fn set_read(&self, read_start: u64, read_cycles: u64) {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe {
            self.base_addr
                .add(Self::READ_START)
                .cast_mut()
                .write_volatile(read_start);
            self.base_addr
                .add(Self::READ_CYCLES)
                .cast_mut()
                .write_volatile(read_cycles);
        }
    }

    pub fn set_write(&self, write_start: u64, write_cycles: u64) {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe {
            self.base_addr
                .add(Self::WRITE_START)
                .cast_mut()
                .write_volatile(write_start);
            self.base_addr
                .add(Self::WRITE_CYCLES)
                .cast_mut()
                .write_volatile(write_cycles);
        }
    }

    pub fn get_read(&self) -> (u64, u64) {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe {
            let read_start = self.base_addr.add(Self::READ_START).read_volatile();
            let read_cycles = self.base_addr.add(Self::READ_CYCLES).read_volatile();
            (read_start, read_cycles)
        }
    }

    pub fn get_write(&self) -> (u64, u64) {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe {
            let write_start = self.base_addr.add(Self::WRITE_START).read_volatile();
            let write_cycles = self.base_addr.add(Self::WRITE_CYCLES).read_volatile();
            (write_start, write_cycles)
        }
    }

    pub fn get_counter(&self) -> u64 {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe { self.base_addr.add(Self::COUNTER).read_volatile() }
    }

    pub fn buffer(&self) -> impl Iterator<Item = NodeData> + '_ {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        (Self::BUFFER..Self::BUFFER + BUFFER_SIZE * 3)
            .step_by(3)
            .map(|i| unsafe { self.base_addr.add(i).cast::<NodeData>().read_volatile() })
    }
}
