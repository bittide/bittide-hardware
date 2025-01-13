// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
pub struct GatherUnit<const MEM_SIZE: usize> {
    base_addr: *mut u32,
}

impl<const MEM_SIZE: usize> GatherUnit<MEM_SIZE> {
    const METACYCLE_OFFSET: usize = MEM_SIZE * 2;

    /// Create a new [`GatherUnit`] instance given a base address. The
    /// `MEM_SIZE` is the number of 64-bit words.
    ///
    /// # Safety
    ///
    /// The `base_addr` pointer MUST be a valid pointer that is backed
    /// by a memory mapped gather unit instance.
    pub unsafe fn new(base_addr: *const ()) -> GatherUnit<MEM_SIZE> {
        let addr = base_addr as *mut u32;
        GatherUnit { base_addr: addr }
    }

    /// Write a slice to the gather memory.
    ///
    /// # Panics
    ///
    /// The source memory size must be smaller or equal to the memory size of
    /// the `GatherUnit` memory.
    pub unsafe fn write_slice(&self, src: &[u32], offset: usize) {
        let dst = self.base_addr.add(offset);
        assert!(src.len() + offset <= Self::METACYCLE_OFFSET);
        core::ptr::copy_nonoverlapping(src.as_ptr(), dst, src.len());
    }

    /// Wait for the start of a new metacycle.
    ///
    /// Reading from the register will cause a stall until the end of the
    /// metacycle. The read value is not actually relevant, so it's safe
    /// to discard.
    pub fn wait_for_new_metacycle(&self) {
        unsafe {
            let _val = self
                .base_addr
                .add(Self::METACYCLE_OFFSET)
                .read_volatile();
        }
    }
}
