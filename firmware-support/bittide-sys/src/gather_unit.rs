// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
pub struct GatherUnit<const MEM_SIZE: usize> {
    memory: *mut u32,
    metacycle_register: *const u32,
}

impl<const MEM_SIZE: usize> GatherUnit<MEM_SIZE> {
    /// Create a new [`GatherUnit`] instance given a base address. The
    /// `MEM_SIZE` is the number of 64-bit words.
    ///
    /// # Safety
    ///
    /// The `base_addr` pointer MUST be a valid pointer that is backed
    /// by a memory mapped gather unit instance.
    pub unsafe fn new(base_addr: *const ()) -> GatherUnit<MEM_SIZE> {
        let addr = base_addr as *const u32;
        GatherUnit {
            memory: addr.cast_mut(),
            metacycle_register: addr.add(MEM_SIZE * 2),
        }
    }

    /// # Safety
    ///
    /// The source memory size must be smaller or equal to the size of the
    /// `GatherUnit` memory.
    pub unsafe fn copy_from_slice(&self, src: &[u32], offset: usize) {
        let dst = self.memory.add(offset);
        assert!(src.len() + offset <= MEM_SIZE * 2);
        core::ptr::copy_nonoverlapping(src.as_ptr(), dst, src.len());
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
