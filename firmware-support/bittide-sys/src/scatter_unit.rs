// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
pub struct ScatterUnit<const MEM_SIZE: usize> {
    memory: *const u32,
    metacycle_register: *const u32,
}

impl<const MEM_SIZE: usize> ScatterUnit<MEM_SIZE> {
    /// Create a new [`ScatterUnit`] instance given a base address. The
    /// `MEM_SIZE` is the number of 64-bit words.
    ///
    /// # Safety
    ///
    /// The `base_addr` pointer MUST be a valid pointer that is backed
    /// by a memory mapped scatter unit instance.
    pub unsafe fn new(base_addr: *const ()) -> ScatterUnit<MEM_SIZE> {
        let addr = base_addr as *const u32;
        ScatterUnit {
            memory: addr,
            metacycle_register: addr.add(MEM_SIZE * 2),
        }
    }

    /// # Safety
    ///
    /// The destination memory size must be smaller or equal to the size of the
    /// `ScatterUnit`.
    pub unsafe fn copy_to_slice(&self, dst: &mut [u32], offset: usize) {
        let src = self.memory.add(offset);
        assert!(dst.len() + offset <= MEM_SIZE * 2);
        core::ptr::copy_nonoverlapping(src, dst.as_mut_ptr(), dst.len());
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
