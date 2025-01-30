// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
pub struct ScatterUnit<const MEM_SIZE: usize> {
    base_addr: *const u64,
}

impl<const MEM_SIZE: usize> ScatterUnit<MEM_SIZE> {
    const METACYCLE_OFFSET: usize = MEM_SIZE;

    /// Create a new [`ScatterUnit`] instance given a base address. The
    /// `MEM_SIZE` is the number of 64-bit words.
    ///
    /// # Safety
    ///
    /// The `base_addr` pointer MUST be a valid pointer that is backed
    /// by a memory mapped scatter unit instance.
    pub unsafe fn new(base_addr: *const ()) -> ScatterUnit<MEM_SIZE> {
        let addr = base_addr as *const u64;
        ScatterUnit { base_addr: addr }
    }

    /// Read a slice from the scatter memory.
    ///
    /// # Panics
    ///
    /// The destination memory size must be smaller or equal to the memory size
    ///  of the `ScatterUnit`.
    pub fn read_slice(&self, dst: &mut [u64], offset: usize) {
        assert!(dst.len() + offset <= Self::METACYCLE_OFFSET);
        let mut off = offset;
        for d in dst {
            unsafe {
                *d = self.base_addr.add(off).read_volatile();
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
        unsafe {
            let _val = self
                .base_addr
                .add(Self::METACYCLE_OFFSET)
                .cast::<usize>()
                .read_volatile();
        }
    }
}
