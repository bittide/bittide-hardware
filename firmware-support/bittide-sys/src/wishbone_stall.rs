// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

pub struct WishboneStall {
    base_addr: *mut u32,
}

impl WishboneStall {
    /// Create a new `WishboneStall` instance.
    ///
    /// # Safety
    ///
    /// `base_addr` needs to point to a mapped memory address for a Wishbone stall component.
    pub unsafe fn from_base_addr(base_addr: *const u32) -> WishboneStall {
        let addr = base_addr as *mut u32;
        WishboneStall { base_addr: addr }
    }

    pub fn stall_poll(&self) {
        while unsafe { self.base_addr.read() == 0 } {}
    }

    pub fn stall_ack(&self) {
        unsafe { self.base_addr.write(1) }
    }
}
