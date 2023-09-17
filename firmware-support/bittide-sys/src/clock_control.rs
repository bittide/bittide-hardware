// SPDX-FileCopyrightText: 2023 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use crate::ComponentLoadError;

#[derive(ufmt::derive::uDebug)]
pub struct OutOfBoundsError;

#[derive(ufmt::derive::uDebug, Copy, Clone)]
pub enum SpeedChange {
    SpeedUp,
    SlowDown,
    NoChange,
}

pub struct ClockControl {
    num_links: u8,
    link_mask: *const u32,
    finc_fdec: *mut u32,
    links_stable: *const u32,
    links_settled: *const u32,
    data_counts_start: *const i32,
}

impl ClockControl {
    /// Load the clock-control registers from a flattened-devicetree.
    ///
    /// # Safety
    ///
    /// The `base_addr` must be a valid memory address to clock-control registers.
    pub unsafe fn from_base_addr(base_addr: *const u32) -> Result<Self, ComponentLoadError> {
        let num_links_addr = base_addr;
        Ok(Self {
            num_links: num_links_addr.read_volatile().try_into().unwrap(),
            link_mask: base_addr.add(1),
            finc_fdec: base_addr.add(2).cast_mut(),
            links_stable: base_addr.add(3),
            links_settled: base_addr.add(4),
            data_counts_start: base_addr.add(5).cast(),
        })
    }

    pub fn num_links(&self) -> u8 {
        self.num_links
    }

    pub fn link_mask(&mut self) -> u32 {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe { self.link_mask.read_volatile() }
    }

    pub fn change_speed(&mut self, speed_change: SpeedChange) {
        let n = match speed_change {
            SpeedChange::SpeedUp => 0,
            SpeedChange::SlowDown => 1,
            SpeedChange::NoChange => 2,
        };

        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe {
            self.finc_fdec.write_volatile(n);
        }
    }

    pub fn link_stable(&mut self, link: u8) -> bool {
        let mask = 1u32 << link;
        (self.links_stable() & mask) != 0
    }

    pub fn links_stable(&mut self) -> u32 {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe { self.links_stable.read_volatile() }
    }

    pub fn link_settled(&mut self, link: u8) -> bool {
        let mask = 1u32 << link;
        (self.links_settled() & mask) != 0
    }

    pub fn links_settled(&mut self) -> u32 {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe { self.links_settled.read_volatile() }
    }

    pub fn data_count(&mut self, link: u8) -> Result<i32, OutOfBoundsError> {
        if link >= self.num_links {
            return Err(OutOfBoundsError);
        }

        let data_count = unsafe { self.data_counts_start.add(link as usize).read_volatile() };
        Ok(data_count)
    }
}
