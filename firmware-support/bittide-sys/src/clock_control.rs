// SPDX-FileCopyrightText: 2023 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#[derive(ufmt::derive::uDebug, Copy, Clone)]
pub enum SpeedChange {
    /// Increases clock speed.
    SpeedUp,
    /// Decreases clock speed.
    SlowDown,
    /// Keeps the clock as it is.
    NoChange,
}

impl SpeedChange {
    pub fn sign(&self) -> i32 {
        match &self {
            SpeedChange::SpeedUp => 1,
            SpeedChange::NoChange => 0,
            SpeedChange::SlowDown => -1,
        }
    }
}

pub struct ClockControl {
    num_links: *const u8,
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
    pub unsafe fn from_base_addr(base_addr: *const u32) -> Self {
        Self {
            num_links: base_addr.cast::<u8>(),
            link_mask: base_addr.add(1),
            finc_fdec: base_addr.add(2).cast_mut(),
            links_stable: base_addr.add(3),
            links_settled: base_addr.add(4),
            data_counts_start: base_addr.add(5).cast(),
        }
    }

    pub fn num_links(&self) -> u8 {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe { self.num_links.read_volatile() }
    }

    pub fn link_mask(&self) -> u32 {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe { self.link_mask.read_volatile() }
    }

    pub fn change_speed(&mut self, speed_change: SpeedChange) {
        let n = match speed_change {
            SpeedChange::NoChange => 0,
            SpeedChange::SlowDown => 1,
            SpeedChange::SpeedUp => 2,
        };

        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe {
            self.finc_fdec.write_volatile(n);
        }
    }

    pub fn link_stable(&self, link: u8) -> bool {
        let mask = 1u32 << link;
        (self.links_stable() & mask) != 0
    }

    pub fn links_stable(&self) -> u32 {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe { self.links_stable.read_volatile() }
    }

    pub fn link_settled(&self, link: u8) -> bool {
        let mask = 1u32 << link;
        (self.links_settled() & mask) != 0
    }

    pub fn links_settled(&self) -> u32 {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe { self.links_settled.read_volatile() }
    }

    pub fn data_counts(&self) -> impl Iterator<Item = i32> + '_ {
        let n = self.num_links();
        let mut i = 0;
        core::iter::from_fn(move || {
            if i == n {
                None
            } else {
                let count = unsafe { self.data_counts_start.add(i as usize).read_volatile() };
                i += 1;
                Some(count)
            }
        })
    }
}
