// SPDX-FileCopyrightText: 2023 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#[derive(ufmt::derive::uDebug, Copy, Clone)]
#[repr(u8)]
pub enum SpeedChange {
    /// Increases clock speed.
    SpeedUp = 2,
    /// Decreases clock speed.
    SlowDown = 1,
    /// Keeps the clock as it is.
    NoChange = 0,
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

//          Field name |     Type | Byte offset | Notes
//           num_links |       u8 |           0 |
//           link_mask |      u32 |           4 |
//    link_mask_popcnt |      u32 |           8 |
//   reframing_enabled |      u32 |          12 |
//           finc_fdec |      u32 |          16 | write `SpeedChange` values
//        links_stable |      u32 |          20 |
//       links_settled |      u32 |          24 |
//   data_counts_start | [i32; N] |          28 | masked by `link_mask`
pub struct ClockControl {
    base_addr: *const u32,
}

impl ClockControl {
    // Offsets for the fields from the base address.
    const LINK_MASK: usize = 1;
    const UP_LINKS: usize = 2;
    const REFRAMING_ENABLED: usize = 3;
    const CHANGE_SPEED: usize = 4;
    const LINKS_STABLE: usize = 5;
    const LINKS_SETTLED: usize = 6;
    const DATA_COUNTS: usize = 7;

    /// Load the clock-control registers from a flattened-devicetree.
    ///
    /// # Safety
    ///
    /// The `base_addr` must be a valid memory address to clock-control registers.
    pub unsafe fn from_base_addr(base_addr: *const u32) -> Self {
        Self { base_addr }
    }

    pub fn num_links(&self) -> u8 {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe { self.base_addr.cast::<u8>().read_volatile() }
    }

    pub fn link_mask(&self) -> u32 {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe { self.base_addr.add(Self::LINK_MASK).read_volatile() }
    }

    pub fn up_links(&self) -> u8 {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe {
            self.base_addr
                .add(Self::UP_LINKS)
                .cast::<u8>()
                .read_volatile()
        }
    }

    pub fn reframing_enabled(&self) -> bool {
        unsafe { self.base_addr.add(Self::REFRAMING_ENABLED).read_volatile() != 0 }
    }

    pub fn change_speed(&mut self, speed_change: SpeedChange) {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe {
            self.base_addr
                .add(Self::CHANGE_SPEED)
                .cast_mut()
                .write_volatile(speed_change as u32);
        }
    }

    pub fn link_stable(&self, link: u8) -> bool {
        let mask = 1u32 << link;
        (self.links_stable() & mask) != 0
    }

    pub fn links_stable(&self) -> u32 {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe { self.base_addr.add(Self::LINKS_STABLE).read_volatile() }
    }

    pub fn link_settled(&self, link: u8) -> bool {
        let mask = 1u32 << link;
        (self.links_settled() & mask) != 0
    }

    pub fn links_settled(&self) -> u32 {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe { self.base_addr.add(Self::LINKS_SETTLED).read_volatile() }
    }

    pub fn data_counts(&self) -> impl Iterator<Item = i32> + '_ {
        let n = self.num_links();
        // NOTE: Consider fixed length loop version?
        // TODO: Mask out values read whose bit is not set in `link_mask()`.
        (Self::DATA_COUNTS..Self::DATA_COUNTS + n as usize)
            .map(|i| unsafe { self.base_addr.add(i as usize).cast::<i32>().read_volatile() })
    }
}
