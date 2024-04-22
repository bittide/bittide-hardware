// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

pub struct DataCounts {
    num_links: *const u8,
    data_counts_start: *const i32,
}

impl DataCounts {
    /// Load the domainDiffCounters registers from a flattened-devicetree.
    ///
    /// # Safety
    ///
    /// The `base_addr` must be a valid memory address to dataCount registers.
    pub unsafe fn from_base_addr(base_addr: *const u32) -> Self {
        Self {
            num_links: base_addr.cast::<u8>(),
            data_counts_start: base_addr.add(1).cast(),
        }
    }

    pub fn num_links(&self) -> u8 {
        // SAFETY: This is safe since this function can only be called
        // after construction, which is only valid with valid addresses.
        unsafe { self.num_links.read_volatile() }
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
