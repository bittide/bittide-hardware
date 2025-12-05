// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
/*! Calendar Interface
 *
The `calendar` module provides a hardware abstraction layer over based on the generated
peripheral access code for the `Calendar` device.
 */

use crate::{self as bittide_hal, hals};

use bittide_hal::{
    hals::switch_c::devices::calendar::Calendar,
    types::{ValidEntry_12, ValidEntry_16},
};

pub type EntryType = ValidEntry_12<[u8; 16]>;

/// Generic Calendar implementations
impl Calendar {
    /// Reads entry n from the shadow calendar.
    pub fn read_shadow_entry(&self, n: usize) -> EntryType {
        self.set_read_addr(n as _);
        self.shadow_entry()
    }

    /// Writes entry `entry` to the shadow calendar at address `n`.
    pub fn write_shadow_entry(&self, n: usize, entry: EntryType) {
        let n1 = n as u8;
        self.set_shadow_entry(entry);
        self.set_write_addr(n1);
    }

    /// Swaps the active and shadow calendar at the end of the metacycle.
    pub fn swap_calendar(&self) {
        self.set_swap_active(true);
    }

    /// Stalls until the end of the metacycle.
    pub fn wait_for_end_of_metacycle(&self) {
        self.set_end_of_metacycle(true);
    }

    /// Returns the number of entries in the shadow calendar.
    pub fn shadow_depth(&self) -> usize {
        self.shadow_depth_index() as usize + 1
    }

    /// Returns an iterator over the shadow calendar entries.
    pub fn read_shadow_calendar<'a>(&'a self) -> impl Iterator<Item = EntryType> + 'a {
        (0..self.shadow_depth()).map(move |n| {
            let n1 = n as u8;
            self.set_read_addr(n1);
            self.shadow_entry()
        })
    }

    /// Writes a calendar to the shadow calendar.
    pub fn write_shadow_calendar(&self, entries: &[EntryType]) {
        assert!(
            entries.len() <= Self::SHADOW_DEPTH_INDEX_SIZE,
            "Entries exceed shadow calendar size"
        );
        for (n, entry) in entries.iter().enumerate() {
            let n1 = n as u8;
            self.set_shadow_entry(*entry);
            self.set_write_addr(n1);
        }
        self.set_shadow_depth_index((entries.len() - 1) as u8);
    }
}

type RingbufferCalendar = hals::soft_ugn_demo_mu::devices::calendar::Calendar;

/// Ringbuffer related calendar implementations
impl RingbufferCalendar {
    pub fn initialize_as_ringbuffer(&self, size: usize) {
        assert!(
            size <= Self::SHADOW_DEPTH_INDEX_SIZE,
            "Size exceeds shadow calendar size"
        );
        for n in 0..size {
            let entry = ValidEntry_16 {
                ve_entry: n as u16,
                ve_repeat: 0,
            };
            self.set_shadow_entry(entry);
            self.set_write_addr(n as u16);
        }
        self.set_shadow_depth_index((size - 1) as u16);
        self.set_swap_active(true);
    }
}
