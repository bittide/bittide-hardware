// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
/*! Calendar Interface

The `calendar` module provides a hardware abstraction layer over based on the generated
peripheral access code for the `Calendar` device.
 */

use crate::manual_additions::{FromAs, IntoAs};

pub trait ValidEntryType {
    type Inner;
    type Repeat;

    const REPEAT_MASK: Self::Repeat;

    fn new(entry: Self::Inner, repeat: Self::Repeat) -> Self;
}

pub type ValidEntryInner<T> = <T as ValidEntryType>::Inner;
pub type ValidEntryRepeat<T> = <T as ValidEntryType>::Repeat;

pub const fn valid_entry_repeat_mask<T: ValidEntryType>() -> ValidEntryRepeat<T> {
    <T as ValidEntryType>::REPEAT_MASK
}

macro_rules! impl_valid_entry_type {
    (
        type: [$($valid:tt)+],
        repeat: $repeat:ty,
        mask: $mask:literal,
    ) => {
        impl<T> ValidEntryType for $($valid)+<T> {
            type Inner = T;
            type Repeat = $repeat;

            const REPEAT_MASK: Self::Repeat = $mask as $repeat;

            fn new(entry: Self::Inner, repeat: Self::Repeat) -> Self {
                let repeat = repeat & Self::REPEAT_MASK;
                Self {
                    ve_entry: entry,
                    ve_repeat: repeat,
                }
            }
        }
    }
}

impl_valid_entry_type! {
    type: [crate::types::ValidEntry_12],
    repeat: u16,
    mask: 12,
}

impl_valid_entry_type! {
    type: [crate::types::ValidEntry_16],
    repeat: u16,
    mask: 16,
}

/// Abstraction trait over all the methods that a calendar type should provide
pub trait CalendarInterface {
    type EntryType: Copy + ValidEntryType;
    type MetacycleCount;

    type ShadowIndex: Ord + FromAs<usize> + IntoAs<usize> + IntoAs<u128>;
    const SHADOW_INDEX_MAX: Self::ShadowIndex;

    type WriteIndex: Ord + FromAs<usize> + IntoAs<u128>;
    const WRITE_ADDR_MAX: Self::WriteIndex;

    type ReadIndex: Ord + FromAs<usize> + IntoAs<u128>;
    const READ_ADDR_MAX: Self::ReadIndex;

    fn calint_shadow_depth_index(&self) -> Self::ShadowIndex;
    fn calint_shadow_entry(&self) -> Self::EntryType;
    fn calint_metacycle_count(&self) -> u32;
    fn calint_set_shadow_depth_index(&self, val: Self::ShadowIndex);
    fn calint_set_write_addr(&self, val: Self::WriteIndex);
    fn calint_set_read_addr(&self, val: Self::ReadIndex);
    fn calint_set_shadow_entry(&self, val: Self::EntryType);
    fn calint_set_swap_active(&self, val: bool);
    fn calint_set_end_of_metacycle(&self, val: bool);
}

/// Type alias for retrieving the entry type of a calendar type.
pub type CalendarEntryType<T> = <T as CalendarInterface>::EntryType;

/// Type alias for retrieving the metacycle count type of a calendar type.
pub type CalendarMetacycleCount<T> = <T as CalendarInterface>::MetacycleCount;

/// Type alias for retrieving the shadow index type of a calendar type.
pub type CalendarShadowIndex<T> = <T as CalendarInterface>::ShadowIndex;

/// Type alias for retrieving the write index type of a calendar type.
pub type CalendarWriteIndex<T> = <T as CalendarInterface>::WriteIndex;

/// Type alias for retrieving the read index type of a calendar type.
pub type CalendarReadIndex<T> = <T as CalendarInterface>::ReadIndex;

macro_rules! impl_calendar_interface {
    (
        cal: $cty:ty,
        metacycle: $mc:ty,
        shadow: $si:ty,
        write: $wi:ty,
        read: $ri:ty,
        entry: $et:ty,
    ) => {
        // Assertions that must be met for the implementation to be valid
        const _: () = {
            // Assert that the metacycle count type is minimally sized
            let mc_min_bytes = <$cty>::METACYCLE_COUNT_WIDTH
                .div_ceil(8)
                .next_power_of_two();
            if mc_min_bytes as usize * 8 != <$mc>::BITS as usize {
                const_panic::concat_panic!(
                    "Metacycle count type for type ",
                    stringify!($cty),
                    " is not minimally sized. Should be of type u",
                    <$cty>::METACYCLE_COUNT_WIDTH.next_power_of_two(),
                );
            }
            // Assert that the shadow index type can hold the maximum shadow depth index
            if (<$cty>::SHADOW_DEPTH_INDEX_SIZE as u128 - 1) > (<$si>::MAX as u128) {
                const_panic::concat_panic!(
                    "Shadow index type ",
                    stringify!($si),
                    " cannot fit the maximum value ",
                    <$cty>::SHADOW_DEPTH_INDEX_SIZE as u128 - 1,
                );
            }
            // Assert that the write index type can hold the maximum write address
            if <$cty>::WRITE_ADDR_SIZE as u128 - 1 > <$wi>::MAX as u128 {
                const_panic::concat_panic!(
                    "Write index type ",
                    stringify!($wi),
                    " cannot fit the maximum value ",
                    <$cty>::WRITE_ADDR_SIZE as u128 - 1,
                );
            }
            // Assert that the read index type can hold the maximum read address
            if <$cty>::READ_ADDR_SIZE as u128 - 1 > <$ri>::MAX as u128 {
                const_panic::concat_panic!(
                    "Read index type ",
                    stringify!($ri),
                    " cannot fit the maximum value ",
                    <$cty>::READ_ADDR_SIZE as u128 - 1,
                );
            }
        };
        impl CalendarInterface for $cty {
            type EntryType = $et;
            type MetacycleCount = $mc;

            type ShadowIndex = $si;
            const SHADOW_INDEX_MAX: $si = (<$cty>::SHADOW_DEPTH_INDEX_SIZE - 1) as $si;

            type WriteIndex = $wi;
            const WRITE_ADDR_MAX: $wi = (<$cty>::WRITE_ADDR_SIZE - 1) as $wi;

            type ReadIndex = $ri;
            const READ_ADDR_MAX: $ri = (<$cty>::READ_ADDR_SIZE - 1) as $ri;

            fn calint_shadow_depth_index(&self) -> Self::ShadowIndex {
                self.shadow_depth_index()
            }

            fn calint_shadow_entry(&self) -> Self::EntryType {
                self.shadow_entry()
            }

            fn calint_metacycle_count(&self) -> Self::MetacycleCount {
                self.metacycle_count()
            }

            fn calint_set_shadow_depth_index(&self, val: Self::ShadowIndex) {
                self.set_shadow_depth_index(val)
            }

            fn calint_set_write_addr(&self, val: Self::WriteIndex) {
                self.set_write_addr(val)
            }

            fn calint_set_read_addr(&self, val: Self::ReadIndex) {
                self.set_read_addr(val)
            }

            fn calint_set_shadow_entry(&self, val: Self::EntryType) {
                self.set_shadow_entry(val)
            }

            fn calint_set_swap_active(&self, val: bool) {
                self.set_swap_active(val)
            }

            fn calint_set_end_of_metacycle(&self, val: bool) {
                self.set_end_of_metacycle(val)
            }
        }
    };
}

/// Abstraction over the interface that should be provided by all calendar types.
pub trait CalendarType: CalendarInterface {
    /// Reads entry `n` from the shadow calendar.
    ///
    /// # Panics
    ///
    /// Panics if `n` is an invalid value for a `Self::ReadIndex` type instance.
    fn read_shadow_entry(&self, n: usize) -> Self::EntryType {
        assert!(
            n as u128 <= Self::READ_ADDR_MAX.into_as(),
            "Shadow entry read index is out of bounds."
        );
        self.calint_set_read_addr(Self::ReadIndex::from_as(n));
        self.calint_shadow_entry()
    }

    /// Writes entry `entry` to the shadow calendar at address `n`.
    ///
    /// # Panics
    ///
    /// Panics if `n` is an invalid value for a `Self::WriteIndex` type instance.
    fn write_shadow_entry(&self, n: usize, entry: Self::EntryType) {
        assert!(
            n as u128 <= Self::WRITE_ADDR_MAX.into_as(),
            "Shadow entry write index is out of bounds."
        );
        self.calint_set_shadow_entry(entry);
        self.calint_set_write_addr(Self::WriteIndex::from_as(n));
    }

    /// Swaps the active and shadow calendar at the end of the metacycle.
    fn swap_calendar(&self) {
        self.calint_set_swap_active(true);
    }

    /// Stalls until the end of the metacycle.
    fn wait_for_end_of_metacycle(&self) {
        self.calint_set_end_of_metacycle(true);
    }

    /// Returns the number of entries in the shadow calendar.
    fn shadow_depth(&self) -> usize {
        <Self::ShadowIndex as IntoAs<usize>>::into_as(self.calint_shadow_depth_index()) + 1
    }

    /// Returns an iterator over the shadow calendar entries.
    fn read_shadow_calendar<'a>(&'a self) -> impl Iterator<Item = Self::EntryType> + 'a {
        (0..self.shadow_depth()).map(|n| {
            self.calint_set_read_addr(Self::ReadIndex::from_as(n));
            self.calint_shadow_entry()
        })
    }

    /// Writes a calendar to the shadow calendar.
    ///
    /// # Panics
    ///
    /// Panics if the length of the slice passed to this method is longer than the length
    /// of the shadow calendar.
    fn write_shadow_calendar(&self, entries: &[Self::EntryType]) {
        if entries.is_empty() {
            return;
        }
        assert!(
            entries.len() as u128
                <= <Self::ShadowIndex as IntoAs<u128>>::into_as(Self::SHADOW_INDEX_MAX),
            "Entries exceed shadow calendar size"
        );
        for (n, entry) in entries.iter().enumerate() {
            self.calint_set_shadow_entry(*entry);
            self.calint_set_write_addr(Self::WriteIndex::from_as(n));
        }
        self.calint_set_shadow_depth_index(Self::ShadowIndex::from_as(entries.len() - 1));
    }
}

impl<T: CalendarInterface> CalendarType for T {}

impl_calendar_interface! {
    cal: crate::hals::switch_c::devices::Calendar,
    metacycle: u32,
    shadow: u8,
    write: u8,
    read: u8,
    entry: crate::types::ValidEntry_12<[u8; 16]>,
}

impl_calendar_interface! {
    cal: crate::hals::switch_demo_mu::devices::Calendar,
    metacycle: u32,
    shadow: u8,
    write: u8,
    read: u8,
    entry: crate::types::ValidEntry_12<[u8; 8]>,
}

impl_calendar_interface! {
    cal: crate::hals::switch_demo_gppe_mu::devices::Calendar,
    metacycle: u32,
    shadow: u16,
    write: u16,
    read: u16,
    entry: crate::types::ValidEntry_12<u16>,
}

impl_calendar_interface! {
    cal: crate::hals::scatter_gather_pe::devices::Calendar,
    metacycle: u32,
    shadow: u8,
    write: u8,
    read: u8,
    entry: crate::types::ValidEntry_12<u8>,
}

impl_calendar_interface! {
    cal: crate::hals::soft_ugn_demo_mu::devices::Calendar,
    metacycle: u32,
    shadow: u16,
    write: u16,
    read: u16,
    entry: crate::types::ValidEntry_16<u16>,
}

pub trait RingbufferCalendar {
    fn initialize_as_ringbuffer(&self, size: usize);
}

impl<T> RingbufferCalendar for T
where
    T: CalendarType,
    ValidEntryInner<CalendarEntryType<T>>: FromAs<usize>,
    ValidEntryRepeat<CalendarEntryType<T>>: FromAs<u8>,
{
    /// Initializes a calendar type which contains entries derivable from `usize` and repeats
    /// derivable from `u8` as a ringbuffer.
    ///
    /// # Panics
    ///
    /// Panics if:
    /// - Attempting to initialize a ringbuffer of size 0
    /// - Attempting to initialize a ringbuffer of a size greater than the maximum size of the
    ///   calendar (compared against [`Self::SHADOW_INDEX_MAX`])
    fn initialize_as_ringbuffer(&self, size: usize) {
        assert!(size > 0, "Cannot have a ringbuffer of size 0!");
        let size_max_index: CalendarShadowIndex<T> = (size - 1).into_as();
        if size_max_index > Self::SHADOW_INDEX_MAX {
            panic!(
                "Size ({size}) exceeds calendar size ({})",
                <CalendarShadowIndex<T> as IntoAs<u128>>::into_as(Self::SHADOW_INDEX_MAX)
            );
        }
        for n in 0..size {
            let entry = CalendarEntryType::<Self>::new(n.into_as(), 0.into_as());
            self.write_shadow_entry(n, entry);
        }
        self.calint_set_shadow_depth_index(size_max_index);
        self.calint_set_swap_active(true);
    }
}
