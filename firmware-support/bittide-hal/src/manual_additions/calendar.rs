// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
/*! Calendar Interface

The `calendar` module provides a hardware abstraction layer over based on the generated
peripheral access code for the `Calendar` device.
 */

use crate::manual_additions::{
    index::{Index, IndexInner, IndexInterface, IndexSizeCheck},
    unsigned::{Unsigned, UnsignedInterface, UnsignedSizeCheck},
    FromAs, IntoAs,
};
use bittide_macros::{Index, Unsigned};

pub trait ValidEntryType {
    type Inner;
    type Repeat;

    fn new(entry: Self::Inner, repeat: Self::Repeat) -> Self;
}

pub type ValidEntryInner<T> = <T as ValidEntryType>::Inner;
pub type ValidEntryRepeat<T> = <T as ValidEntryType>::Repeat;

macro_rules! impl_valid_entry_type {
    (
        type: [$($valid:tt)+],
        repeat: $repeat:ty,
        mask: $mask:literal,
    ) => {
        impl<T> ValidEntryType for $($valid)+<T> {
            type Inner = T;
            type Repeat = $repeat;

            #[inline]
            fn new(entry: Self::Inner, repeat: Self::Repeat) -> Self {
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
    repeat: Unsigned!(16),
    mask: 12,
}

impl_valid_entry_type! {
    type: [crate::types::ValidEntry_16],
    repeat: Unsigned!(16),
    mask: 16,
}

/// Abstraction trait over all the methods that a calendar type should provide
pub trait CalendarInterface {
    type EntryType: Copy + ValidEntryType;
    type MetacycleCount: UnsignedInterface;
    type Index: IndexInterface<Inner: FromAs<usize> + PartialOrd> + IntoAs<usize>;

    fn calint_shadow_depth_index(&self) -> Self::Index;
    fn calint_shadow_entry(&self) -> Self::EntryType;
    fn calint_metacycle_count(&self) -> Self::MetacycleCount;
    fn calint_set_shadow_depth_index(&self, val: Self::Index);
    fn calint_set_write_addr(&self, val: Self::Index);
    fn calint_set_read_addr(&self, val: Self::Index);
    fn calint_set_shadow_entry(&self, val: Self::EntryType);
    fn calint_set_swap_active(&self, val: bool);
    fn calint_set_end_of_metacycle(&self, val: bool);
}

/// Type alias for retrieving the entry type of a calendar type.
pub type CalendarEntryType<T> = <T as CalendarInterface>::EntryType;

/// Type alias for retrieving the metacycle count type of a calendar type.
pub type CalendarMetacycleCount<T> = <T as CalendarInterface>::MetacycleCount;

/// Type alias for retrieving the index type of a calendar type.
pub type CalendarIndex<T> = <T as CalendarInterface>::Index;

macro_rules! impl_calendar_interface {
    (
        cal: $cty:ty,
        metacycle: $mc:ty,
        index: $ix:ty,
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
            if (<$cty>::SHADOW_DEPTH_INDEX_SIZE as u128 - 1) > (<$ix>::MAX as u128) {
                const_panic::concat_panic!(
                    "Shadow index type ",
                    stringify!($si),
                    " cannot fit the maximum value ",
                    <$cty>::SHADOW_DEPTH_INDEX_SIZE as u128 - 1,
                );
            }
        };
        impl CalendarInterface for $cty {
            type EntryType = $et;
            type MetacycleCount = $mc;
            type Index = $ix;

            #[inline]
            fn calint_shadow_depth_index(&self) -> Self::Index {
                self.shadow_depth_index()
            }

            #[inline]
            fn calint_shadow_entry(&self) -> Self::EntryType {
                self.shadow_entry()
            }

            #[inline]
            fn calint_metacycle_count(&self) -> Self::MetacycleCount {
                self.metacycle_count()
            }

            #[inline]
            fn calint_set_shadow_depth_index(&self, val: Self::Index) {
                self.set_shadow_depth_index(val)
            }

            #[inline]
            fn calint_set_write_addr(&self, val: Self::Index) {
                self.set_write_addr(val)
            }

            #[inline]
            fn calint_set_read_addr(&self, val: Self::Index) {
                self.set_read_addr(val)
            }

            #[inline]
            fn calint_set_shadow_entry(&self, val: Self::EntryType) {
                self.set_shadow_entry(val)
            }

            #[inline]
            fn calint_set_swap_active(&self, val: bool) {
                self.set_swap_active(val)
            }

            #[inline]
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
        let n_inner = n.into_as();
        assert!(
            n_inner <= Self::Index::MAX,
            "Shadow entry read index is out of bounds."
        );
        self.calint_set_read_addr(unsafe { Self::Index::idx_new_unchecked(n_inner) });
        self.calint_shadow_entry()
    }

    /// Writes entry `entry` to the shadow calendar at address `n`.
    ///
    /// # Panics
    ///
    /// Panics if `n` is an invalid value for a `Self::WriteIndex` type instance.
    fn write_shadow_entry(&self, n: usize, entry: Self::EntryType) {
        let n_inner = n.into_as();
        assert!(
            n_inner <= Self::Index::MAX,
            "Shadow entry write index is out of bounds."
        );
        self.calint_set_shadow_entry(entry);
        self.calint_set_write_addr(unsafe { Self::Index::idx_new_unchecked(n_inner) });
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
        self.calint_shadow_depth_index().into_as() + 1
    }

    /// Returns an iterator over the shadow calendar entries.
    fn read_shadow_calendar<'a>(&'a self) -> impl Iterator<Item = Self::EntryType> + 'a {
        (0..self.shadow_depth()).map(|n| {
            self.calint_set_read_addr(unsafe { Self::Index::idx_new_unchecked(n.into_as()) });
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
            entries.len() <= Self::Index::IMAX.into_as(),
            "Input slice (length {}) is too long for calendar of size {}",
            entries.len(),
            Self::Index::N_,
        );
        for (n, entry) in entries.iter().enumerate() {
            self.calint_set_shadow_entry(*entry);
            self.calint_set_write_addr(unsafe { Self::Index::idx_new_unchecked(n.into_as()) });
        }
        self.calint_set_shadow_depth_index(unsafe {
            Self::Index::idx_new_unchecked((entries.len() - 1).into_as())
        });
    }
}

impl<T> CalendarType for T where T: CalendarInterface {}

impl_calendar_interface! {
    cal: crate::hals::switch_c::devices::Calendar,
    metacycle: Unsigned!(32),
    index: Index!(256),
    entry: crate::types::ValidEntry_12<[Index!(17); 16]>,
}

impl_calendar_interface! {
    cal: crate::hals::switch_demo_mu::devices::Calendar,
    metacycle: Unsigned!(32),
    index: Index!(7),
    entry: crate::types::ValidEntry_12<[Index!(9); 8]>,
}

impl_calendar_interface! {
    cal: crate::hals::switch_demo_gppe_mu::devices::Calendar,
    metacycle: Unsigned!(32),
    index: Index!(1024),
    entry: crate::types::ValidEntry_12<Index!(1024)>,
}

impl_calendar_interface! {
    cal: crate::hals::scatter_gather_pe::devices::Calendar,
    metacycle: Unsigned!(32),
    index: Index!(32),
    entry: crate::types::ValidEntry_12<Index!(16)>,
}

impl_calendar_interface! {
    cal: crate::hals::soft_ugn_demo_mu::devices::Calendar,
    metacycle: Unsigned!(32),
    index: Index!(4000),
    entry: crate::types::ValidEntry_16<Index!(4000)>,
}

pub trait RingbufferCalendar {
    fn initialize_as_ringbuffer(&self, size: usize);
}

impl<T> RingbufferCalendar for T
where
    T: CalendarType,
    core::ops::Range<IndexInner<T::Index>>: IntoIterator<Item = IndexInner<T::Index>>,
    ValidEntryInner<CalendarEntryType<T>>: From<T::Index>,
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
        let size_max_index: IndexInner<T::Index> = (size - 1).into_as();
        if <usize as IntoAs<IndexInner<T::Index>>>::into_as(size) > T::Index::N_AS_INNER {
            panic!("Size ({size}) exceeds calendar size ({})", T::Index::N_);
        }
        for (n, idx) in (IndexInner::<T::Index>::from_as(0usize)..T::Index::N_AS_INNER)
            .into_iter()
            .map(|n| unsafe { T::Index::idx_new_unchecked(n) })
            .enumerate()
        {
            let entry = CalendarEntryType::<Self>::new(idx.into(), 0.into_as());
            self.write_shadow_entry(n, entry);
        }
        self.calint_set_shadow_depth_index(unsafe { T::Index::idx_new_unchecked(size_max_index) });
        self.calint_set_swap_active(true);
    }
}
