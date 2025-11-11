// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
/*! Calendar Interface

The `calendar` module provides a hardware abstraction layer over based on the generated
peripheral access code for the `Calendar` device.
 */

use crate as bittide_hal;
use bittide_hal::shared::types::ValidEntry;
use bittide_macros::Index;

/// Stand-in for `From<usize> + Into<usize> + Into<u128>`
///
/// As it turns out, the primitive numeric types don't implement `From`/`Into` for each
/// other. So there's no `usize: From<u8>` and so on, and there's no trait to constrain on
/// to guarantee that you can `expr as T`. This trait is meant to make that guarantee.
pub trait IndexIConv {
    fn from_usize(val: usize) -> Self;
    fn into_usize(self) -> usize;
    fn into_u128(self) -> u128;
}

macro_rules! impl_indexiconv {
    ($($t:ty),+$(,)?) => {
        $(
            impl IndexIConv for $t {
                fn from_usize(val: usize) -> Self {
                    val as $t
                }

                fn into_usize(self) -> usize {
                    self as usize
                }

                fn into_u128(self) -> u128 {
                    self as u128
                }
            }
        )+
    }
}

impl_indexiconv!(u8, u16, u32, u64, u128, usize);

/// Abstraction trait over `IndexTy` to provide its core functionalities.
pub trait IndexT: Sized {
    const MIN: Self;
    const MAX: Self;

    type Inner: IndexIConv;

    /// `IndexTy::new`
    fn new(val: Self::Inner) -> Option<Self>;
    #[doc(hidden)]
    unsafe fn new_unchecked(n: Self::Inner) -> Self;
    /// `IndexTy::into_underlying`
    fn into_underlying(self) -> Self::Inner;
}

type IndexI<T> = <T as IndexT>::Inner;

macro_rules! impl_index_trait {
    ($($t:ty),+$(,)?) => {
        $(
            impl<const N: u128> IndexT for crate::manual_additions::index::IndexTy<N, $t> {
                const MIN: Self = Self::MIN;
                const MAX: Self = Self::MAX;

                type Inner = $t;

                fn new(val: Self::Inner) -> Option<Self> {
                    Self::new(val)
                }

                unsafe fn new_unchecked(n: Self::Inner) -> Self {
                    Self::new_unchecked(n)
                }

                fn into_underlying(self) -> Self::Inner {
                    self.into_underlying()
                }
            }
        )+
    }
}

impl_index_trait!(u8, u16, u32, u64, u128);

/// Abstraction trait over all the methods that a calendar type should provide
pub trait CalendarInterface {
    type EntryType: Copy;
    type ShadowIndex: IndexT;
    type WriteIndex: IndexT;
    type ReadIndex: IndexT;

    fn shadow_depth_index(&self) -> Self::ShadowIndex;
    fn shadow_entry(&self) -> Self::EntryType;
    fn metacycle_count(&self) -> u32;
    fn set_shadow_depth_index(&self, val: Self::ShadowIndex);
    fn set_write_addr(&self, val: Self::WriteIndex);
    fn set_read_addr(&self, val: Self::ReadIndex);
    fn set_shadow_entry(&self, val: Self::EntryType);
    fn set_swap_active(&self, val: bool);
    fn set_end_of_metacycle(&self, val: bool);
}

/// Type alias for retrieving the entry type of a calendar type.
pub type CalEntry<T> = <T as CalendarInterface>::EntryType;

type CalSII<T> = IndexI<<T as CalendarInterface>::ShadowIndex>;
type CalWII<T> = IndexI<<T as CalendarInterface>::WriteIndex>;
type CalRII<T> = IndexI<<T as CalendarInterface>::ReadIndex>;

macro_rules! impl_calendar_interface {
    (cal: $cty:ty, entry: $et:ty, len: $len:literal$(,)?) => {
        impl CalendarInterface for $cty {
            type EntryType = $et;
            type ShadowIndex = Index![$len];
            type WriteIndex = Index![$len];
            type ReadIndex = Index![$len];

            fn shadow_depth_index(&self) -> Self::ShadowIndex {
                self.shadow_depth_index()
            }

            fn shadow_entry(&self) -> Self::EntryType {
                self.shadow_entry()
            }

            fn metacycle_count(&self) -> u32 {
                self.metacycle_count()
            }

            fn set_shadow_depth_index(&self, val: Self::ShadowIndex) {
                self.set_shadow_depth_index(val)
            }

            fn set_write_addr(&self, val: Self::WriteIndex) {
                self.set_write_addr(val)
            }

            fn set_read_addr(&self, val: Self::ReadIndex) {
                self.set_read_addr(val)
            }

            fn set_shadow_entry(&self, val: Self::EntryType) {
                self.set_shadow_entry(val)
            }

            fn set_swap_active(&self, val: bool) {
                self.set_swap_active(val)
            }

            fn set_end_of_metacycle(&self, val: bool) {
                self.set_end_of_metacycle(val)
            }
        }
    };
}

/// Abstraction over the interface that should be provided by all calendar types.
pub trait Calendar: CalendarInterface {
    /// Reads entry `n` from the shadow calendar.
    ///
    /// # Panics
    ///
    /// Panics if `n` is an invalid value for a `Self::ReadIndex` type instance.
    fn read_shadow_entry(&self, n: usize) -> Self::EntryType {
        let n1 = Self::ReadIndex::new(CalRII::<Self>::from_usize(n)).unwrap();
        self.set_read_addr(n1);
        self.shadow_entry()
    }

    /// Writes entry `entry` to the shadow calendar at address `n`.
    ///
    /// # Panics
    ///
    /// Panics if `n` is an invalid value for a `Self::WriteIndex` type instance.
    fn write_shadow_entry(&self, n: usize, entry: Self::EntryType) {
        let n1 = Self::WriteIndex::new(CalWII::<Self>::from_usize(n)).unwrap();
        self.set_shadow_entry(entry);
        self.set_write_addr(n1);
    }

    /// Swaps the active and shadow calendar at the end of the metacycle.
    fn swap_calendar(&self) {
        self.set_swap_active(true);
    }

    /// Stalls until the end of the metacycle.
    fn wait_for_end_of_metacycle(&self) {
        self.set_end_of_metacycle(true);
    }

    /// Returns the number of entries in the shadow calendar.
    fn shadow_depth(&self) -> usize {
        self.shadow_depth_index().into_underlying().into_usize() + 1
    }

    /// Returns an iterator over the shadow calendar entries.
    fn read_shadow_calendar<'a>(&'a self) -> impl Iterator<Item = Self::EntryType> + 'a {
        (0..self.shadow_depth()).map(move |n| {
            let n1 = unsafe { Self::ReadIndex::new_unchecked(CalRII::<Self>::from_usize(n)) };
            self.set_read_addr(n1);
            self.shadow_entry()
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
            entries.len() as u128 <= Self::ShadowIndex::MAX.into_underlying().into_u128(),
            "Entries exceed shadow calendar size"
        );
        for (n, entry) in entries.iter().enumerate() {
            let n1 = unsafe { Self::WriteIndex::new_unchecked(CalWII::<Self>::from_usize(n)) };
            self.set_shadow_entry(*entry);
            self.set_write_addr(n1);
        }
        self.set_shadow_depth_index(unsafe {
            Self::ShadowIndex::new_unchecked(CalSII::<Self>::from_usize(entries.len() - 1))
        });
    }
}

impl<T: CalendarInterface> Calendar for T {}

impl_calendar_interface! {
    cal: crate::hals::switch_c::devices::SwitchCalendar,
    entry: ValidEntry<[Index![17]; 16]>,
    len: 256,
}

impl_calendar_interface! {
    cal: crate::hals::switch_demo_mu::devices::SwitchCalendar,
    entry: ValidEntry<[Index![9]; 8]>,
    len: 7,
}

impl_calendar_interface! {
    cal: crate::hals::switch_demo_gppe_mu::devices::SwitchCalendar,
    entry: ValidEntry<[Index![9]; 8]>,
    len: 7,
}

impl_calendar_interface! {
    cal: crate::hals::switch_demo_gppe_mu::devices::ScatterCalendar,
    entry: ValidEntry<Index![1024]>,
    len: 1024,
}

impl_calendar_interface! {
    cal: crate::hals::switch_demo_gppe_mu::devices::GatherCalendar,
    entry: ValidEntry<Index![1024]>,
    len: 1024,
}

impl_calendar_interface! {
    cal: crate::hals::scatter_gather_pe::devices::ScatterCalendar,
    entry: ValidEntry<Index![16]>,
    len: 32,
}

impl_calendar_interface! {
    cal: crate::hals::scatter_gather_pe::devices::GatherCalendar,
    entry: ValidEntry<Index![16]>,
    len: 32,
}

impl_calendar_interface! {
    cal: crate::hals::soft_ugn_demo_mu::devices::ScatterCalendar,
    entry: ValidEntry<Index![1024]>,
    len: 1024,
}

impl_calendar_interface! {
    cal: crate::hals::soft_ugn_demo_mu::devices::GatherCalendar,
    entry: ValidEntry<Index![1024]>,
    len: 1024,
}
