// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

pub trait FreezeInterface {
    const EB_COUNTERS_LEN: usize;

    fn freeze_counter(&self) -> u32;
    fn local_clock_counter(&self) -> u64;
    fn number_of_sync_pulses_seen(&self) -> u32;
    fn cycles_since_sync_pulse(&self) -> u32;

    fn eb_counters(&self, idx: usize) -> Option<i32>;
    /// # Safety
    /// `idx` must be less than `EB_COUNTERS_LEN`.
    unsafe fn eb_counters_unchecked(&self, idx: usize) -> i32;
    fn eb_counters_volatile_iter(&self) -> impl DoubleEndedIterator<Item = i32> + '_;

    fn set_freeze(&self, val: ());
}

macro_rules! impl_freeze_interface {
    ($t:ty) => {
        impl FreezeInterface for $t {
            const EB_COUNTERS_LEN: usize = <$t>::EB_COUNTERS_LEN;

            fn freeze_counter(&self) -> u32 {
                <$t>::freeze_counter(self)
            }
            fn local_clock_counter(&self) -> u64 {
                <$t>::local_clock_counter(self)
            }
            fn number_of_sync_pulses_seen(&self) -> u32 {
                <$t>::number_of_sync_pulses_seen(self)
            }
            fn cycles_since_sync_pulse(&self) -> u32 {
                <$t>::cycles_since_sync_pulse(self)
            }

            fn eb_counters(&self, idx: usize) -> Option<i32> {
                <$t>::eb_counters(self, idx)
            }
            unsafe fn eb_counters_unchecked(&self, idx: usize) -> i32 {
                <$t>::eb_counters_unchecked(self, idx)
            }
            fn eb_counters_volatile_iter(&self) -> impl DoubleEndedIterator<Item = i32> + '_ {
                <$t>::eb_counters_volatile_iter(self)
            }

            fn set_freeze(&self, val: ()) {
                <$t>::set_freeze(self, val)
            }
        }
    };
}

impl_freeze_interface!(crate::hals::freeze::devices::Freeze);
impl_freeze_interface!(crate::hals::soft_ugn_demo_cc::devices::Freeze);
impl_freeze_interface!(crate::hals::switch_demo_cc::devices::Freeze);
impl_freeze_interface!(crate::hals::switch_demo_gppe_cc::devices::Freeze);
impl_freeze_interface!(crate::hals::wire_demo_cc::devices::Freeze);
