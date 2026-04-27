// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use crate::types::cc_conf::CcConf;
use crate::types::speed_change::SpeedChange;

pub trait ClockControlInterface {
    const DATA_COUNTS_LEN: usize;

    fn n_links(&self) -> u8;
    fn link_mask(&self) -> [u8; 1];
    fn link_mask_pop_count(&self) -> u8;
    fn link_mask_rev(&self) -> [u8; 1];
    fn links_ok(&self) -> [u8; 1];
    fn links_stable(&self) -> [u8; 1];
    fn links_settled(&self) -> [u8; 1];

    fn min_data_counts_seen(&self, idx: usize) -> Option<i32>;
    /// # Safety
    /// `idx` must be less than `DATA_COUNTS_LEN`.
    unsafe fn min_data_counts_seen_unchecked(&self, idx: usize) -> i32;
    fn min_data_counts_seen_volatile_iter(&self) -> impl DoubleEndedIterator<Item = i32> + '_;

    fn max_data_counts_seen(&self, idx: usize) -> Option<i32>;
    /// # Safety
    /// `idx` must be less than `DATA_COUNTS_LEN`.
    unsafe fn max_data_counts_seen_unchecked(&self, idx: usize) -> i32;
    fn max_data_counts_seen_volatile_iter(&self) -> impl DoubleEndedIterator<Item = i32> + '_;

    fn data_counts(&self, idx: usize) -> Option<i32>;
    /// # Safety
    /// `idx` must be less than `DATA_COUNTS_LEN`.
    unsafe fn data_counts_unchecked(&self, idx: usize) -> i32;
    fn data_counts_volatile_iter(&self) -> impl DoubleEndedIterator<Item = i32> + '_;

    fn config(&self) -> CcConf<[u8; 1]>;

    fn set_change_speed(&self, val: SpeedChange);
    fn set_links_stable(&self, val: [u8; 1]);
    fn set_links_settled(&self, val: [u8; 1]);
    fn set_clear_data_counts_seen(&self, val: bool);
    fn set_config(&self, val: CcConf<[u8; 1]>);
}

macro_rules! impl_clock_control_interface {
    ($cc:ty) => {
        impl ClockControlInterface for $cc {
            const DATA_COUNTS_LEN: usize = <$cc>::DATA_COUNTS_LEN;

            fn n_links(&self) -> u8 {
                <$cc>::n_links(self)
            }
            fn link_mask(&self) -> [u8; 1] {
                <$cc>::link_mask(self)
            }
            fn link_mask_pop_count(&self) -> u8 {
                <$cc>::link_mask_pop_count(self)
            }
            fn link_mask_rev(&self) -> [u8; 1] {
                <$cc>::link_mask_rev(self)
            }
            fn links_ok(&self) -> [u8; 1] {
                <$cc>::links_ok(self)
            }
            fn links_stable(&self) -> [u8; 1] {
                <$cc>::links_stable(self)
            }
            fn links_settled(&self) -> [u8; 1] {
                <$cc>::links_settled(self)
            }

            fn min_data_counts_seen(&self, idx: usize) -> Option<i32> {
                <$cc>::min_data_counts_seen(self, idx)
            }
            unsafe fn min_data_counts_seen_unchecked(&self, idx: usize) -> i32 {
                <$cc>::min_data_counts_seen_unchecked(self, idx)
            }
            fn min_data_counts_seen_volatile_iter(
                &self,
            ) -> impl DoubleEndedIterator<Item = i32> + '_ {
                <$cc>::min_data_counts_seen_volatile_iter(self)
            }

            fn max_data_counts_seen(&self, idx: usize) -> Option<i32> {
                <$cc>::max_data_counts_seen(self, idx)
            }
            unsafe fn max_data_counts_seen_unchecked(&self, idx: usize) -> i32 {
                <$cc>::max_data_counts_seen_unchecked(self, idx)
            }
            fn max_data_counts_seen_volatile_iter(
                &self,
            ) -> impl DoubleEndedIterator<Item = i32> + '_ {
                <$cc>::max_data_counts_seen_volatile_iter(self)
            }

            fn data_counts(&self, idx: usize) -> Option<i32> {
                <$cc>::data_counts(self, idx)
            }
            unsafe fn data_counts_unchecked(&self, idx: usize) -> i32 {
                <$cc>::data_counts_unchecked(self, idx)
            }
            fn data_counts_volatile_iter(&self) -> impl DoubleEndedIterator<Item = i32> + '_ {
                <$cc>::data_counts_volatile_iter(self)
            }

            fn config(&self) -> CcConf<[u8; 1]> {
                <$cc>::config(self)
            }

            fn set_change_speed(&self, val: SpeedChange) {
                <$cc>::set_change_speed(self, val)
            }
            fn set_links_stable(&self, val: [u8; 1]) {
                <$cc>::set_links_stable(self, val)
            }
            fn set_links_settled(&self, val: [u8; 1]) {
                <$cc>::set_links_settled(self, val)
            }
            fn set_clear_data_counts_seen(&self, val: bool) {
                <$cc>::set_clear_data_counts_seen(self, val)
            }
            fn set_config(&self, val: CcConf<[u8; 1]>) {
                <$cc>::set_config(self, val)
            }
        }
    };
}

impl_clock_control_interface!(crate::hals::clock_control_wb::devices::ClockControl);
impl_clock_control_interface!(crate::hals::soft_ugn_demo_cc::devices::ClockControl);
impl_clock_control_interface!(crate::hals::soft_ugn_demo_mu::devices::ClockControl);
impl_clock_control_interface!(crate::hals::switch_demo_cc::devices::ClockControl);
impl_clock_control_interface!(crate::hals::switch_demo_mu::devices::ClockControl);
impl_clock_control_interface!(crate::hals::switch_demo_gppe_cc::devices::ClockControl);
impl_clock_control_interface!(crate::hals::switch_demo_gppe_mu::devices::ClockControl);
impl_clock_control_interface!(crate::hals::wire_demo_cc::devices::ClockControl);
impl_clock_control_interface!(crate::hals::wire_demo_mu::devices::ClockControl);
