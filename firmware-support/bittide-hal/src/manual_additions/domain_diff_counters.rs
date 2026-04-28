// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

pub trait DomainDiffCountersInterface {
    const ENABLE_LEN: usize;

    fn enable(&self, idx: usize) -> Option<bool>;
    /// # Safety
    /// `idx` must be less than `ENABLE_LEN`.
    unsafe fn enable_unchecked(&self, idx: usize) -> bool;

    fn set_enable(&self, idx: usize, val: bool) -> Option<()>;
    /// # Safety
    /// `idx` must be less than `ENABLE_LEN`.
    unsafe fn set_enable_unchecked(&self, idx: usize, val: bool);
}

macro_rules! impl_domain_diff_counters_interface {
    ($t:ty) => {
        impl DomainDiffCountersInterface for $t {
            const ENABLE_LEN: usize = <$t>::ENABLE_LEN;

            fn enable(&self, idx: usize) -> Option<bool> {
                <$t>::enable(self, idx)
            }
            unsafe fn enable_unchecked(&self, idx: usize) -> bool {
                <$t>::enable_unchecked(self, idx)
            }

            fn set_enable(&self, idx: usize, val: bool) -> Option<()> {
                <$t>::set_enable(self, idx, val)
            }
            unsafe fn set_enable_unchecked(&self, idx: usize, val: bool) {
                <$t>::set_enable_unchecked(self, idx, val)
            }
        }
    };
}

impl_domain_diff_counters_interface!(crate::hals::soft_ugn_demo_cc::devices::DomainDiffCounters);
impl_domain_diff_counters_interface!(crate::hals::switch_demo_cc::devices::DomainDiffCounters);
impl_domain_diff_counters_interface!(crate::hals::switch_demo_gppe_cc::devices::DomainDiffCounters);
impl_domain_diff_counters_interface!(crate::hals::wire_demo_cc::devices::DomainDiffCounters);
