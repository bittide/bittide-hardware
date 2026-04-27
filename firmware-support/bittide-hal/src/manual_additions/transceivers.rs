// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use crate::types::statistics::Statistics;

pub trait TransceiversInterface {
    const CHANNEL_ENABLES_LEN: usize;
    const HANDSHAKES_DONE_LEN: usize;

    fn transceiver_enable(&self) -> bool;

    fn channel_enables(&self, idx: usize) -> Option<bool>;
    /// # Safety
    /// `idx` must be in bounds.
    unsafe fn channel_enables_unchecked(&self, idx: usize) -> bool;
    fn channel_enables_volatile_iter(&self) -> impl DoubleEndedIterator<Item = bool> + '_;

    fn receive_readys(&self, idx: usize) -> Option<bool>;
    /// # Safety
    /// `idx` must be in bounds.
    unsafe fn receive_readys_unchecked(&self, idx: usize) -> bool;
    fn receive_readys_volatile_iter(&self) -> impl DoubleEndedIterator<Item = bool> + '_;

    fn transmit_starts(&self, idx: usize) -> Option<bool>;
    /// # Safety
    /// `idx` must be in bounds.
    unsafe fn transmit_starts_unchecked(&self, idx: usize) -> bool;
    fn transmit_starts_volatile_iter(&self) -> impl DoubleEndedIterator<Item = bool> + '_;

    fn statistics(&self, idx: usize) -> Option<Statistics>;
    /// # Safety
    /// `idx` must be in bounds.
    unsafe fn statistics_unchecked(&self, idx: usize) -> Statistics;
    fn statistics_volatile_iter(&self) -> impl DoubleEndedIterator<Item = Statistics> + '_;

    fn handshakes_done(&self, idx: usize) -> Option<bool>;
    /// # Safety
    /// `idx` must be in bounds.
    unsafe fn handshakes_done_unchecked(&self, idx: usize) -> bool;
    fn handshakes_done_volatile_iter(&self) -> impl DoubleEndedIterator<Item = bool> + '_;

    fn neighbor_receive_readys(&self, idx: usize) -> Option<bool>;
    /// # Safety
    /// `idx` must be in bounds.
    unsafe fn neighbor_receive_readys_unchecked(&self, idx: usize) -> bool;
    fn neighbor_receive_readys_volatile_iter(&self) -> impl DoubleEndedIterator<Item = bool> + '_;

    fn neighbor_transmit_readys(&self, idx: usize) -> Option<bool>;
    /// # Safety
    /// `idx` must be in bounds.
    unsafe fn neighbor_transmit_readys_unchecked(&self, idx: usize) -> bool;
    fn neighbor_transmit_readys_volatile_iter(&self) -> impl DoubleEndedIterator<Item = bool> + '_;

    fn set_transceiver_enable(&self, val: bool);
    fn set_channel_enables(&self, idx: usize, val: bool) -> Option<()>;
    fn set_receive_readys(&self, idx: usize, val: bool) -> Option<()>;
    fn set_transmit_starts(&self, idx: usize, val: bool) -> Option<()>;
}

macro_rules! impl_transceivers_interface {
    ($t:ty) => {
        impl TransceiversInterface for $t {
            const CHANNEL_ENABLES_LEN: usize = <$t>::CHANNEL_ENABLES_LEN;
            const HANDSHAKES_DONE_LEN: usize = <$t>::HANDSHAKES_DONE_LEN;

            fn transceiver_enable(&self) -> bool {
                <$t>::transceiver_enable(self)
            }

            fn channel_enables(&self, idx: usize) -> Option<bool> {
                <$t>::channel_enables(self, idx)
            }
            unsafe fn channel_enables_unchecked(&self, idx: usize) -> bool {
                <$t>::channel_enables_unchecked(self, idx)
            }
            fn channel_enables_volatile_iter(&self) -> impl DoubleEndedIterator<Item = bool> + '_ {
                <$t>::channel_enables_volatile_iter(self)
            }

            fn receive_readys(&self, idx: usize) -> Option<bool> {
                <$t>::receive_readys(self, idx)
            }
            unsafe fn receive_readys_unchecked(&self, idx: usize) -> bool {
                <$t>::receive_readys_unchecked(self, idx)
            }
            fn receive_readys_volatile_iter(&self) -> impl DoubleEndedIterator<Item = bool> + '_ {
                <$t>::receive_readys_volatile_iter(self)
            }

            fn transmit_starts(&self, idx: usize) -> Option<bool> {
                <$t>::transmit_starts(self, idx)
            }
            unsafe fn transmit_starts_unchecked(&self, idx: usize) -> bool {
                <$t>::transmit_starts_unchecked(self, idx)
            }
            fn transmit_starts_volatile_iter(&self) -> impl DoubleEndedIterator<Item = bool> + '_ {
                <$t>::transmit_starts_volatile_iter(self)
            }

            fn statistics(&self, idx: usize) -> Option<Statistics> {
                <$t>::statistics(self, idx)
            }
            unsafe fn statistics_unchecked(&self, idx: usize) -> Statistics {
                <$t>::statistics_unchecked(self, idx)
            }
            fn statistics_volatile_iter(&self) -> impl DoubleEndedIterator<Item = Statistics> + '_ {
                <$t>::statistics_volatile_iter(self)
            }

            fn handshakes_done(&self, idx: usize) -> Option<bool> {
                <$t>::handshakes_done(self, idx)
            }
            unsafe fn handshakes_done_unchecked(&self, idx: usize) -> bool {
                <$t>::handshakes_done_unchecked(self, idx)
            }
            fn handshakes_done_volatile_iter(&self) -> impl DoubleEndedIterator<Item = bool> + '_ {
                <$t>::handshakes_done_volatile_iter(self)
            }

            fn neighbor_receive_readys(&self, idx: usize) -> Option<bool> {
                <$t>::neighbor_receive_readys(self, idx)
            }
            unsafe fn neighbor_receive_readys_unchecked(&self, idx: usize) -> bool {
                <$t>::neighbor_receive_readys_unchecked(self, idx)
            }
            fn neighbor_receive_readys_volatile_iter(
                &self,
            ) -> impl DoubleEndedIterator<Item = bool> + '_ {
                <$t>::neighbor_receive_readys_volatile_iter(self)
            }

            fn neighbor_transmit_readys(&self, idx: usize) -> Option<bool> {
                <$t>::neighbor_transmit_readys(self, idx)
            }
            unsafe fn neighbor_transmit_readys_unchecked(&self, idx: usize) -> bool {
                <$t>::neighbor_transmit_readys_unchecked(self, idx)
            }
            fn neighbor_transmit_readys_volatile_iter(
                &self,
            ) -> impl DoubleEndedIterator<Item = bool> + '_ {
                <$t>::neighbor_transmit_readys_volatile_iter(self)
            }

            fn set_transceiver_enable(&self, val: bool) {
                <$t>::set_transceiver_enable(self, val)
            }
            fn set_channel_enables(&self, idx: usize, val: bool) -> Option<()> {
                <$t>::set_channel_enables(self, idx, val)
            }
            fn set_receive_readys(&self, idx: usize, val: bool) -> Option<()> {
                <$t>::set_receive_readys(self, idx, val)
            }
            fn set_transmit_starts(&self, idx: usize, val: bool) -> Option<()> {
                <$t>::set_transmit_starts(self, idx, val)
            }
        }
    };
}

impl_transceivers_interface!(crate::hals::soft_ugn_demo_boot::devices::Transceivers);
impl_transceivers_interface!(crate::hals::soft_ugn_demo_mu::devices::Transceivers);
impl_transceivers_interface!(crate::hals::switch_demo_boot::devices::Transceivers);
impl_transceivers_interface!(crate::hals::switch_demo_mu::devices::Transceivers);
impl_transceivers_interface!(crate::hals::switch_demo_gppe_boot::devices::Transceivers);
impl_transceivers_interface!(crate::hals::switch_demo_gppe_mu::devices::Transceivers);
impl_transceivers_interface!(crate::hals::wire_demo_boot::devices::Transceivers);
impl_transceivers_interface!(crate::hals::wire_demo_mu::devices::Transceivers);
