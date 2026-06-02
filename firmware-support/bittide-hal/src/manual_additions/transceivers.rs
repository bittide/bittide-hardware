// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use crate::types::statistics::Statistics;

pub trait TransceiversInterface {
    const CHANNEL_ENABLES_LEN: usize;
    const STATISTICS_LEN: usize;
    const RX_DATA_INIT_DONES_LEN: usize;
    const TX_DATA_INIT_DONES_LEN: usize;

    fn transceiver_enable(&self) -> bool;

    fn channel_enables(&self, idx: usize) -> Option<bool>;
    /// # Safety
    /// `idx` must be in bounds.
    unsafe fn channel_enables_unchecked(&self, idx: usize) -> bool;
    fn channel_enables_volatile_iter(&self) -> impl DoubleEndedIterator<Item = bool> + '_;

    fn statistics(&self, idx: usize) -> Option<Statistics>;
    /// # Safety
    /// `idx` must be in bounds.
    unsafe fn statistics_unchecked(&self, idx: usize) -> Statistics;
    fn statistics_volatile_iter(&self) -> impl DoubleEndedIterator<Item = Statistics> + '_;

    fn rx_data_init_dones(&self, idx: usize) -> Option<bool>;
    /// # Safety
    /// `idx` must be in bounds.
    unsafe fn rx_data_init_dones_unchecked(&self, idx: usize) -> bool;
    fn rx_data_init_dones_volatile_iter(&self) -> impl DoubleEndedIterator<Item = bool> + '_;

    fn tx_data_init_dones(&self, idx: usize) -> Option<bool>;
    /// # Safety
    /// `idx` must be in bounds.
    unsafe fn tx_data_init_dones_unchecked(&self, idx: usize) -> bool;
    fn tx_data_init_dones_volatile_iter(&self) -> impl DoubleEndedIterator<Item = bool> + '_;

    fn set_transceiver_enable(&self, val: bool);
    fn set_channel_enables(&self, idx: usize, val: bool) -> Option<()>;
    fn set_rx_data_init_dones(&self, idx: usize, val: bool) -> Option<()>;
    fn set_tx_data_init_dones(&self, idx: usize, val: bool) -> Option<()>;
}

macro_rules! impl_transceivers_interface {
    ($t:ty) => {
        impl TransceiversInterface for $t {
            const CHANNEL_ENABLES_LEN: usize = <$t>::CHANNEL_ENABLES_LEN;
            const STATISTICS_LEN: usize = <$t>::STATISTICS_LEN;
            const RX_DATA_INIT_DONES_LEN: usize = <$t>::RX_DATA_INIT_DONES_LEN;
            const TX_DATA_INIT_DONES_LEN: usize = <$t>::TX_DATA_INIT_DONES_LEN;

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

            fn statistics(&self, idx: usize) -> Option<Statistics> {
                <$t>::statistics(self, idx)
            }
            unsafe fn statistics_unchecked(&self, idx: usize) -> Statistics {
                <$t>::statistics_unchecked(self, idx)
            }
            fn statistics_volatile_iter(&self) -> impl DoubleEndedIterator<Item = Statistics> + '_ {
                <$t>::statistics_volatile_iter(self)
            }

            fn rx_data_init_dones(&self, idx: usize) -> Option<bool> {
                <$t>::rx_data_init_dones(self, idx)
            }
            unsafe fn rx_data_init_dones_unchecked(&self, idx: usize) -> bool {
                <$t>::rx_data_init_dones_unchecked(self, idx)
            }
            fn rx_data_init_dones_volatile_iter(
                &self,
            ) -> impl DoubleEndedIterator<Item = bool> + '_ {
                <$t>::rx_data_init_dones_volatile_iter(self)
            }

            fn tx_data_init_dones(&self, idx: usize) -> Option<bool> {
                <$t>::tx_data_init_dones(self, idx)
            }
            unsafe fn tx_data_init_dones_unchecked(&self, idx: usize) -> bool {
                <$t>::tx_data_init_dones_unchecked(self, idx)
            }
            fn tx_data_init_dones_volatile_iter(
                &self,
            ) -> impl DoubleEndedIterator<Item = bool> + '_ {
                <$t>::tx_data_init_dones_volatile_iter(self)
            }

            fn set_transceiver_enable(&self, val: bool) {
                <$t>::set_transceiver_enable(self, val)
            }
            fn set_channel_enables(&self, idx: usize, val: bool) -> Option<()> {
                <$t>::set_channel_enables(self, idx, val)
            }
            fn set_rx_data_init_dones(&self, idx: usize, val: bool) -> Option<()> {
                <$t>::set_rx_data_init_dones(self, idx, val)
            }
            fn set_tx_data_init_dones(&self, idx: usize, val: bool) -> Option<()> {
                <$t>::set_tx_data_init_dones(self, idx, val)
            }
        }
    };
}

impl_transceivers_interface!(crate::shared_devices::Transceivers);
