// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::{
    manual_additions::clock_chip::ClockChip, shared_devices::Si539xSpi, types::RegisterOperation,
};

impl ClockChip for Si539xSpi {
    fn set_register_operation(&self, val: RegisterOperation) {
        Si539xSpi::set_register_operation(self, val)
    }

    fn set_commit(&self, val: bool) {
        Si539xSpi::set_commit(self, val)
    }

    fn commit(&self) -> bool {
        Si539xSpi::commit(self)
    }

    fn read_byte(&self) -> u8 {
        self.read_data().into_inner()[0]
    }

    fn set_config_done(&self, val: bool) {
        self.set_spi_done(val)
    }

    fn set_reset(&self, val: bool) {
        Si539xSpi::set_reset(self, val)
    }
}
