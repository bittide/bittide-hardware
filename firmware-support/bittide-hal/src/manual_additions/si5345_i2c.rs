// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::{
    manual_additions::clock_chip::ClockChip, shared_devices::Si5345I2c, types::RegisterOperation,
};

impl ClockChip for Si5345I2c {
    fn set_register_operation(&self, val: RegisterOperation) {
        Si5345I2c::set_register_operation(self, val)
    }

    fn set_commit(&self, val: bool) {
        Si5345I2c::set_commit(self, val)
    }

    fn commit(&self) -> bool {
        Si5345I2c::commit(self)
    }

    fn read_byte(&self) -> u8 {
        self.read_data().into_inner()[0]
    }

    fn set_config_done(&self, val: bool) {
        self.set_done(val)
    }

    fn set_reset(&self, val: bool) {
        Si5345I2c::set_reset(self, val)
    }
}
