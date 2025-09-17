// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::manual_additions::timer::Duration;
use crate::shared::devices::Si539xSpi;
use crate::shared::devices::Timer;
use crate::shared::types::Maybe::{Just, Nothing};
use crate::shared::types::RegisterOperation;
use crate::shared::types::Tuple0;

use ufmt::derive::uDebug;

pub struct Config<const PRE_LEN: usize, const CFG_LEN: usize, const PST_LEN: usize> {
    pub preamble: [ConfigEntry; PRE_LEN],
    pub config: [ConfigEntry; CFG_LEN],
    pub postamble: [ConfigEntry; PST_LEN],
}

#[derive(uDebug, Copy, Clone)]
pub struct ConfigEntry {
    pub page: u8,
    pub address: u8,
    pub data: u8,
}

#[allow(clippy::from_over_into)]
impl Into<RegisterOperation> for ConfigEntry {
    fn into(self) -> RegisterOperation {
        RegisterOperation {
            page: self.page,
            address: self.address,
            write: Just(self.data),
        }
    }
}

impl Si539xSpi {
    /// Write a clock board configuration using SPI.
    ///
    /// TODO: Add configuration as argument
    pub fn write_configuration(&self, timer: &Timer) {
        self.set_spi_done(false);
        // Wait for ready
        self.wait_for_ready();
        // Write preamble registers (don't confirm with read back, since some are self-clearing)
        // for reg_op in config.preamble {
        //     self.write(reg_op);
        // }
        // Wait 300 milliseconds for the calibration of the preamble (timer must be in a free domain)
        timer.wait(Duration::from_millis(300));
        // Write and read back all configuration registers one by one
        // for reg_op in config.config {
        //     self.write_and_confirm(reg_op);
        // }
        // Write postamble registers (don't confirm with read back, since some are self-clearing)
        // for reg_op in config.postamble {
        //     self.write(reg_op);
        // }
        // Wait for lock
        self.wait_for_lock();
        // Finished
        self.set_spi_done(true);
    }

    /// Wait until the device is ready for operation.
    ///
    /// Continuously read from 'Address' 0xFE at any 'Page', if this operations
    /// returns 0x0F twice in a row, the device is considered to be ready for
    /// operation.
    pub fn wait_for_ready(&self) {
        let read_op = RegisterOperation {
            page: 0x00,
            address: 0xFE,
            write: Nothing,
        };
        let mut seen_once = false;
        loop {
            let dat = self.read(read_op);
            if seen_once && dat == 0x0F {
                break;
            } else if dat == 0x0F {
                seen_once = true;
                // Reset the driver so it sets the page and address again
                self.set_reset(Tuple0);
            }
        }
    }

    /// Wait for the calibration to be finished.
    ///
    /// Continuously read from 'Address' 0x0C at 'Page' 0x00 until it returns
    /// bit 3 is 0.
    pub fn wait_for_lock(&self) {
        let read_op = RegisterOperation {
            page: 0x00,
            address: 0x0C,
            write: Nothing,
        };
        let mask = 0b0000_0100;
        loop {
            let dat = self.read(read_op);
            if (dat & mask) == 0b0 {
                break;
            }
        }
    }

    /// Perform a read operation.
    fn read(&self, read_op: RegisterOperation) -> u8 {
        self.wait_for_idle();
        self.set_register_operation(read_op);
        self.commit();
        self.read_data()
    }

    /// Perform a write operation.
    ///
    /// It looks like this is what the function `si539xSpi` does (see line 311).
    /// When it is doing a `WriteEntry` operation, it also waits on until
    /// `driverByte` is a `Just` while not looking at the value.
    pub fn write(&self, write_op: RegisterOperation) {
        let _ = self.read(write_op);
    }

    /// Perform a write operation and then confirm with a read operation.
    pub fn write_and_confirm(&self, write_op: RegisterOperation) {
        self.write(write_op);
        let read_op = RegisterOperation {
            page: write_op.page,
            address: write_op.address,
            write: Nothing,
        };
        let read_dat = self.read(read_op);
        if Just(read_dat) != write_op.write {
            // Throw error
        }
    }

    /// Wait for the SPI subordinate to no longer be busy.
    pub fn wait_for_idle(&self) {
        while self.busy() {
            continue;
        }
    }

    /// Start an SPI transaction and wait for it to be finished.
    ///
    /// Commit the data in the 'register_operation' register and wait for the
    /// SPI to be finished.
    pub fn commit(&self) {
        self.set_commit(Tuple0);
        self.wait_for_idle();
    }
}
