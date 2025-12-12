// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::manual_additions::timer::Duration;
use crate::shared_devices::Si539xSpi;
use crate::shared_devices::Timer;
use crate::types::Maybe::{Just, Nothing};
use crate::types::RegisterOperation;

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
            page: [self.page],
            address: [self.address],
            write: Just([self.data]),
        }
    }
}

#[derive(uDebug, Copy, Clone)]
pub enum WriteError {
    NotConfirmed { entry: ConfigEntry, read_data: u8 },
}

#[derive(uDebug, Copy, Clone)]
pub enum ConfigError {
    NotExpectedConfig { design_id: [u8; 8] },
}

impl Si539xSpi {
    /// Write a clock board configuration using SPI.
    ///
    /// Steps:
    ///   - Wait for ready
    ///   - Write preamble registers
    ///   - Wait 300 milliseconds for the calibration of the preamble (timer must be in a free domain)
    ///   - Write and read back all configuration registers one by one
    ///   - Write postamble registers
    ///   - Wait for lock
    ///   - Finished
    pub fn write_configuration<const PRE_LEN: usize, const CFG_LEN: usize, const PST_LEN: usize>(
        &self,
        timer: &Timer,
        config: &Config<PRE_LEN, CFG_LEN, PST_LEN>,
    ) -> Result<(), WriteError> {
        self.set_spi_done(false);
        self.wait_for_ready();

        // Write preamble
        for entry in config.preamble {
            self.write(entry);
        }
        // Waiting for calibration of preamble
        timer.wait(Duration::from_millis(300));

        // Write the configuration and confirm by reading back
        for entry in config.config {
            self.write_and_confirm(entry)?;
        }

        // Write postamble
        for entry in config.postamble {
            self.write(entry);
        }

        self.wait_for_lock();
        self.set_spi_done(true);
        Ok(())
    }

    /// Verfiy that the config part of the configuration is as expected.
    pub fn verify_configuration<
        const PRE_LEN: usize,
        const CFG_LEN: usize,
        const PST_LEN: usize,
    >(
        &self,
        config: &Config<PRE_LEN, CFG_LEN, PST_LEN>,
    ) -> Result<(), ConfigError> {
        self.wait_for_ready();
        let mut correct = true;
        for entry in config.config {
            let read_data = self.read(entry.page, entry.address);
            if read_data != entry.data {
                correct = false;
            }
        }
        if correct {
            Ok(())
        } else {
            let design_id = self.read_design_id();
            Err(ConfigError::NotExpectedConfig { design_id })
        }
    }

    /// Wait until the device is ready to accept commands over SPI.
    ///
    /// Poll the `DEVICE_READY` register at 'Address' 0xFE at any 'Page', if
    /// this operation returns 0x0F twice in a row, the device is ready to
    /// accept commands.
    pub fn wait_for_ready(&self) {
        let mut seen_once = false;
        loop {
            let dat = self.read(0x00, 0xFE);
            if seen_once && dat == 0x0F {
                break;
            } else if dat == 0x0F {
                seen_once = true;
                // Reset the driver so it sets the page and address again
                self.set_reset(true);
                self.set_reset(false);
            }
        }
    }

    /// Wait for the calibration to be finished.
    ///
    /// Continuously read from the Internal Status register at page 0x0C and
    /// address 0x0C until all 4 defined bits are 0:
    /// - Bit 0: 1 if device is calibrating
    /// - Bit 1: 1 if there is no signal at the XAXB pins
    /// - Bit 3: 1 if there is a problem locking to the XAXB input signal
    /// - Bit 5: 1 if there is an SMBus timeout error
    pub fn wait_for_lock(&self) {
        let mask = 0b0010_1011;
        loop {
            let dat = self.read(0x00, 0x0C);
            if dat & mask == 0 {
                break;
            }
        }
    }

    /// Perform a read operation.
    pub fn read(&self, page: u8, address: u8) -> u8 {
        let read_op = RegisterOperation {
            page: [page],
            address: [address],
            write: Nothing,
        };
        self.set_register_operation(read_op);
        self.set_commit(true);
        while self.commit() {
            continue;
        }
        self.read_data()[0]
    }

    /// Perform a write operation.
    ///
    /// It looks like this is what the function `si539xSpi` does (see line 311).
    /// When it is doing a `WriteEntry` operation, it also waits on until
    /// `driverByte` is a `Just` while not looking at the value.
    pub fn write(&self, entry: ConfigEntry) {
        self.set_register_operation(entry.into());
        self.set_commit(true);
        while self.commit() {
            continue;
        }
    }

    /// Perform a write operation and then confirm with a read operation.
    pub fn write_and_confirm(&self, entry: ConfigEntry) -> Result<(), WriteError> {
        self.write(entry);
        let read_data = self.read(entry.page, entry.address);
        if read_data != entry.data {
            Err(WriteError::NotConfirmed { entry, read_data })
        } else {
            Ok(())
        }
    }

    /// Read the 8 DESIGN_ID registers.
    pub fn read_design_id(&self) -> [u8; 8] {
        const PAGE: u8 = 0x02;
        const DESIGN_ID_ADDRESSES: [u8; 8] = [0x6B, 0x6C, 0x6D, 0x6E, 0x6F, 0x70, 0x71, 0x72];
        let mut design_id: [u8; 8] = [0; 8];

        self.wait_for_ready();
        for (i, &address) in DESIGN_ID_ADDRESSES.iter().enumerate() {
            design_id[i] = self.read(PAGE, address);
        }
        design_id
    }

    /// Write the 8 DESIGN_ID registers.
    pub fn write_design_id(&self, design_id: [u8; 8]) {
        const PAGE: u8 = 0x02;
        const DESIGN_ID_ADDRESSES: [u8; 8] = [0x6B, 0x6C, 0x6D, 0x6E, 0x6F, 0x70, 0x71, 0x72];

        self.wait_for_ready();
        for (i, &address) in DESIGN_ID_ADDRESSES.iter().enumerate() {
            let write_op = ConfigEntry {
                page: PAGE,
                address,
                data: design_id[i],
            };
            self.write(write_op);
        }
    }
}
