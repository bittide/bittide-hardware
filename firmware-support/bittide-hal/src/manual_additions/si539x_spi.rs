// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::manual_additions::timer::Duration;
use crate::shared::devices::Si539xSpi;
use crate::shared::devices::Timer;
use crate::shared::devices::Uart;
use crate::shared::types::Maybe::{Just, Nothing};
use crate::shared::types::RegisterOperation;
use crate::shared::types::Tuple0;

use ufmt::derive::uDebug;
use ufmt::uwriteln;

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

pub enum Si539xError {
    WriteNotConfirmed { read_data: u8 },
}

impl Si539xSpi {
    /// Write a clock board configuration using SPI.
    ///
    /// Steps:
    ///   - Wait for ready
    ///   - Write preamble registers (don't confirm with read back, since some are self-clearing)
    ///   - Wait 300 milliseconds for the calibration of the preamble (timer must be in a free domain)
    ///   - Write and read back all configuration registers one by one
    ///   - Write postamble registers (don't confirm with read back, since some are self-clearing)
    ///   - Wait for lock
    ///   - Finished
    pub fn write_configuration<const PRE_LEN: usize, const CFG_LEN: usize, const PST_LEN: usize>(
        &self,
        timer: &Timer,
        config: &Config<PRE_LEN, CFG_LEN, PST_LEN>,
        uart: &mut Uart,
    ) {
        let start = timer.now();
        uwriteln!(
            uart,
            "{}: Setting 'spi_done' to false...",
            timer.now() - start
        )
        .unwrap();
        self.set_spi_done(false);
        uwriteln!(
            uart,
            "{}: Waiting for clock board to be ready for configuration...",
            timer.now() - start
        )
        .unwrap();
        self.wait_for_ready();

        uwriteln!(uart, "{}: Writing preamble...", timer.now() - start).unwrap();
        for entry in config.preamble {
            self.write(entry.into());
        }
        uwriteln!(
            uart,
            "{}: Waiting for calibration of preamble...",
            timer.now() - start
        )
        .unwrap();
        timer.wait(Duration::from_millis(300));

        uwriteln!(uart, "{}: Writing config...", timer.now() - start).unwrap();
        for entry in config.config {
            // TODO: Use 'write_and_confirm'
            self.write(entry.into());

            // match self.write_and_confirm(entry.into(), timer) {
            //     Ok(()) => {
            //         uwriteln!(
            //             uart,
            //             "At 0x{:02X}{:02X} wrote and confirmed 0x{:02X}",
            //             entry.page,
            //             entry.address,
            //             entry.data,
            //         )
            //         .unwrap();
            //         continue
            //     },
            //     Err(Si539xError::WriteNotConfirmed { read_data }) => {
            //         uwriteln!(
            //             uart,
            //             "ERROR: At 0x{:02X}{:02X} wrote 0x{:02X}, but read back 0x{:02X}",
            //             entry.page,
            //             entry.address,
            //             entry.data,
            //             read_data,
            //         ).unwrap();
            //     },
            // };
        }

        uwriteln!(uart, "{}: Writing postamble...", timer.now() - start).unwrap();
        for entry in config.postamble {
            self.write(entry.into());
        }

        uwriteln!(
            uart,
            "{}: Waiting for lock of clock...",
            timer.now() - start
        )
        .unwrap();
        self.wait_for_lock(uart);
        uwriteln!(
            uart,
            "{}: Setting 'spi_done' to true...",
            timer.now() - start
        )
        .unwrap();
        self.set_spi_done(true);
        uwriteln!(
            uart,
            "{}: Finished writing configuration",
            timer.now() - start
        )
        .unwrap();
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
    pub fn wait_for_lock(&self, uart: &mut Uart) {
        let read_op = RegisterOperation {
            page: 0x00,
            address: 0x0C,
            write: Nothing,
        };
        let mask = 0b1 << 2;
        loop {
            let dat = self.read(read_op);
            if (dat & mask) == 0b0 {
                break;
            }
            // Register 0x000C Internal Status Bits:
            // Bit 0: 1 if device is calibrating
            // Bit 1: 1 if there is no signal at the XAXB pins
            // Bit 3: 1 if there is a problem locking to the XAXB input signal
            // Bit 5: 1 if there is an SMBus timeout error
            uwriteln!(uart, "Not locked, got: 0x{:08X}", dat).unwrap();
        }
    }

    /// Perform a read operation.
    pub fn read(&self, read_op: RegisterOperation) -> u8 {
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
    pub fn write_and_confirm(&self, write_op: RegisterOperation) -> Result<(), Si539xError> {
        self.write(write_op);
        let read_op = RegisterOperation {
            page: write_op.page,
            address: write_op.address,
            write: Nothing,
        };
        let read_data = self.read(read_op);
        if Just(read_data) != write_op.write {
            Err(Si539xError::WriteNotConfirmed { read_data })
        } else {
            Ok(())
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
