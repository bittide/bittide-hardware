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
        self.wait_for_ready(timer);

        uwriteln!(uart, "{}: Writing preamble...", timer.now() - start).unwrap();
        for entry in config.preamble {
            self.write(entry.into(), timer);
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
            self.write_and_confirm(entry.into(), uart, timer);
        }

        uwriteln!(uart, "{}: Writing postamble...", timer.now() - start).unwrap();
        for entry in config.postamble {
            self.write(entry.into(), timer);
        }

        uwriteln!(
            uart,
            "{}: Waiting for lock of clock...",
            timer.now() - start
        )
        .unwrap();
        self.wait_for_lock(uart, timer);
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
    pub fn wait_for_ready(&self, timer: &Timer) {
        let read_op = RegisterOperation {
            page: 0x00,
            address: 0xFE,
            write: Nothing,
        };
        let mut seen_once = false;
        loop {
            let dat = self.read(read_op, timer);
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
    pub fn wait_for_lock(&self, uart: &mut Uart, timer: &Timer) {
        let read_op = RegisterOperation {
            page: 0x00,
            address: 0x0C,
            write: Nothing,
        };
        let mask = 0b0000_0100;
        let mut i: u32 = 0;
        loop {
            let dat = self.read(read_op, timer);
            if (dat & mask) == 0b0 {
                break;
            }
            // if (i & (0b1 << 10)) == 0b01 {
            //     uwriteln!(uart, "Waiting for lock for {} SPI operations", i).unwrap();
            // }
            i += 1;
        }
        uwriteln!(uart, "Waiting for lock took {} SPI operations", i).unwrap();
    }

    /// Perform a read operation.
    pub fn read(&self, read_op: RegisterOperation, timer: &Timer) -> u8 {
        self.wait_for_idle();
        self.set_register_operation(read_op);
        self.commit();
        timer.wait(Duration::from_millis(100));
        self.read_data()
    }

    /// Perform a write operation.
    ///
    /// It looks like this is what the function `si539xSpi` does (see line 311).
    /// When it is doing a `WriteEntry` operation, it also waits on until
    /// `driverByte` is a `Just` while not looking at the value.
    pub fn write(&self, write_op: RegisterOperation, timer: &Timer) {
        let _ = self.read(write_op, timer);
    }

    /// Perform a write operation and then confirm with a read operation.
    pub fn write_and_confirm(&self, write_op: RegisterOperation, uart: &mut Uart, timer: &Timer) {
        self.write(write_op, timer);
        let read_op = RegisterOperation {
            page: write_op.page,
            address: write_op.address,
            write: Nothing,
        };
        let read_data = self.read(read_op, timer);
        if Just(read_data) != write_op.write {
            // TODO: Throw error
            let write_data = match write_op.write {
                Just(w) => w,
                Nothing => {
                    uwriteln!(
                        uart,
                        "ERROR: Write operation at addr 0x{:02X}{:02X} has no data",
                        write_op.page,
                        write_op.address,
                    )
                    .unwrap();
                    0xFF
                }
            };
            uwriteln!(
                uart,
                "ERROR: At 0x{:02X}{:02X} wrote 0x{:02X}, but read back 0x{:02X}",
                write_op.page,
                write_op.address,
                write_data,
                read_data
            )
            .unwrap();
        } else {
            uwriteln!(
                uart,
                "At 0x{:02X}{:02X} wrote and confirmed 0x{:02X}",
                write_op.page,
                write_op.address,
                read_data
            )
            .unwrap();
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
