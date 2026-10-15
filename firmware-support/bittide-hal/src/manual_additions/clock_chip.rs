// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
//! Register access shared by the Skyworks clock chips we use: the Si5395 on
//! the external clock board (over SPI) and the Si5345 on the FMC card (over
//! I2C). Both chips have the same paged register map for the registers used
//! here.
use crate::{
    manual_additions::{timer::Duration, ConvertOptional},
    shared_devices::Timer,
    types::{Maybe::Nothing, RegisterOperation},
};
use clash_bindings::bitvector::BitVector;
use clash_macros::bitvector;
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

impl ConfigEntry {
    /// Whether the written value can be read back. Register files from
    /// ClockBuilder Pro also write the tool version (page 0, 0x0006 to 0x0008).
    /// These registers are informational, and on the Si5345 0x0007 keeps its
    /// own value.
    pub fn is_verifiable(&self) -> bool {
        !(self.page == 0x00 && (0x06..=0x08).contains(&self.address))
    }
}

impl From<ConfigEntry> for RegisterOperation {
    fn from(value: ConfigEntry) -> Self {
        RegisterOperation {
            page: bitvector!([value.page], n = 8),
            address: bitvector!([value.address], n = 8),
            write: BitVector::new([value.data]).conv_optional(),
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

/// Register based interface to a clock chip. Implementors only provide access to
/// the hardware registers, all chip logic is provided by this trait.
pub trait ClockChip {
    /// Set the register operation to perform on the next commit.
    fn set_register_operation(&self, val: RegisterOperation);
    /// Start the register operation. Cleared by hardware once it has finished.
    fn set_commit(&self, val: bool);
    /// Whether the last committed operation is still in progress.
    fn commit(&self) -> bool;
    /// Byte returned by the last operation.
    fn read_byte(&self) -> u8;
    /// Tell the hardware that the configuration is done and the clock is running.
    fn set_config_done(&self, val: bool);
    /// Reset the interface, so it selects the page again.
    fn set_reset(&self, val: bool);

    /// Write a clock board configuration.
    ///
    /// Steps:
    ///   - Wait for ready
    ///   - Write preamble registers
    ///   - Wait 300 milliseconds for the calibration of the preamble (timer must be in a free domain)
    ///   - Write and read back all configuration registers one by one
    ///   - Write postamble registers
    ///   - Wait for lock
    ///   - Finished
    fn write_configuration<const PRE_LEN: usize, const CFG_LEN: usize, const PST_LEN: usize>(
        &self,
        timer: &Timer,
        config: &Config<PRE_LEN, CFG_LEN, PST_LEN>,
    ) -> Result<(), WriteError> {
        self.set_config_done(false);
        self.wait_for_ready();

        // Write preamble
        for entry in config.preamble {
            self.write(entry);
        }
        // Waiting for calibration of preamble
        timer.wait(Duration::from_millis(300));

        // Write the configuration and confirm by reading back
        for entry in config.config {
            if entry.is_verifiable() {
                self.write_and_confirm(entry)?;
            } else {
                self.write(entry);
            }
        }

        // Write postamble
        for entry in config.postamble {
            self.write(entry);
        }

        self.wait_for_lock();
        self.set_config_done(true);
        Ok(())
    }

    /// Write a clock board configuration, will retry upon failure.
    ///
    /// Writing a configuration can sometimes fail, usually because a written register was not
    /// the same when not read back. This happens roughly once in 200 runs.
    fn write_configuration_with_retry<
        const PRE_LEN: usize,
        const CFG_LEN: usize,
        const PST_LEN: usize,
    >(
        &self,
        timer: &Timer,
        config: &Config<PRE_LEN, CFG_LEN, PST_LEN>,
        max_retries: usize,
    ) -> Result<(), WriteError> {
        let mut retries = 0;
        loop {
            match self.write_configuration(timer, config) {
                Ok(()) => return Ok(()),
                Err(e) => {
                    retries += 1;
                    if retries >= max_retries {
                        return Err(e);
                    }
                }
            }
        }
    }

    /// Verfiy that the config part of the configuration is as expected.
    fn verify_configuration<const PRE_LEN: usize, const CFG_LEN: usize, const PST_LEN: usize>(
        &self,
        config: &Config<PRE_LEN, CFG_LEN, PST_LEN>,
    ) -> Result<(), ConfigError> {
        self.wait_for_ready();
        let mut correct = true;
        for entry in config.config.iter().filter(|e| e.is_verifiable()) {
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

    /// Wait until the device is ready to accept commands.
    ///
    /// Poll the `DEVICE_READY` register at 'Address' 0xFE at any 'Page', if
    /// this operation returns 0x0F twice in a row, the device is ready to
    /// accept commands.
    fn wait_for_ready(&self) {
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
    fn wait_for_lock(&self) {
        let mask = 0b0010_1011;
        loop {
            let dat = self.read(0x00, 0x0C);
            if dat & mask == 0 {
                break;
            }
        }
    }

    /// Perform a read operation.
    fn read(&self, page: u8, address: u8) -> u8 {
        self.start_read(page, address);
        while self.is_busy() {
            continue;
        }
        self.read_byte()
    }

    /// Start a read operation without waiting for it to finish. Use
    /// [`Self::is_busy`] to check whether it has finished, and
    /// [`Self::read_byte`] to get the result.
    fn start_read(&self, page: u8, address: u8) {
        let read_op = RegisterOperation {
            page: bitvector!([page], n = 8),
            address: bitvector!([address], n = 8),
            write: Nothing,
        };
        self.set_register_operation(read_op);
        self.set_commit(true);
    }

    /// Perform a write operation.
    ///
    /// It looks like this is what the function `si539xSpi` does (see line 311).
    /// When it is doing a `WriteEntry` operation, it also waits on until
    /// `driverByte` is a `Just` while not looking at the value.
    fn write(&self, entry: ConfigEntry) {
        self.start_write(entry);
        while self.is_busy() {
            continue;
        }
    }

    /// Start a write operation without waiting for it to finish. Use
    /// [`Self::is_busy`] to check whether it has finished.
    fn start_write(&self, entry: ConfigEntry) {
        self.set_register_operation(entry.into());
        self.set_commit(true);
    }

    /// Whether an operation is still in progress.
    fn is_busy(&self) -> bool {
        self.commit()
    }

    /// Perform a write operation and then confirm with a read operation.
    fn write_and_confirm(&self, entry: ConfigEntry) -> Result<(), WriteError> {
        self.write(entry);
        let read_data = self.read(entry.page, entry.address);
        if read_data != entry.data {
            Err(WriteError::NotConfirmed { entry, read_data })
        } else {
            Ok(())
        }
    }

    /// Read the 8 DESIGN_ID registers.
    fn read_design_id(&self) -> [u8; 8] {
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
    fn write_design_id(&self, design_id: [u8; 8]) {
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

    /// Do a frequency increment 'n' times.
    ///
    /// Waits for 1 us between transactions to adhere to the maximum update rate of
    /// 1 us of the chip.
    fn finc(&self, timer: &Timer, n: u8) {
        for _ in 0..n {
            self.write(FINC);
            timer.wait(Duration::from_micros(1));
        }
    }

    /// Do a frequency decrement 'n' times.
    ///
    /// Waits for 1 us between transactions to adhere to the maximum update rate of
    /// 1 us of the chip.
    fn fdec(&self, timer: &Timer, n: u8) {
        for _ in 0..n {
            self.write(FDEC);
            timer.wait(Duration::from_micros(1));
        }
    }

    /// Request a single frequency increment or decrement, without waiting for the
    /// transaction to finish.
    ///
    /// Returns `false` if a previous operation is still in progress, in which case
    /// nothing is requested. [`SpeedChange::NoChange`] is always accepted.
    ///
    /// Unlike [`Self::finc`] and [`Self::fdec`], this does not wait 1 us after the
    /// transaction to adhere to the maximum update rate of the chip. With the SPI and
    /// I2C clocks we use, a single transaction already takes much longer than that.
    fn try_speed_change(&self, speed_change: SpeedChange) -> bool {
        let entry = match speed_change {
            SpeedChange::NoChange => return true,
            SpeedChange::SpeedUp => FINC,
            SpeedChange::SlowDown => FDEC,
        };
        if self.is_busy() {
            false
        } else {
            self.start_write(entry);
            true
        }
    }
}

/// A request to change the output frequency of the clock chip by one step.
#[derive(Clone, Copy, PartialEq, Eq, uDebug)]
pub enum SpeedChange {
    NoChange,
    SlowDown,
    SpeedUp,
}

/// Write to this register to increment the output frequency by one step.
const FINC: ConfigEntry = ConfigEntry {
    page: 0x00,
    address: 0x1D,
    data: 0b1 << 0,
};

/// Write to this register to decrement the output frequency by one step.
const FDEC: ConfigEntry = ConfigEntry {
    page: 0x00,
    address: 0x1D,
    data: 0b1 << 1,
};
