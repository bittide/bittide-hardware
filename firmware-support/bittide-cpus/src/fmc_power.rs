// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Sets VADJ_1V8 of the KCU105 to 1.8 V over PMBus.
//!
//! VADJ powers the FPGA banks connected to the FMC HPC connector, and the I/O
//! of the FMC card on it. The KCU105 system controller sets VADJ at power-on,
//! from the FRU EEPROM on the FMC card. With the Trenz TEF0008 it sets 0 V
//! after a power cycle, even with a valid EEPROM. That leaves the card's
//! Si5345 clock chip unpowered. So boot CPUs set VADJ themselves, by writing
//! to the regulator that supplies it: a MAX15301 (U30) at address 0x12, behind
//! channel 0 of the PCA9544 I2C switch on the main I2C bus of the KCU105. See
//! the KCU105 board user guide (UG917), tables 1-19, 1-25 and 1-26, and the
//! "MAX15301 PMBus Command Set User's Guide".
//!
//! The regulator is only written to in the state the system controller leaves
//! it in after setting VADJ to 0 V, or with the set point already at 1.8 V but
//! the output not, or with the output switched off (soft-off) by an earlier
//! attempt that failed halfway. Anything else is reported, and nothing is
//! written. In particular, the immediate-off left behind when VADJ came up too
//! high is not accepted: that needs a power cycle, or the system controller
//! menu.

use bittide_hal::manual_additions::i2c_controller::I2cError;
use bittide_hal::manual_additions::timer::Duration;
use bittide_hal::shared_devices::{I2cController, Timer, Uart};
use ufmt::derive::uDebug;
use ufmt::uwriteln;

/// PCA9544 I2C switch (U80) on the main I2C bus
const PCA9544_ADDRESS: u8 = 0x75;
/// PCA9544 control register value that enables channel 0: the PMBus
/// regulators
const PCA9544_PMBUS: u8 = 0b100;
/// MAX15301 that supplies VADJ_1V8_FPGA
const VADJ_REGULATOR: u8 = 0x12;

const OPERATION: u8 = 0x01;
const ON_OFF_CONFIG: u8 = 0x02;
const WRITE_PROTECT: u8 = 0x10;
const VOUT_MODE: u8 = 0x20;
const VOUT_COMMAND: u8 = 0x21;
const VOUT_MAX: u8 = 0x24;
const TOFF_DELAY: u8 = 0x64;
const TOFF_FALL: u8 = 0x65;
const STATUS_WORD: u8 = 0x79;
const READ_VOUT: u8 = 0x8B;

/// Linear mode, exponent -12: volts = mantissa / 4096
const EXPECTED_VOUT_MODE: u8 = 0x14;
/// Output enabled if both OPERATION and the EN pin allow it
const EXPECTED_ON_OFF_CONFIG: u8 = 0x1E;
const OPERATION_ON: u8 = 0x80;
const OPERATION_SOFT_OFF: u8 = 0x40;
const OPERATION_IMMEDIATE_OFF: u8 = 0x00;

/// 1.8 V: 7373 / 4096 = 1.80005 V. The regulator rounds it, and reads back
/// 0x1CCE.
const VOUT_1V8: u16 = 0x1CCD;
/// 1.71 V and 1.89 V: the range the TEF0008 supports
const VOUT_MIN_OK: u16 = 7005;
const VOUT_MAX_OK: u16 = 7741;

/// STATUS_WORD bit that is set while the output is not in regulation
const POWER_GOOD_N: u16 = 1 << 11;
/// STATUS_WORD bit that is set while the output is off
const OFF: u16 = 1 << 6;

/// Generous: the regulator has a turn-on delay of 25 ms and a rise time of
/// 2.3 ms (UG917, table 1-26). Also used for turning off.
const SETTLE_TIMEOUT_MS: u64 = 500;

/// How long to retry a transaction that failed in a way that resolves itself:
/// the regulator does not acknowledge while it is busy (for example while it
/// switches its output off, which can take over 100 ms: run 40905551317), and
/// the system controller uses the same bus, so we may lose arbitration.
const RETRY_TIMEOUT_MS: u64 = 500;
const RETRY_DELAY_MS: u64 = 1;

/// What an I2C transaction was for
#[derive(uDebug, Copy, Clone, PartialEq, Eq)]
pub enum Step {
    SelectChannel,
    ReadState,
    SwitchOff,
    WaitForOff,
    WriteVoutCommand,
    ReadBackVoutCommand,
    SwitchOn,
    WaitForOn,
    DeselectChannel,
}

#[derive(uDebug, Copy, Clone, PartialEq, Eq)]
pub enum VadjError {
    /// An I2C transaction failed
    I2c(Step, I2cError),
    /// The regulator is not in a state this code expects. Nothing was written.
    UnexpectedState,
    /// VOUT_COMMAND did not read back in range after writing it
    VoutCommand(u16),
    /// The output did not turn off in time
    NotOff,
    /// The output came up too high, in millivolts. It was switched off.
    TooHigh(u32),
    /// The output did not come up in range in time
    NotInRange { millivolts: u32, status_word: u16 },
}

#[derive(uDebug, Copy, Clone, PartialEq, Eq)]
pub enum VadjOutcome {
    /// VADJ was already in range, nothing was written
    AlreadySet,
    /// VADJ was set to 1.8 V
    Set,
}

type I2cResult<T> = Result<T, I2cError>;

/// Run an I2C transaction, retrying for up to [RETRY_TIMEOUT_MS] while it
/// fails in a way that resolves itself.
fn retry<T>(timer: &Timer, mut transaction: impl FnMut() -> I2cResult<T>) -> I2cResult<T> {
    let deadline = timer.now() + Duration::from_millis(RETRY_TIMEOUT_MS);
    loop {
        match transaction() {
            Err(I2cError::NotAcknowledged | I2cError::ArbitrationLost)
                if timer.now() < deadline =>
            {
                timer.wait(Duration::from_millis(RETRY_DELAY_MS))
            }
            result => return result,
        }
    }
}

struct Regulator<'a> {
    i2c: &'a I2cController,
    timer: &'a Timer,
}

impl Regulator<'_> {
    fn read<const N: usize>(&self, command: u8) -> I2cResult<[u8; N]> {
        let mut data = [0u8; N];
        retry(self.timer, || {
            self.i2c
                .write_read(self.timer, VADJ_REGULATOR, &[command], &mut data)
        })?;
        Ok(data)
    }

    fn read_byte(&self, command: u8) -> I2cResult<u8> {
        Ok(self.read::<1>(command)?[0])
    }

    /// PMBus words are little endian
    fn read_word(&self, command: u8) -> I2cResult<u16> {
        Ok(u16::from_le_bytes(self.read(command)?))
    }

    /// `data` is a command code followed by its value
    fn write(&self, data: &[u8]) -> I2cResult<()> {
        retry(self.timer, || {
            self.i2c.write(self.timer, VADJ_REGULATOR, data)
        })
    }

    fn set_operation(&self, operation: u8) -> I2cResult<()> {
        self.write(&[OPERATION, operation])
    }
}

fn millivolts(mantissa: u16) -> u32 {
    (mantissa as u32 * 1000) >> 12
}

/// Decode a PMBus linear11 time (in milliseconds) to microseconds
fn linear11_us(raw: u16) -> i32 {
    let exponent = (raw as i16) >> 11;
    let mantissa = ((raw << 5) as i16 >> 5) as i32;
    if exponent >= 0 {
        (mantissa * 1000) << exponent
    } else {
        (mantissa * 1000) >> -exponent
    }
}

fn in_range(mantissa: u16) -> bool {
    (VOUT_MIN_OK..=VOUT_MAX_OK).contains(&mantissa)
}

struct State {
    vout_mode: u8,
    operation: u8,
    on_off_config: u8,
    write_protect: u8,
    status_word: u16,
    vout_command: u16,
    vout_max: u16,
    read_vout: u16,
    toff_delay: u16,
    toff_fall: u16,
}

impl State {
    fn read(regulator: &Regulator) -> I2cResult<State> {
        Ok(State {
            vout_mode: regulator.read_byte(VOUT_MODE)?,
            operation: regulator.read_byte(OPERATION)?,
            on_off_config: regulator.read_byte(ON_OFF_CONFIG)?,
            write_protect: regulator.read_byte(WRITE_PROTECT)?,
            status_word: regulator.read_word(STATUS_WORD)?,
            vout_command: regulator.read_word(VOUT_COMMAND)?,
            vout_max: regulator.read_word(VOUT_MAX)?,
            read_vout: regulator.read_word(READ_VOUT)?,
            toff_delay: regulator.read_word(TOFF_DELAY)?,
            toff_fall: regulator.read_word(TOFF_FALL)?,
        })
    }

    /// Voltages are only decoded correctly if VOUT_MODE is as expected.
    fn print(&self, uart: &mut Uart, when: &str) {
        uwriteln!(
            uart,
            "VADJ {}: {} mV, set point {} mV, status {:#x}, VOUT_MODE {:#x}, OPERATION {:#x}, ON_OFF_CONFIG {:#x}, TOFF_DELAY {} us, TOFF_FALL {} us",
            when,
            millivolts(self.read_vout),
            millivolts(self.vout_command),
            self.status_word,
            self.vout_mode,
            self.operation,
            self.on_off_config,
            linear11_us(self.toff_delay),
            linear11_us(self.toff_fall)
        )
        .unwrap();
    }

    /// Whether the regulator is in a state in which we may change the set point
    fn is_expected(&self) -> bool {
        self.vout_mode == EXPECTED_VOUT_MODE
            && self.on_off_config == EXPECTED_ON_OFF_CONFIG
            // Soft-off: an earlier attempt failed after switching the output off
            && (self.operation == OPERATION_ON || self.operation == OPERATION_SOFT_OFF)
            && self.write_protect == 0
            && (self.vout_command < VOUT_MIN_OK || in_range(self.vout_command))
            && self.vout_max >= VOUT_1V8
    }
}

/// Set VADJ to 1.8 V, if it is not already. Prints the regulator state before
/// and after. Takes up to about a second.
pub fn set_vadj_1v8(
    i2c: &I2cController,
    timer: &Timer,
    uart: &mut Uart,
) -> Result<VadjOutcome, VadjError> {
    retry(timer, || {
        i2c.write(timer, PCA9544_ADDRESS, &[PCA9544_PMBUS])
    })
    .map_err(|e| VadjError::I2c(Step::SelectChannel, e))?;
    let result = set_vadj_1v8_selected(&Regulator { i2c, timer }, uart);
    // Deselect the channel also if setting VADJ failed: the system controller
    // expects all channels to be off.
    let deselect = retry(timer, || i2c.write(timer, PCA9544_ADDRESS, &[0]))
        .map_err(|e| VadjError::I2c(Step::DeselectChannel, e));
    let outcome = result?;
    deselect?;
    Ok(outcome)
}

fn set_vadj_1v8_selected(regulator: &Regulator, uart: &mut Uart) -> Result<VadjOutcome, VadjError> {
    let before = State::read(regulator).map_err(|e| VadjError::I2c(Step::ReadState, e))?;
    before.print(uart, "before");
    if before.vout_mode == EXPECTED_VOUT_MODE && in_range(before.read_vout) {
        return Ok(VadjOutcome::AlreadySet);
    }
    if !before.is_expected() {
        return Err(VadjError::UnexpectedState);
    }

    if let Err(e) = change_set_point(regulator) {
        // While the output is off, other supplies feed VADJ to about 0.5 V.
        // Rather keep the output on, but only at a set point known to be safe.
        if !restore_output(regulator, before.vout_command) {
            uwriteln!(
                uart,
                "VADJ: output left off, set point not known to be safe"
            )
            .unwrap();
        }
        return Err(e);
    }
    regulator
        .set_operation(OPERATION_ON)
        .map_err(|e| VadjError::I2c(Step::SwitchOn, e))?;
    let result = wait_for_vadj(regulator);

    // Also printed on failure, to show what went wrong
    if let Ok(after) = State::read(regulator) {
        after.print(uart, "after");
    }
    result.map(|()| VadjOutcome::Set)
}

/// Switch the output off and write the new set point. The regulator only
/// selects a new feedback divider while its output is off: changing the set
/// point from 0 V to 1.8 V with the output on makes the output saturate at
/// about 1.16 V.
fn change_set_point(regulator: &Regulator) -> Result<(), VadjError> {
    regulator
        .set_operation(OPERATION_SOFT_OFF)
        .map_err(|e| VadjError::I2c(Step::SwitchOff, e))?;
    let deadline = regulator.timer.now() + Duration::from_millis(SETTLE_TIMEOUT_MS);
    loop {
        let status_word = regulator
            .read_word(STATUS_WORD)
            .map_err(|e| VadjError::I2c(Step::WaitForOff, e))?;
        if status_word & OFF != 0 {
            break;
        }
        if regulator.timer.now() > deadline {
            return Err(VadjError::NotOff);
        }
    }

    let [low, high] = VOUT_1V8.to_le_bytes();
    regulator
        .write(&[VOUT_COMMAND, low, high])
        .map_err(|e| VadjError::I2c(Step::WriteVoutCommand, e))?;
    let vout_command = regulator
        .read_word(VOUT_COMMAND)
        .map_err(|e| VadjError::I2c(Step::ReadBackVoutCommand, e))?;
    if !in_range(vout_command) {
        return Err(VadjError::VoutCommand(vout_command));
    }
    Ok(())
}

/// Switch the output back on after an attempt failed while it was off. The
/// write of VOUT_COMMAND may have failed halfway, so the set point is read
/// back first. If it is not known to be safe, it is restored to `set_point`,
/// the set point before the attempt. If that fails too, the output stays off.
/// Returns whether the output was switched on.
fn restore_output(regulator: &Regulator, set_point: u16) -> bool {
    let is_safe = |vout_command: I2cResult<u16>| matches!(vout_command, Ok(mantissa) if mantissa <= VOUT_MAX_OK);
    let restored = is_safe(regulator.read_word(VOUT_COMMAND)) || {
        let [low, high] = set_point.to_le_bytes();
        regulator.write(&[VOUT_COMMAND, low, high]).is_ok()
            && is_safe(regulator.read_word(VOUT_COMMAND))
    };
    restored && regulator.set_operation(OPERATION_ON).is_ok()
}

/// Wait for the output to come up in range, with power good. Switch it off if
/// it comes up too high.
fn wait_for_vadj(regulator: &Regulator) -> Result<(), VadjError> {
    let deadline = regulator.timer.now() + Duration::from_millis(SETTLE_TIMEOUT_MS);
    loop {
        let read = || {
            Ok((
                regulator.read_word(READ_VOUT)?,
                regulator.read_word(STATUS_WORD)?,
            ))
        };
        let (read_vout, status_word) = read().map_err(|e| VadjError::I2c(Step::WaitForOn, e))?;
        if read_vout > VOUT_MAX_OK {
            let _ = regulator.set_operation(OPERATION_IMMEDIATE_OFF);
            return Err(VadjError::TooHigh(millivolts(read_vout)));
        }
        if in_range(read_vout) && status_word & POWER_GOOD_N == 0 {
            return Ok(());
        }
        if regulator.timer.now() > deadline {
            return Err(VadjError::NotInRange {
                millivolts: millivolts(read_vout),
                status_word,
            });
        }
    }
}
