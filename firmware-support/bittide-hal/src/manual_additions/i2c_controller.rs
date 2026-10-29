// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
//! Transactions on top of the byte-level commands of the `I2cController`
//! device. Addresses are 7-bit I2C addresses.
use crate::{
    manual_additions::timer::{Duration, Instant},
    shared_devices::{I2cController, Timer},
    types::{I2cAck, I2cCommand, I2cTransfer},
};
use clash_bindings::bitvector::BitVector;
use ufmt::derive::uDebug;

/// Maximum time a single command may take. A byte takes 90 us at 100 kHz, but
/// targets may stretch the clock.
const COMMAND_TIMEOUT_MS: u64 = 10;

/// Maximum time to wait for another controller to finish its transaction.
const BUS_FREE_TIMEOUT_MS: u64 = 100;

#[derive(uDebug, Copy, Clone, PartialEq, Eq)]
pub enum I2cError {
    /// The target did not acknowledge a byte
    NotAcknowledged,
    /// Another controller took over the bus. The transaction can be retried.
    ArbitrationLost,
    /// A command did not finish in time, for example because a target holds
    /// SCL low, or another controller did not release the bus in time
    Timeout,
}

fn address_byte(address: u8, read: bool) -> u8 {
    (address << 1) | (read as u8)
}

impl I2cController {
    /// Perform a single command and wait for it to finish.
    ///
    /// The command is only performed once `commit` is set: the CPU may write the
    /// multi-byte command with multiple bus transactions, in any order.
    fn run(&self, timer: &Timer, command: I2cCommand) -> Result<(), I2cError> {
        let deadline: Instant = timer.now() + Duration::from_millis(COMMAND_TIMEOUT_MS);
        self.set_command(command);
        self.set_commit(true);
        loop {
            if !self.commit() {
                break;
            }
            if timer.now() > deadline {
                return Err(I2cError::Timeout);
            }
        }
        if self.arbitration_lost() {
            return Err(I2cError::ArbitrationLost);
        }
        Ok(())
    }

    /// Wait until no other controller is using the bus. The I2C core does not
    /// check this before generating a START condition.
    fn wait_for_bus_free(&self, timer: &Timer) -> Result<(), I2cError> {
        let deadline: Instant = timer.now() + Duration::from_millis(BUS_FREE_TIMEOUT_MS);
        while self.bus_busy() {
            if timer.now() > deadline {
                return Err(I2cError::Timeout);
            }
        }
        Ok(())
    }

    fn write_byte(&self, timer: &Timer, start: bool, byte: u8, stop: bool) -> Result<(), I2cError> {
        let transfer = I2cTransfer::WriteByte(BitVector::new([byte]).unwrap());
        self.run(
            timer,
            I2cCommand {
                start,
                transfer,
                stop,
            },
        )?;
        match self.ack_received() {
            I2cAck::Acknowledge => Ok(()),
            I2cAck::NotAcknowledge => Err(I2cError::NotAcknowledged),
        }
    }

    fn read_byte(&self, timer: &Timer, last: bool) -> Result<u8, I2cError> {
        let ack = if last {
            I2cAck::NotAcknowledge
        } else {
            I2cAck::Acknowledge
        };
        self.run(
            timer,
            I2cCommand {
                start: false,
                transfer: I2cTransfer::ReadByte(ack),
                stop: last,
            },
        )?;
        Ok(self.read_data().into_inner()[0])
    }

    /// Generate a STOP condition. Used to release the bus after an error.
    fn stop(&self, timer: &Timer) {
        let _ = self.run(
            timer,
            I2cCommand {
                start: false,
                transfer: I2cTransfer::NoTransfer,
                stop: true,
            },
        );
    }

    /// Releases the bus if a transaction failed halfway.
    fn finish<T>(&self, timer: &Timer, result: Result<T, I2cError>) -> Result<T, I2cError> {
        match result {
            // After losing arbitration, the bus belongs to another controller.
            Err(I2cError::NotAcknowledged) | Err(I2cError::Timeout) => self.stop(timer),
            _ => {}
        }
        result
    }

    /// Write `data` to the target at `address`.
    pub fn write(&self, timer: &Timer, address: u8, data: &[u8]) -> Result<(), I2cError> {
        // Not part of the transaction below: we must not generate a STOP
        // condition while another controller owns the bus.
        self.wait_for_bus_free(timer)?;
        let result = (|| {
            self.write_byte(timer, true, address_byte(address, false), data.is_empty())?;
            for (i, byte) in data.iter().enumerate() {
                self.write_byte(timer, false, *byte, i == data.len() - 1)?;
            }
            Ok(())
        })();
        self.finish(timer, result)
    }

    /// Write `data` to the target at `address`, then read `buffer.len()` bytes
    /// after a repeated START condition. `buffer` must not be empty.
    pub fn write_read(
        &self,
        timer: &Timer,
        address: u8,
        data: &[u8],
        buffer: &mut [u8],
    ) -> Result<(), I2cError> {
        self.wait_for_bus_free(timer)?;
        let result = (|| {
            self.write_byte(timer, true, address_byte(address, false), false)?;
            for byte in data {
                self.write_byte(timer, false, *byte, false)?;
            }
            self.write_byte(timer, true, address_byte(address, true), false)?;
            let len = buffer.len();
            for (i, slot) in buffer.iter_mut().enumerate() {
                *slot = self.read_byte(timer, i == len - 1)?;
            }
            Ok(())
        })();
        self.finish(timer, result)
    }

    /// Whether a target acknowledges `address`.
    pub fn probe(&self, timer: &Timer, address: u8) -> Result<bool, I2cError> {
        match self.write(timer, address, &[]) {
            Ok(()) => Ok(true),
            Err(I2cError::NotAcknowledged) => Ok(false),
            Err(e) => Err(e),
        }
    }
}
