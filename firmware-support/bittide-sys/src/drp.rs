// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Driver for the GTH transceiver Dynamic Reconfiguration Port (DRP).
//!
//! Wraps the `Transceivers` device's `drp_request` / `drp_status` registers
//! (see `Bittide.Transceiver.Wishbone` and `Bittide.Transceiver.Drp`) to perform
//! single read / write / read-modify-write transactions on any GTH channel's DRP
//! attribute space. This is the foundation for runtime transceiver introspection
//! such as the eye scan (RX margin analysis).
//!
//! All transactions block until the hardware reports completion (or a timeout).

use bittide_hal::shared_devices::Transceivers;
use bittide_hal::types::{DrpRequest, DrpStatus};
use clash_bindings::bitvector::BitVector;
use clash_bindings::unsigned::Unsigned;
use ufmt::derive::uDebug;

/// Maximum number of `drp_status` polls before giving up. A DRP transaction
/// completes in well under ~600 cycles, so any realistic number of MMIO polls is
/// plenty; this only guards against a wedged interconnect.
const DRP_POLL_LIMIT: u32 = 1_000_000;

/// Errors that can occur during a DRP transaction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, uDebug)]
pub enum DrpError {
    /// The hardware watchdog reported that the channel did not assert `drprdy`.
    HwTimeout,
    /// `drp_status.busy` never cleared within [`DRP_POLL_LIMIT`] polls.
    PollTimeout,
}

/// DRP accessor over a [`Transceivers`] device.
pub struct Drp<'a> {
    transceivers: &'a Transceivers,
}

impl<'a> Drp<'a> {
    /// Create a DRP accessor for the given transceiver block.
    pub fn new(transceivers: &'a Transceivers) -> Self {
        Self { transceivers }
    }

    /// Poll `drp_status` until the in-flight transaction completes.
    fn wait(&self) -> Result<DrpStatus, DrpError> {
        for _ in 0..DRP_POLL_LIMIT {
            let status = self.transceivers.drp_status();
            if !status.busy {
                return if status.timed_out {
                    Err(DrpError::HwTimeout)
                } else {
                    Ok(status)
                };
            }
        }
        Err(DrpError::PollTimeout)
    }

    /// Read a 16-bit DRP register from `channel` at `address` (9-bit).
    pub fn read(&self, channel: usize, address: u16) -> Result<u16, DrpError> {
        self.transceivers.set_drp_request(DrpRequest {
            channel: channel_to_unsigned(channel),
            address: address_to_bv(address),
            write_data: data_to_bv(0),
            is_write: false,
        });
        let status = self.wait()?;
        Ok(bv_to_u16(status.read_data))
    }

    /// Write a 16-bit `data` word to `channel`'s DRP register at `address`.
    pub fn write(&self, channel: usize, address: u16, data: u16) -> Result<(), DrpError> {
        self.transceivers.set_drp_request(DrpRequest {
            channel: channel_to_unsigned(channel),
            address: address_to_bv(address),
            write_data: data_to_bv(data),
            is_write: true,
        });
        self.wait()?;
        Ok(())
    }

    /// Read-modify-write: replace the bits selected by `mask` with `value`,
    /// leaving the rest untouched. Skips the write if nothing changes.
    pub fn modify(
        &self,
        channel: usize,
        address: u16,
        mask: u16,
        value: u16,
    ) -> Result<(), DrpError> {
        let old = self.read(channel, address)?;
        let new = (old & !mask) | (value & mask);
        if new != old {
            self.write(channel, address, new)?;
        }
        Ok(())
    }
}

fn channel_to_unsigned(channel: usize) -> Unsigned<8, u8> {
    // Safe: N (8) equals the number of bits in the backing type (u8).
    unsafe { Unsigned::new_unchecked(channel as u8) }
}

fn address_to_bv(address: u16) -> BitVector<9, 2> {
    // Mask to the 9-bit DRP address space before the unchecked construction.
    let u: Unsigned<9, u16> = unsafe { Unsigned::new_unchecked(address & 0x1FF) };
    u.into()
}

fn data_to_bv(data: u16) -> BitVector<16, 2> {
    // Safe: N (16) equals the number of bits in the backing type (u16).
    let u: Unsigned<16, u16> = unsafe { Unsigned::new_unchecked(data) };
    u.into()
}

fn bv_to_u16(bv: BitVector<16, 2>) -> u16 {
    let u: Unsigned<16, u16> = bv.into();
    u.into_inner()
}
