// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! smoltcp [`Device`] implementation for aligned ring_buffers.
//!
//! Provides point-to-point IP communication using scatter/gather units
//! as ring_buffers. The ring_buffers must be aligned using the alignment
//! protocol before constructing a [`RingBufferDevice`].
//!
//! # Packet format
//!
//! Each packet consists of:
//! - Header (8 bytes): CRC32 (4 bytes LE) + sequence number (2 bytes LE) + length (2 bytes LE)
//! - Payload (variable, up to MTU)
//!
//! The CRC32 covers sequence + length + payload (everything except the CRC itself).
//!
//! # Volatile buffer handling
//!
//! The receive ring_buffer contents can change at any time due to incoming
//! data. To handle this safely, [`Device::receive`] disables the buffer,
//! validates the CRC, and re-enables it after consumption. Sequence
//! numbers detect repeated packets.
use bittide_hal::manual_additions::ring_buffer::{
    AlignedReceiveBuffer, ReceiveRingBufferInterface, TransmitRingBufferInterface,
};

use crc::{Crc, CRC_32_ISCSI};
use log::{debug, error, trace, warn};
use smoltcp::phy::{self, Device, DeviceCapabilities, Medium};
use smoltcp::time::Instant;

const PACKET_HEADER_SIZE: usize = 8;

/// Minimum IP packet size (20-byte IPv4 header).
const MIN_IP_PACKET_SIZE: usize = 20;

const CRC: Crc<u32> = Crc::<u32>::new(&CRC_32_ISCSI);

/// smoltcp [`Device`] backed by a pair of aligned ring_buffers.
///
/// MTU is derived from the smaller of the RX/TX buffer sizes (in bytes),
/// minus the 8-byte packet header, capped at 1500.
pub struct RingBufferDevice<Rx, Tx> {
    rx_buffer: AlignedReceiveBuffer<Rx, Tx>,
    tx_buffer: Tx,
    mtu: usize,
    /// Tracks last received CRC to detect repeated packets.
    last_crc: u32,
    tx_seq_num: u16,
}

impl<Rx, Tx> RingBufferDevice<Rx, Tx>
where
    Rx: ReceiveRingBufferInterface + 'static,
    Tx: TransmitRingBufferInterface + 'static,
{
    /// Create a new ring_buffer device from an already-aligned buffer pair.
    pub fn new(rx_buffer: AlignedReceiveBuffer<Rx, Tx>, tx_buffer: Tx) -> Self {
        let mtu = (Rx::DATA_LEN * 8).min(1500).min(Tx::DATA_LEN * 8) - PACKET_HEADER_SIZE;
        assert!(rx_buffer.is_aligned(), "RX buffer is not aligned");
        debug!(
            "ring_buffer device init rx_words {} tx_words {} mtu {}",
            Rx::DATA_LEN,
            Tx::DATA_LEN,
            mtu
        );

        Self {
            rx_buffer,
            tx_buffer,
            mtu,
            last_crc: 0,
            tx_seq_num: 0,
        }
    }

    pub fn mtu(&self) -> usize {
        self.mtu
    }
}

impl<Rx, Tx> Device for RingBufferDevice<Rx, Tx>
where
    Rx: ReceiveRingBufferInterface + 'static,
    Tx: TransmitRingBufferInterface + 'static,
{
    type RxToken<'a>
        = RxToken<'a, Rx>
    where
        Self: 'a;
    type TxToken<'a> = TxToken<'a, Tx>;

    fn capabilities(&self) -> DeviceCapabilities {
        let mut cap = DeviceCapabilities::default();
        cap.max_transmission_unit = self.mtu;
        cap.medium = Medium::Ip;
        cap
    }

    fn receive(&mut self, _timestamp: Instant) -> Option<(Self::RxToken<'_>, Self::TxToken<'_>)> {
        if !(self.rx_buffer.buffer.get_enable()) {
            error!("RX buffer is not enabled at start of receive. Enabling and returning None.");
            self.rx_buffer.buffer.set_enable(true);
            return None;
        }
        // Disable RX buffer to prevent it from advancing while we read
        self.rx_buffer.buffer.set_enable(false);

        // Separate disabling the buffer from reading its contents
        core::sync::atomic::fence(core::sync::atomic::Ordering::AcqRel);

        // Get direct pointer to hardware buffer (base pointer points to array of [u8; 8])
        let hw_buffer_ptr = self.rx_buffer.buffer.base_ptr() as *const u8;

        unsafe {
            // Read header fields directly from hardware buffer using volatile reads
            // Header format: CRC32 (4 bytes) + sequence (2 bytes) + length (2 bytes)
            let crc = *(hw_buffer_ptr as *const u32);
            let seq_num = *(hw_buffer_ptr.add(4) as *const u16);
            let packet_len = *(hw_buffer_ptr.add(6) as *const u16) as usize;

            core::sync::atomic::fence(core::sync::atomic::Ordering::AcqRel);

            // Check if this is the same packet we saw before (based on CRC)
            if crc == self.last_crc {
                debug!("Detected repeated packet with CRC {crc}");
                self.rx_buffer.buffer.set_enable(true);
                return None;
            }

            if packet_len < MIN_IP_PACKET_SIZE || packet_len > self.mtu {
                warn!(
                    "Invalid packet length: {} (must be {}-{})",
                    packet_len, MIN_IP_PACKET_SIZE, self.mtu
                );
                self.rx_buffer.buffer.set_enable(true);
                return None;
            }

            let total_len = PACKET_HEADER_SIZE + packet_len;
            debug!("rx packet seq {seq_num} len {packet_len} total {total_len}");

            // Create slice directly from hardware buffer for CRC validation
            // We need to use volatile reads to ensure the hardware buffer is read correctly
            let packet_bytes = core::slice::from_raw_parts(hw_buffer_ptr, total_len);

            trace!("Calculating CRC for received packet");
            let calculated_crc = CRC.checksum(&packet_bytes[4..]);

            core::sync::atomic::fence(core::sync::atomic::Ordering::AcqRel);
            if calculated_crc != crc {
                debug!("CRC validation failed for packet seq {seq_num}");
                self.rx_buffer.buffer.set_enable(true);
                return None;
            }

            trace!("Valid packet: seq {seq_num}, payload {packet_len} bytes");
            self.last_crc = crc;

            // Create token that will process directly from hardware buffer
            let rx = RxToken {
                rx_buffer: &self.rx_buffer.buffer,
                packet_len,
            };
            let tx = TxToken {
                tx_buffer: &mut self.tx_buffer,
                mtu: self.mtu,
                seq_num: &mut self.tx_seq_num,
            };
            Some((rx, tx))
        }
    }

    fn transmit(&mut self, _timestamp: Instant) -> Option<Self::TxToken<'_>> {
        self.tx_buffer.set_enable(true);
        Some(TxToken {
            tx_buffer: &mut self.tx_buffer,
            mtu: self.mtu,
            seq_num: &mut self.tx_seq_num,
        })
    }
}

/// Receive token that reads a packet directly from the hardware buffer.
///
/// Re-enables the RX buffer after the packet is consumed.
pub struct RxToken<'a, Rx: ReceiveRingBufferInterface> {
    rx_buffer: &'a Rx,
    packet_len: usize,
}

impl<Rx: ReceiveRingBufferInterface> phy::RxToken for RxToken<'_, Rx> {
    fn consume<R, F>(self, f: F) -> R
    where
        F: FnOnce(&[u8]) -> R,
    {
        trace!(
            "Consuming packet directly from hardware buffer ({} bytes)",
            self.packet_len
        );

        let result = unsafe {
            // Create slice pointing to payload in hardware buffer (skip header)
            let payload_ptr = (self.rx_buffer.base_ptr() as *const u8).add(PACKET_HEADER_SIZE);
            let payload_slice = core::slice::from_raw_parts(payload_ptr, self.packet_len);

            // Let smoltcp process the packet
            let result = f(payload_slice);

            // Memory fence: Ensure all reads from the buffer complete before re-enabling
            core::sync::atomic::fence(core::sync::atomic::Ordering::AcqRel);
            result
        };

        // Re-enable the RX buffer so it can advance to the next packet
        self.rx_buffer.set_enable(true);
        trace!("RX buffer re-enabled after packet consumption");
        result
    }
}

/// Transmit token that writes a packet directly into the hardware buffer.
pub struct TxToken<'a, Tx>
where
    Tx: TransmitRingBufferInterface,
{
    tx_buffer: &'a mut Tx,
    mtu: usize,
    seq_num: &'a mut u16,
}

impl<Tx> phy::TxToken for TxToken<'_, Tx>
where
    Tx: TransmitRingBufferInterface,
{
    fn consume<R, F>(self, len: usize, f: F) -> R
    where
        F: FnOnce(&mut [u8]) -> R,
    {
        assert!(
            len <= self.mtu,
            "Packet length {} exceeds MTU {}",
            len,
            self.mtu
        );

        let total_len = PACKET_HEADER_SIZE + len;
        let hw_buffer_ptr = self.tx_buffer.base_ptr() as *mut u8;

        unsafe {
            // Write header fields directly to hardware buffer
            // Header format: CRC32 (4 bytes) + sequence (2 bytes) + length (2 bytes)
            trace!("Writing header fields to hardware buffer");

            // Write sequence and length (CRC written later after payload)
            (hw_buffer_ptr.add(4) as *mut u16).write_unaligned(self.seq_num.to_le());
            (hw_buffer_ptr.add(6) as *mut u16).write_unaligned((len as u16).to_le());

            // Create payload slice directly in hardware buffer for smoltcp to write to
            trace!("Creating payload slice in hardware buffer for smoltcp");
            let payload_slice =
                core::slice::from_raw_parts_mut(hw_buffer_ptr.add(PACKET_HEADER_SIZE), len);

            // Let smoltcp fill the packet data directly into hardware buffer
            trace!("Filling payload using provided closure");
            let result = f(payload_slice);

            // Calculate CRC over sequence + length + payload
            // Write CRC to header

            trace!("Calculating CRC for packet");
            let crc_data = core::slice::from_raw_parts(hw_buffer_ptr.add(4), total_len - 4);
            let crc: u32 = CRC.checksum(crc_data);
            (hw_buffer_ptr as *mut u32).write_unaligned(crc.to_le());

            trace!(
                "tx packet: crc {}, seq {}, len {}",
                crc,
                self.seq_num,
                total_len,
            );
            *self.seq_num = self.seq_num.wrapping_add(1);

            result
        }
    }
}
