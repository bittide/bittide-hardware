// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! smoltcp Device implementation for aligned ringbuffers.
//!
//! This module provides a `Device` implementation that uses scatter/gather units
//! as ringbuffers for point-to-point communication. The ringbuffers must
//! be aligned using the alignment protocol before use.
//!
//! The device uses IP medium for point-to-point links.
//!
//! # Packet Format
//!
//! Each packet consists of:
//! - Header (8 bytes): CRC32 (4 bytes LE) + sequence number (2 bytes LE) + length (2 bytes LE)
//! - Payload (variable, up to MTU)
//!
//! The CRC32 is calculated over sequence + length + payload (i.e., everything except the CRC itself).
//!
//! # Volatile Buffer Handling
//!
//! The receive ringbuffer is volatile - its contents can change at any time due to
//! incoming data. To safely handle this, we:
//! 1. Read the packet header to get CRC, sequence number, and length
//! 2. Copy the entire packet (header + payload) to a local buffer
//! 3. Verify the CRC32 over sequence + length + payload
//! 4. If CRC validates, extract payload and consume packet
//! 5. Track sequence numbers to detect repeated packets

use bittide_hal::hals::scatter_gather_pe::devices::{GatherUnit, ScatterUnit};
use bittide_hal::manual_additions::aligned_ringbuffer::{ReceiveRingbuffer, TransmitRingbuffer};
use crc::{Crc, CRC_32_ISCSI};
use log::trace;
use smoltcp::phy::{self, Device, DeviceCapabilities, Medium};
use smoltcp::time::Instant;

/// Size of packet header: CRC32 (4 bytes) + sequence (2 bytes) + length (2 bytes)
const PACKET_HEADER_SIZE: usize = 8;

/// Minimum IP packet size (20 byte IPv4 header minimum)
const MIN_IP_PACKET_SIZE: usize = 20;

/// CRC-32 (Castagnoli) instance for packet integrity checking.
/// Initialized at compile-time for zero-cost runtime usage.
const CRC: Crc<u32> = Crc::<u32>::new(&CRC_32_ISCSI);

/// Verify packet integrity by checking the CRC32 in the header.
///
/// Returns true if the CRC stored in the first 4 bytes matches the calculated
/// CRC over bytes[4..] (sequence + length + payload). Uses unaligned-safe reads.
///
/// # Arguments
/// * `buffer` - Complete packet buffer including header and payload
fn is_valid(buffer: &[u8]) -> bool {
    if buffer.len() < PACKET_HEADER_SIZE {
        return false;
    }
    let stored_crc = u32::from_le_bytes([buffer[0], buffer[1], buffer[2], buffer[3]]);
    let calculated_crc = CRC.checksum(&buffer[4..]);
    stored_crc == calculated_crc
}

/// Device implementation for ringbuffer communication.
///
/// Provides a simple interface for point-to-point IP communication using
/// scatter/gather units. The ringbuffers handle all internal state management.
///
/// The MTU is automatically calculated from the minimum of the scatter and gather
/// buffer sizes (in bytes), minus space for packet header (which includes CRC32).
pub struct RingbufferDevice {
    rx_buffer: ReceiveRingbuffer,
    tx_buffer: TransmitRingbuffer,
    mtu: usize,
    /// Last valid sequence number we saw (to detect repeated packets)
    last_rx_seq: u16,
    /// Transmit sequence number (incremented for each packet sent)
    tx_seq_num: u16,
}

impl RingbufferDevice {
    /// Create a new ringbuffer device.
    ///
    /// The ringbuffers must already be aligned using the alignment protocol.
    /// The MTU is calculated as the minimum of the RX and TX buffer sizes in bytes.
    pub fn new(rx_buffer: ReceiveRingbuffer, tx_buffer: TransmitRingbuffer) -> Self {
        // Calculate MTU from buffer sizes (each word is 8 bytes)
        // Reserve space for packet header (CRC is part of header)
        let rx_bytes = ScatterUnit::SCATTER_MEMORY_LEN * 8;
        let tx_bytes = GatherUnit::GATHER_MEMORY_LEN * 8;
        let mtu = rx_bytes.min(tx_bytes) - PACKET_HEADER_SIZE;

        Self {
            rx_buffer,
            tx_buffer,
            mtu,
            last_rx_seq: u16::MAX,
            tx_seq_num: 0,
        }
    }

    /// Get the maximum transmission unit (in bytes) for this device.
    pub fn mtu(&self) -> usize {
        self.mtu
    }
}

impl Device for RingbufferDevice {
    type RxToken<'a> = RxToken;
    type TxToken<'a> = TxToken<'a>;

    fn capabilities(&self) -> DeviceCapabilities {
        let mut cap = DeviceCapabilities::default();
        cap.max_transmission_unit = self.mtu;
        cap.medium = Medium::Ip;
        cap
    }

    fn receive(&mut self, _timestamp: Instant) -> Option<(Self::RxToken<'_>, Self::TxToken<'_>)> {
        // Read packet header: CRC32 (4 bytes) + sequence (2 bytes) + length (2 bytes)
        let mut header_buf: [[u8; 8]; 1] = [[0u8; 8]; 1];
        self.rx_buffer.read_slice(&mut header_buf, 0);

        // Extract CRC, sequence number, and length from header
        let _stored_crc = u32::from_le_bytes([
            header_buf[0][0],
            header_buf[0][1],
            header_buf[0][2],
            header_buf[0][3],
        ]);
        let seq_num = u16::from_le_bytes([header_buf[0][4], header_buf[0][5]]);
        let packet_len = u16::from_le_bytes([header_buf[0][6], header_buf[0][7]]) as usize;

        // Check if this is the same packet we saw before (based on sequence number)
        if seq_num == self.last_rx_seq {
            trace!("Detected repeated packet with seq {}", seq_num);
            return None;
        }

        // Validate packet length
        if packet_len < MIN_IP_PACKET_SIZE || packet_len > self.mtu {
            trace!(
                "Invalid packet length: {} (must be {}-{})",
                packet_len,
                MIN_IP_PACKET_SIZE,
                self.mtu
            );
            return None;
        }

        // Calculate total packet size: header + payload
        let total_len = PACKET_HEADER_SIZE + packet_len;
        let num_words = total_len.div_ceil(8);
        let mut buffer = [[0u8; 8]; ScatterUnit::SCATTER_MEMORY_LEN];
        let words = &mut buffer[..num_words];

        // Copy entire packet from ringbuffer (header + payload)
        self.rx_buffer.read_slice(words, 0);

        // Convert to contiguous byte buffer
        let mut packet_buffer = [0u8; ScatterUnit::SCATTER_MEMORY_LEN * 8];
        let mut idx = 0;
        for word in words.iter() {
            for &byte in word.iter() {
                packet_buffer[idx] = byte;
                idx += 1;
                if idx >= total_len {
                    break;
                }
            }
            if idx >= total_len {
                break;
            }
        }

        // Validate CRC
        if !is_valid(&packet_buffer[..total_len]) {
            trace!("CRC validation failed for packet seq {}", seq_num);
            return None;
        }

        trace!(
            "Valid packet: seq {}, payload {} bytes",
            seq_num,
            packet_len
        );
        self.last_rx_seq = seq_num;

        // Extract payload (skip header, exclude CRC)
        let mut payload = [0u8; ScatterUnit::SCATTER_MEMORY_LEN * 8];
        payload[..packet_len]
            .copy_from_slice(&packet_buffer[PACKET_HEADER_SIZE..PACKET_HEADER_SIZE + packet_len]);

        let rx = RxToken {
            buffer: payload,
            length: packet_len,
        };
        let tx = TxToken {
            tx_buffer: &mut self.tx_buffer,
            mtu: self.mtu,
            seq_num: &mut self.tx_seq_num,
        };
        Some((rx, tx))
    }

    fn transmit(&mut self, _timestamp: Instant) -> Option<Self::TxToken<'_>> {
        Some(TxToken {
            tx_buffer: &mut self.tx_buffer,
            mtu: self.mtu,
            seq_num: &mut self.tx_seq_num,
        })
    }
}

/// Receive token for ringbuffer device.
///
/// Contains a local copy of the packet payload that has been validated
/// against CRC32 corruption.
pub struct RxToken {
    buffer: [u8; ScatterUnit::SCATTER_MEMORY_LEN * 8],
    length: usize,
}

impl phy::RxToken for RxToken {
    fn consume<R, F>(self, f: F) -> R
    where
        F: FnOnce(&[u8]) -> R,
    {
        trace!("Consuming validated packet ({} bytes)", self.length);
        f(&self.buffer[..self.length])
    }
}

/// Transmit token for ringbuffer device
pub struct TxToken<'a> {
    tx_buffer: &'a mut TransmitRingbuffer,
    mtu: usize,
    seq_num: &'a mut u16,
}

impl phy::TxToken for TxToken<'_> {
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

        // Prepare buffer: header + payload
        let mut buffer = [0u8; GatherUnit::GATHER_MEMORY_LEN * 8];

        // Header format: CRC32 (4 bytes) + sequence (2 bytes) + length (2 bytes)
        // Checksum added later
        buffer[4..6].copy_from_slice(&(self.seq_num.to_le_bytes()));
        buffer[6..8].copy_from_slice(&(len as u16).to_le_bytes());

        // Let smoltcp fill the packet data
        let result = f(&mut buffer[PACKET_HEADER_SIZE..PACKET_HEADER_SIZE + len]);
        // Calculate total length and seal packet with CRC in header
        let total_len = PACKET_HEADER_SIZE + len;

        let crc = CRC.checksum(&buffer[4..total_len]);
        buffer[0..4].copy_from_slice(&(crc.to_le_bytes()));

        // Convert to word array for ringbuffer
        let num_words = total_len.div_ceil(8);
        let max_words = GatherUnit::GATHER_MEMORY_LEN;
        let mut word_buffer = [[0u8; 8]; GatherUnit::GATHER_MEMORY_LEN];

        for (i, chunk) in buffer[..total_len].chunks(8).enumerate() {
            if i >= max_words {
                break;
            }
            for (j, &byte) in chunk.iter().enumerate() {
                word_buffer[i][j] = byte;
            }
        }

        // Write to ringbuffer starting at offset 0
        let words = &word_buffer[..num_words.min(max_words)];
        self.tx_buffer.write_slice(words, 0);

        trace!(
            "Transmitted packet: checksum {}, seq {}, total length {}",
            crc,
            self.seq_num,
            total_len,
        );
        *self.seq_num = self.seq_num.wrapping_add(1);

        result
    }
}
