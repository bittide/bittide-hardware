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
use bittide_hal::manual_additions::addressable_buffer::Aligned;
use bittide_hal::manual_additions::ringbuffer::{
    AlignedReceiveBuffer, ReceiveRingbufferInterface, TransmitRingbufferInterface,
};

use crc::{Crc, CRC_32_ISCSI};
use log::{debug, trace, warn};
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
pub struct RingbufferDevice<Rx, Tx>
where
    Rx: ReceiveRingbufferInterface,
    Tx: TransmitRingbufferInterface,
{
    rx_buffer: AlignedReceiveBuffer<Rx, Tx>,
    tx_buffer: Tx,
    mtu: usize,
    /// Last valid sequence number we saw (to detect repeated packets)
    last_rx_seq: u16,
    /// Transmit sequence number (incremented for each packet sent)
    tx_seq_num: u16,
}

impl<Rx, Tx> RingbufferDevice<Rx, Tx>
where
    Rx: ReceiveRingbufferInterface + 'static,
    Tx: TransmitRingbufferInterface + 'static,
{
    /// Create a new ringbuffer device.
    ///
    /// The ringbuffers must already be aligned using the alignment protocol.
    /// The MTU is calculated as the minimum of the RX and TX buffer sizes in bytes.
    pub fn new(rx_buffer: AlignedReceiveBuffer<Rx, Tx>, tx_buffer: Tx) -> Self {
        // Calculate MTU from buffer sizes (each word is 8 bytes)
        // Reserve space for packet header (CRC is part of header)
        let mtu = (Rx::DATA_LEN * 8).min(1500).min(Tx::DATA_LEN * 8) - PACKET_HEADER_SIZE;
        assert!(rx_buffer.is_aligned(), "RX buffer is not aligned ");
        debug!(
            "ringbuffer device init rx_words {} tx_words {} mtu {}",
            Rx::DATA_LEN,
            Tx::DATA_LEN,
            mtu
        );

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

impl<Rx, Tx> Device for RingbufferDevice<Rx, Tx>
where
    Rx: ReceiveRingbufferInterface + 'static,
    Tx: TransmitRingbufferInterface + 'static,
    [(); Rx::DATA_LEN * 8]:,
    [(); Tx::DATA_LEN * 8]:,
{
    type RxToken<'a>
        = RxToken<{ Rx::DATA_LEN * 8 }>
    where
        [(); Rx::DATA_LEN * 8]:;
    type TxToken<'a>
        = TxToken<'a, Tx, { Tx::DATA_LEN * 8 }>
    where
        [(); Tx::DATA_LEN * 8]:;

    fn capabilities(&self) -> DeviceCapabilities {
        let mut cap = DeviceCapabilities::default();
        cap.max_transmission_unit = self.mtu;
        cap.medium = Medium::Ip;
        cap
    }

    fn receive(&mut self, _timestamp: Instant) -> Option<(Self::RxToken<'_>, Self::TxToken<'_>)> {
        // Allocate aligned buffer for reading from ringbuffer
        let mut packet_buffer = Aligned::new([[0u8; 8]; Rx::DATA_LEN]);

        // Read first word containing the header: CRC32 (4 bytes) + sequence (2 bytes) + length (2 bytes)
        self.rx_buffer
            .read_slice(&mut packet_buffer.get_mut()[0..1], 0);

        // Extract CRC, sequence number, and length from header using direct pointer reads
        let header_ptr = packet_buffer.get()[0].as_ptr();
        let _stored_crc = unsafe { (header_ptr as *const u32).read_unaligned() };
        let seq_num = unsafe { (header_ptr.add(4) as *const u16).read_unaligned() };
        let packet_len = unsafe { (header_ptr.add(6) as *const u16).read_unaligned() } as usize;

        // Check if this is the same packet we saw before (based on sequence number)
        if seq_num == self.last_rx_seq {
            trace!("Detected repeated packet with seq {}", seq_num);
            return None;
        }

        // Validate packet length
        if packet_len < MIN_IP_PACKET_SIZE || packet_len > self.mtu {
            warn!(
                "Invalid packet length: {} (must be {}-{})",
                packet_len, MIN_IP_PACKET_SIZE, self.mtu
            );
            return None;
        }

        // Calculate total packet size: header + payload
        let total_len = PACKET_HEADER_SIZE + packet_len;
        let num_words = total_len.div_ceil(8);
        trace!(
            "rx packet seq {} len {} total {} words {}",
            seq_num,
            packet_len,
            total_len,
            num_words
        );

        // Read remaining words if any (we already read the first word)
        if num_words > 1 {
            self.rx_buffer
                .read_slice(&mut packet_buffer.get_mut()[1..num_words], 1);
        }

        // Flatten to bytes for CRC validation
        let packet_bytes = unsafe {
            core::slice::from_raw_parts(packet_buffer.get().as_ptr() as *const u8, total_len)
        };

        // Validate CRC
        if !is_valid(packet_bytes) {
            trace!("CRC validation failed for packet seq {}", seq_num);
            return None;
        }

        trace!(
            "Valid packet: seq {}, payload {} bytes",
            seq_num,
            packet_len
        );
        self.last_rx_seq = seq_num;

        // Extract payload (skip header)
        let mut payload = Aligned::new([0u8; Rx::DATA_LEN * 8]);
        let payload_bytes = unsafe {
            core::slice::from_raw_parts(
                (packet_buffer.get().as_ptr() as *const u8).add(PACKET_HEADER_SIZE),
                packet_len,
            )
        };
        payload.get_mut()[..packet_len].copy_from_slice(payload_bytes);

        let rx = RxToken {
            buffer: payload,
            length: packet_len,
        };
        trace!("rx token ready len {}", packet_len);
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
pub struct RxToken<const RX_BYTES: usize> {
    buffer: Aligned<[u8; RX_BYTES]>,
    length: usize,
}

impl<const RX_BYTES: usize> phy::RxToken for RxToken<RX_BYTES> {
    fn consume<R, F>(self, f: F) -> R
    where
        F: FnOnce(&[u8]) -> R,
    {
        trace!("Consuming validated packet ({} bytes)", self.length);
        f(&self.buffer.get()[..self.length])
    }
}

/// Transmit token for ringbuffer device
pub struct TxToken<'a, Tx, const TX_WORDS: usize>
where
    Tx: TransmitRingbufferInterface,
{
    tx_buffer: &'a mut Tx,
    mtu: usize,
    seq_num: &'a mut u16,
}

impl<Tx, const TX_WORDS: usize> phy::TxToken for TxToken<'_, Tx, TX_WORDS>
where
    Tx: TransmitRingbufferInterface,
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
        trace!("tx consume len {} mtu {}", len, self.mtu);

        // Prepare aligned buffer: header + payload
        let mut buffer = Aligned::new([[0u8; 8]; TX_WORDS]);

        // Write header fields using direct pointer writes
        // Header format: CRC32 (4 bytes) + sequence (2 bytes) + length (2 bytes)
        let header_ptr = buffer.get_mut().as_mut_ptr() as *mut u8;
        unsafe {
            // Write sequence and length (CRC written later after payload)
            (header_ptr.add(4) as *mut u16).write_unaligned(self.seq_num.to_le());
            (header_ptr.add(6) as *mut u16).write_unaligned((len as u16).to_le());
        }

        // Let smoltcp fill the packet data
        let payload_slice =
            unsafe { core::slice::from_raw_parts_mut(header_ptr.add(PACKET_HEADER_SIZE), len) };
        let result = f(payload_slice);

        // Calculate total length and seal packet with CRC in header
        let total_len = PACKET_HEADER_SIZE + len;
        let crc_data = unsafe { core::slice::from_raw_parts(header_ptr.add(4), total_len - 4) };
        let crc = CRC.checksum(crc_data);
        unsafe {
            (header_ptr as *mut u32).write_unaligned(crc.to_le());
        }

        // Calculate number of words needed
        let num_words = total_len.div_ceil(8);
        trace!(
            "tx packet len {} total {} words {}",
            len,
            total_len,
            num_words
        );

        // Write to ringbuffer starting at offset 0
        self.tx_buffer.write_slice(&buffer.get()[..num_words], 0);

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
