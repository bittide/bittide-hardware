// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! smoltcp Device implementation for aligned ringbuffers.
//!
//! This module provides a `Device` implementation that uses scatter/gather units
//! as ringbuffers for point-to-point IP communication. The ringbuffers must
//! be aligned using the alignment protocol before use.
//!
//! The device operates at the IP layer (not Ethernet) since the connections are
//! point-to-point and don't require MAC addressing or routing.
//!
//! The ringbuffer abstraction handles all offset and wrapping management internally,
//! so we simply read/write packets from the start each time.

use bittide_hal::hals::scatter_gather_pe::devices::{GatherUnit, ScatterUnit};
use bittide_hal::manual_additions::aligned_ringbuffer::{ReceiveRingbuffer, TransmitRingbuffer};
use log::{debug, trace};
use smoltcp::phy::{self, Device, DeviceCapabilities, Medium};
use smoltcp::time::Instant;

/// Number of times we can see the same packet ID before considering it stale
const STALE_PACKET_THRESHOLD: u32 = 3;

/// Size of packet ID header in bytes (32-bit identifier)
const PACKET_ID_SIZE: usize = 4;

/// Device implementation for ringbuffer communication.
///
/// Provides a simple interface for point-to-point IP communication using
/// scatter/gather units. The ringbuffers handle all internal state management.
///
/// The MTU is automatically calculated from the minimum of the scatter and gather
/// buffer sizes (in bytes).
pub struct RingbufferDevice {
    rx_buffer: ReceiveRingbuffer,
    tx_buffer: TransmitRingbuffer,
    mtu: usize,
    /// Next packet ID to send
    tx_packet_id: u32,
    /// Last packet ID we received
    rx_packet_id: u32,
    /// How many times we've seen the current RX packet ID
    rx_packet_id_count: u32,
}

impl RingbufferDevice {
    /// Create a new ringbuffer device.
    ///
    /// The ringbuffers must already be aligned using the alignment protocol.
    /// The MTU is calculated as the minimum of the RX and TX buffer sizes in bytes.
    pub fn new(rx_buffer: ReceiveRingbuffer, tx_buffer: TransmitRingbuffer) -> Self {
        // Calculate MTU from buffer sizes (each word is 8 bytes)
        // Reserve space for packet ID header
        let rx_bytes = ScatterUnit::SCATTER_MEMORY_LEN * 8;
        let tx_bytes = GatherUnit::GATHER_MEMORY_LEN * 8;
        let mtu = rx_bytes.min(tx_bytes) - PACKET_ID_SIZE;

        Self {
            rx_buffer,
            tx_buffer,
            mtu,
            tx_packet_id: 1, // Has to be different from first rx packet id
            rx_packet_id: 0,
            rx_packet_id_count: 0,
        }
    }

    /// Get the maximum transmission unit (in bytes) for this device.
    pub fn mtu(&self) -> usize {
        self.mtu
    }
}

impl Device for RingbufferDevice {
    type RxToken<'a> = RxToken<'a>;
    type TxToken<'a> = TxToken<'a>;

    fn capabilities(&self) -> DeviceCapabilities {
        let mut cap = DeviceCapabilities::default();
        cap.max_transmission_unit = self.mtu;
        cap.medium = Medium::Ethernet;
        cap
    }

    fn receive(&mut self, _timestamp: Instant) -> Option<(Self::RxToken<'_>, Self::TxToken<'_>)> {
        // Read the packet ID (first 4 bytes)
        let mut peek_buf: [[u8; 8]; 1] = [[0u8; 8]; 1];
        self.rx_buffer.read_slice(&mut peek_buf, 0);

        // Extract packet ID from first 4 bytes (little-endian)
        let packet_id = u32::from_le_bytes(peek_buf[0][..4].try_into().unwrap());

        // Check if this is the same packet we saw before
        if packet_id == self.rx_packet_id {
            self.rx_packet_id_count += 1;
            trace!(
                "Packet ID {} seen {} times",
                packet_id,
                self.rx_packet_id_count
            );

            // If we've seen it too many times, consider it stale and ignore
            if self.rx_packet_id_count >= STALE_PACKET_THRESHOLD {
                trace!("Packet {} is stale, ignoring", packet_id);
                return None;
            }
        } else {
            // New packet ID
            trace!(
                "New packet ID: {} (previous: {})",
                packet_id,
                self.rx_packet_id
            );
            self.rx_packet_id = packet_id;
            self.rx_packet_id_count = 1;
        }

        let rx = RxToken {
            rx_buffer: &mut self.rx_buffer,
            mtu: self.mtu,
        };
        let tx = TxToken {
            tx_buffer: &mut self.tx_buffer,
            mtu: self.mtu,
            packet_id: &mut self.tx_packet_id,
        };
        Some((rx, tx))
    }

    fn transmit(&mut self, _timestamp: Instant) -> Option<Self::TxToken<'_>> {
        Some(TxToken {
            tx_buffer: &mut self.tx_buffer,
            mtu: self.mtu,
            packet_id: &mut self.tx_packet_id,
        })
    }
}

/// Receive token for ringbuffer device
pub struct RxToken<'a> {
    rx_buffer: &'a mut ReceiveRingbuffer,
    mtu: usize,
}

impl phy::RxToken for RxToken<'_> {
    fn consume<R, F>(self, f: F) -> R
    where
        F: FnOnce(&[u8]) -> R,
    {
        // Read packet from ringbuffer starting at offset 0
        // First 4 bytes are packet ID, rest is actual packet data
        let total_bytes = self.mtu + PACKET_ID_SIZE;
        let num_words = total_bytes.div_ceil(8);

        // Allocate buffer based on actual MTU + packet ID
        let max_words = ScatterUnit::SCATTER_MEMORY_LEN;
        let mut buffer = [[0u8; 8]; ScatterUnit::SCATTER_MEMORY_LEN];
        let words = &mut buffer[..num_words.min(max_words)];

        self.rx_buffer.read_slice(words, 0);

        // Convert to byte slice for processing
        let mut bytes = [0u8; ScatterUnit::SCATTER_MEMORY_LEN * 8];
        let mut idx = 0;
        for word in words.iter() {
            for &byte in word.iter() {
                bytes[idx] = byte;
                idx += 1;
                if idx >= total_bytes {
                    break;
                }
            }
            if idx >= total_bytes {
                break;
            }
        }

        // Extract packet ID for logging
        let packet_id = u32::from_le_bytes(bytes[..4].try_into().unwrap());

        // Process the packet (skip the 4-byte packet ID header)
        let result = f(&bytes[PACKET_ID_SIZE..PACKET_ID_SIZE + self.mtu]);
        trace!(
            "Consumed packet {} ({} bytes from RX buffer)",
            packet_id,
            self.mtu
        );
        result
    }
}

/// Transmit token for ringbuffer device
pub struct TxToken<'a> {
    tx_buffer: &'a mut TransmitRingbuffer,
    mtu: usize,
    packet_id: &'a mut u32,
}

impl phy::TxToken for TxToken<'_> {
    fn consume<R, F>(self, len: usize, f: F) -> R
    where
        F: FnOnce(&mut [u8]) -> R,
    {
        // Ensure the packet doesn't exceed MTU
        if len > self.mtu {
            debug!("Packet length {} exceeds MTU {}", len, self.mtu);
        }
        assert!(
            len <= self.mtu,
            "Packet length {} exceeds MTU {}",
            len,
            self.mtu
        );

        // Prepare buffer: packet ID (4 bytes) + packet data
        let mut bytes = [0u8; GatherUnit::GATHER_MEMORY_LEN * 8];

        // Write packet ID in first 4 bytes (little-endian)
        let packet_id_bytes = self.packet_id.to_le_bytes();
        bytes[0] = packet_id_bytes[0];
        bytes[1] = packet_id_bytes[1];
        bytes[2] = packet_id_bytes[2];
        bytes[3] = packet_id_bytes[3];

        // Let smoltcp fill the packet data after the ID
        let packet = &mut bytes[PACKET_ID_SIZE..PACKET_ID_SIZE + len];
        let result = f(packet);

        // Convert to word array for ringbuffer (packet ID + data)
        let total_len = PACKET_ID_SIZE + len;
        let num_words = total_len.div_ceil(8);
        let max_words = GatherUnit::GATHER_MEMORY_LEN;
        let mut buffer = [[0u8; 8]; GatherUnit::GATHER_MEMORY_LEN];

        for (i, chunk) in bytes[..total_len].chunks(8).enumerate() {
            if i >= max_words {
                break;
            }
            for (j, &byte) in chunk.iter().enumerate() {
                buffer[i][j] = byte;
            }
        }

        // Write to ringbuffer starting at offset 0
        let words = &buffer[..num_words.min(max_words)];
        self.tx_buffer.write_slice(words, 0);

        trace!(
            "Transmitted packet {} ({} bytes to TX buffer)",
            *self.packet_id,
            len
        );

        // Increment packet ID for next transmission
        *self.packet_id = self.packet_id.wrapping_add(1);

        result
    }
}
