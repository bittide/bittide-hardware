// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Aligned Ringbuffer Abstraction
//!
//! Provides `ReceiveRingbuffer` and `TransmitRingbuffer` wrappers around
//! `ScatterUnit` (RX) and `GatherUnit` (TX) for point-to-point communication.
//! See [Ringbuffer Alignment Protocol](../../../docs/sections/ringbuffer-alignment.md)
//! for alignment details.
//!
//! Both types automatically handle buffer wrapping when reads or writes extend
//! beyond the buffer boundary.
//!
//! # Reliability
//!
//! The physical link is unreliable due to hardware/CPU pointer races. Use a
//! higher-level protocol (e.g., TCP/IP via smoltcp) for reliable communication.

use crate::hals::scatter_gather_pe::devices::{GatherUnit, ScatterUnit};

/// Alignment protocol marker values
const ALIGNMENT_EMPTY: u64 = 0;
const ALIGNMENT_ANNOUNCE: u64 = 0xBADC0FFEE;
const ALIGNMENT_ACKNOWLEDGE: u64 = 0xDEADABBA;

/// Find the RX alignment offset by performing a two-phase discovery protocol.
///
/// **Phase 1 (Discovery):** Writes `ALIGNMENT_ANNOUNCE` to TX buffer index 0,
/// then scans the entire RX buffer until it finds either `ALIGNMENT_ANNOUNCE`
/// or `ALIGNMENT_ACKNOWLEDGE` from the remote node. The index where the marker
/// is found becomes the RX alignment offset.
///
/// **Phase 2 (Confirmation):** Writes `ALIGNMENT_ACKNOWLEDGE` to TX buffer
/// index 0, then polls the RX buffer at the discovered offset until receiving
/// an acknowledgment from the remote node, confirming bidirectional alignment.
///
/// Returns the discovered RX alignment offset.
///
/// # Note
///
/// This function will loop indefinitely until alignment succeeds. Both nodes
/// must run this procedure simultaneously for it to complete.
pub fn find_alignment_offset(tx: &TransmitRingbuffer, rx: &ReceiveRingbuffer) -> usize {
    let buffer_size = ScatterUnit::SCATTER_MEMORY_LEN;

    // Initialize TX buffer: write ANNOUNCE at index 0, clear the rest
    let announce_pattern = [ALIGNMENT_ANNOUNCE.to_le_bytes()];
    tx.write_slice(&announce_pattern, 0);

    let empty_pattern: [[u8; 8]; 1] = [ALIGNMENT_EMPTY.to_le_bytes()];
    for i in 1..buffer_size {
        tx.write_slice(&empty_pattern, i);
    }

    // Phase 1: Scan RX buffer to find ANNOUNCE or ACKNOWLEDGE
    // Read directly from scatter memory using read_slice with offset 0
    let rx_offset = 'outer: loop {
        for rx_idx in 0..buffer_size {
            let mut data_buf = [[0u8; 8]; 1];
            // Read directly from physical index by using scatter's read_slice
            rx.scatter.read_slice(&mut data_buf, rx_idx);
            let value = u64::from_le_bytes(data_buf[0]);

            if value == ALIGNMENT_ANNOUNCE || value == ALIGNMENT_ACKNOWLEDGE {
                break 'outer rx_idx;
            }
        }
    };

    // Phase 2: Send ACKNOWLEDGE and wait for confirmation
    let ack_pattern = [ALIGNMENT_ACKNOWLEDGE.to_le_bytes()];
    tx.write_slice(&ack_pattern, 0);

    loop {
        let mut data_buf = [[0u8; 8]; 1];
        // Read directly from physical index
        rx.scatter.read_slice(&mut data_buf, rx_offset);
        let value = u64::from_le_bytes(data_buf[0]);

        if value == ALIGNMENT_ACKNOWLEDGE {
            break;
        }
    }

    rx_offset
}

/// Receive ringbuffer wrapping `ScatterUnit` with alignment offset.
///
/// The offset indicates where the remote transmitter's index 0 appears in the
/// local receive buffer. Reads automatically wrap at buffer boundaries.
pub struct ReceiveRingbuffer {
    /// The underlying scatter unit (RX buffer - hardware writes, CPU reads)
    scatter: ScatterUnit,
    /// The alignment offset: the index in our RX ringbuffer where the remote
    /// TX index 0 appears
    rx_offset: usize,
}

impl ReceiveRingbuffer {
    /// Create a new receive ringbuffer with the specified alignment offset.
    pub fn new(scatter: ScatterUnit, rx_offset: usize) -> Self {
        Self { scatter, rx_offset }
    }

    /// Read from the ringbuffer at a logical offset, adjusted by alignment offset.
    ///
    /// Wraps automatically if the read extends beyond the buffer boundary.
    ///
    /// # Panics
    ///
    /// Panics if `user_offset` is >= buffer size.
    pub fn read_slice(&self, dst: &mut [[u8; 8]], user_offset: usize) {
        // Panic if the user offset is out of bounds
        if user_offset >= ScatterUnit::SCATTER_MEMORY_LEN {
            panic!(
                "Offset {} out of bounds for scatter memory length {}",
                user_offset,
                ScatterUnit::SCATTER_MEMORY_LEN
            );
        }
        // Adjust the user offset by our rx_offset, wrapping around if necessary
        let mut offset: usize = self.rx_offset + user_offset;
        if offset >= ScatterUnit::SCATTER_MEMORY_LEN {
            offset -= ScatterUnit::SCATTER_MEMORY_LEN
        };

        // Read from scatter memory with wrapping if necessary
        if offset + dst.len() <= ScatterUnit::SCATTER_MEMORY_LEN {
            // No wrapping needed
            self.scatter.read_slice(dst, offset);
        } else {
            // Wrapping needed - split into two reads
            let first_part_len = ScatterUnit::SCATTER_MEMORY_LEN - offset;
            let (first, second) = dst.split_at_mut(first_part_len);
            self.scatter.read_slice(first, offset);
            self.scatter.read_slice(second, 0);
        }
    }

    /// Returns the alignment offset.
    pub fn offset(&self) -> usize {
        self.rx_offset
    }

    /// Sets the alignment offset.
    pub fn set_offset(&mut self, offset: usize) {
        self.rx_offset = offset;
    }
}

/// Transmit ringbuffer wrapping `GatherUnit`.
///
/// Writes automatically wrap at buffer boundaries.
pub struct TransmitRingbuffer {
    /// The underlying gather unit (TX buffer - CPU writes, hardware reads)
    gather: GatherUnit,
}

impl TransmitRingbuffer {
    /// Create a new transmit ringbuffer.
    pub fn new(gather: GatherUnit) -> Self {
        Self { gather }
    }

    /// Write to the ringbuffer at an offset.
    ///
    /// Wraps automatically if the write extends beyond the buffer boundary.
    ///
    /// # Panics
    ///
    /// Panics if `offset` is >= buffer size.
    pub fn write_slice(&self, src: &[[u8; 8]], offset: usize) {
        // Panic if the offset is out of bounds
        if offset >= GatherUnit::GATHER_MEMORY_LEN {
            panic!(
                "Offset {} out of bounds for gather memory length {}",
                offset,
                GatherUnit::GATHER_MEMORY_LEN
            );
        }

        // Write to gather memory with wrapping if necessary
        if offset + src.len() <= GatherUnit::GATHER_MEMORY_LEN {
            // No wrapping needed
            self.gather.write_slice(src, offset);
        } else {
            // Wrapping needed - split into two writes
            let first_part_len = GatherUnit::GATHER_MEMORY_LEN - offset;
            let (first, second) = src.split_at(first_part_len);
            self.gather.write_slice(first, offset);
            self.gather.write_slice(second, 0);
        }
    }

    /// Clears the buffer by writing zeros to all entries.
    pub fn clear(&self) {
        let zero_pattern = [[0u8; 8]; 1];
        for i in 0..GatherUnit::GATHER_MEMORY_LEN {
            self.write_slice(&zero_pattern, i);
        }
    }
}
