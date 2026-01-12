// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Aligned Ringbuffer for Soft UGN Demo MU
//!
//! MU-specific wrappers around scatter/gather units providing the same interface
//! as the generic aligned_ringbuffer but using soft_ugn_demo_mu device types.

use crate::hals::soft_ugn_demo_mu::devices::{GatherUnit, ScatterUnit};

/// Alignment protocol marker values
const ALIGNMENT_EMPTY: u64 = 0;
const ALIGNMENT_ANNOUNCE: u64 = 0xBADC0FFEE;
const ALIGNMENT_ACKNOWLEDGE: u64 = 0xDEADABBA;

/// Wrapper around GatherUnit for transmitting data
pub struct TransmitRingbuffer<'a> {
    gather: &'a GatherUnit,
}

impl<'a> TransmitRingbuffer<'a> {
    /// Create a new transmit ringbuffer
    pub fn new(gather: &'a GatherUnit) -> Self {
        Self { gather }
    }

    /// Write a slice to the ringbuffer at a logical offset.
    ///
    /// Wraps automatically if the write extends beyond the buffer boundary.
    ///
    /// # Panics
    ///
    /// Panics if `user_offset` is >= buffer size.
    pub fn write_slice(&self, src: &[[u8; 8]], user_offset: usize) {
        let buffer_size = GatherUnit::GATHER_MEMORY_LEN;
        assert!(user_offset < buffer_size, "Offset out of bounds");

        for (i, &value) in src.iter().enumerate() {
            let physical_idx = (user_offset + i) % buffer_size;
            let _ = self.gather.set_gather_memory(physical_idx, value);
        }
    }

    /// Clear the entire buffer by writing zeros
    pub fn clear(&self) {
        let zero = [0u8; 8];
        for i in 0..GatherUnit::GATHER_MEMORY_LEN {
            let _ = self.gather.set_gather_memory(i, zero);
        }
    }

    /// Get the underlying gather unit
    pub fn gather(&self) -> &GatherUnit {
        self.gather
    }
}

/// Wrapper around ScatterUnit for receiving data with alignment
pub struct ReceiveRingbuffer<'a> {
    scatter: &'a ScatterUnit,
    rx_offset: usize,
}

impl<'a> ReceiveRingbuffer<'a> {
    /// Create a new receive ringbuffer with a given alignment offset
    pub fn new(scatter: &'a ScatterUnit, rx_offset: usize) -> Self {
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
        let buffer_size = ScatterUnit::SCATTER_MEMORY_LEN;
        assert!(user_offset < buffer_size, "Offset out of bounds");

        for (i, val) in dst.iter_mut().enumerate() {
            let logical_idx = (user_offset + i) % buffer_size;
            let physical_idx = (self.rx_offset + logical_idx) % buffer_size;
            *val = self.scatter.scatter_memory(physical_idx).unwrap_or([0; 8]);
        }
    }

    /// Returns the alignment offset.
    pub fn offset(&self) -> usize {
        self.rx_offset
    }

    /// Sets the alignment offset.
    pub fn set_offset(&mut self, offset: usize) {
        self.rx_offset = offset % ScatterUnit::SCATTER_MEMORY_LEN;
    }

    /// Get the underlying scatter unit
    pub fn scatter(&self) -> &ScatterUnit {
        self.scatter
    }
}

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
pub fn find_alignment_offset<'a>(tx: &TransmitRingbuffer<'a>, rx: &ReceiveRingbuffer<'a>) -> usize {
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
            // Read directly from physical index
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
