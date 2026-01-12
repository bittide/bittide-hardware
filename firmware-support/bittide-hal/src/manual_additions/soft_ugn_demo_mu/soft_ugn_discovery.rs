// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Soft UGN Discovery Protocol
//!
//! Implements the soft UGN (Universal Global Number) discovery procedure using
//! scatter/gather units for neighbor detection and UGN calculation.
//!
//! The UGN represents the relationship between local and remote counters:
//! UGN = local_counter - remote_counter
//!
//! This module builds on the ringbuffer alignment protocol to exchange counters
//! and calculate UGNs without dedicated hardware components.

use crate::{
    hals::soft_ugn_demo_mu::devices::{GatherUnit, ScatterUnit},
    manual_additions::soft_ugn_demo_mu::aligned_ringbuffer::{
        ReceiveRingbuffer, TransmitRingbuffer,
    },
};

/// UGN discovery protocol marker values
const UGN_REQUEST: u64 = 0xC0FFEE01;
const UGN_RESPONSE: u64 = 0xC0FFEE02;

/// Represents a discovered UGN for a specific link
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ugn {
    /// Local counter value when the remote counter was received
    pub local_counter: u64,
    /// Remote counter value received from the neighbor
    pub remote_counter: u64,
}

impl Ugn {
    /// Create a new UGN from local and remote counter values
    pub fn new(local_counter: u64, remote_counter: u64) -> Self {
        Self {
            local_counter,
            remote_counter,
        }
    }

    /// Calculate the UGN value (local - remote)
    pub fn value(&self) -> i64 {
        self.local_counter.wrapping_sub(self.remote_counter) as i64
    }
}

/// Perform soft UGN discovery on a single link
///
/// This function assumes the ringbuffers are already aligned.
///
/// # Protocol
///
/// 1. Send UGN_REQUEST with our local counter to TX buffer
/// 2. Wait for UGN_REQUEST or UGN_RESPONSE from remote node
/// 3. Extract remote counter from received message
/// 4. Send UGN_RESPONSE with our local counter
/// 5. Wait for UGN_RESPONSE confirmation
///
/// Returns the discovered UGN.
///
/// # Arguments
///
/// * `tx` - Transmit ringbuffer for this link
/// * `rx` - Receive ringbuffer for this link (must be aligned)
/// * `local_counter` - Current local counter value
///
/// # Note
///
/// Both nodes must run this procedure simultaneously for it to complete.
/// The function will loop indefinitely until successful discovery.
pub fn discover_ugn<'a>(
    tx: &TransmitRingbuffer<'a>,
    rx: &ReceiveRingbuffer<'a>,
    local_counter: u64,
) -> Ugn {
    let buffer_size = ScatterUnit::SCATTER_MEMORY_LEN;

    // Phase 1: Send our local counter with UGN_REQUEST marker
    let request_data = [UGN_REQUEST.to_le_bytes(), local_counter.to_le_bytes()];
    tx.write_slice(&request_data, 0);

    // Clear the rest of TX buffer
    let empty: [[u8; 8]; 1] = [[0; 8]];
    for i in 2..buffer_size {
        tx.write_slice(&empty, i);
    }

    // Phase 2: Wait for UGN_REQUEST or UGN_RESPONSE from remote
    let (remote_counter, response_received) = 'outer: loop {
        for rx_idx in 0..buffer_size {
            let mut data_buf = [[0u8; 8]; 2];
            rx.read_slice(&mut data_buf, rx_idx);

            let marker = u64::from_le_bytes(data_buf[0]);
            let counter = u64::from_le_bytes(data_buf[1]);

            if marker == UGN_REQUEST || marker == UGN_RESPONSE {
                break 'outer (counter, marker == UGN_RESPONSE);
            }
        }
    };

    // Phase 3: Send UGN_RESPONSE with our local counter
    let response_data = [UGN_RESPONSE.to_le_bytes(), local_counter.to_le_bytes()];
    tx.write_slice(&response_data, 0);

    // Phase 4: Wait for UGN_RESPONSE if we haven't received it yet
    if !response_received {
        loop {
            let mut data_buf = [[0u8; 8]; 1];
            rx.read_slice(&mut data_buf, 0);
            let marker = u64::from_le_bytes(data_buf[0]);

            if marker == UGN_RESPONSE {
                break;
            }
        }
    }

    Ugn::new(local_counter, remote_counter)
}

/// Helper to read counter from scatter unit at a specific logical index
pub fn read_counter_at(scatter: &ScatterUnit, rx_offset: usize, logical_idx: usize) -> u64 {
    let physical_idx = (rx_offset + logical_idx) % ScatterUnit::SCATTER_MEMORY_LEN;
    let data = scatter.scatter_memory(physical_idx).unwrap_or([0; 8]);
    u64::from_le_bytes(data)
}

/// Helper to write counter to gather unit at a specific logical index
pub fn write_counter_at(gather: &GatherUnit, logical_idx: usize, value: u64) {
    let _ = gather.set_gather_memory(logical_idx, value.to_le_bytes());
}
