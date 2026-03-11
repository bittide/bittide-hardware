// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::hals::soft_ugn_demo_mu::devices::{ReceiveRingbuffer, TransmitRingbuffer};
use log::{debug, trace, warn};

/// Alignment protocol marker values
const ALIGNMENT_EMPTY: u64 = 0;
const ALIGNMENT_ANNOUNCE: u64 = 0xBADC0FFEE;
const ALIGNMENT_ACKNOWLEDGE: u64 = 0xDEADABBA;

impl TransmitRingbuffer {
    /// Write a slice to the transmit buffer.
    ///
    /// # Panics
    ///
    /// The source memory size must be smaller or equal to the memory size of
    /// the `TransmitRingbuffer` memory.
    pub fn write_slice(&self, src: &[[u8; 8]], offset: usize) {
        trace!(
            "TransmitRingbuffer::write_slice: len={}, offset={}",
            src.len(),
            offset
        );
        assert!(src.len() + offset <= Self::DATA_LEN);
        unsafe {
            self.write_slice_unchecked(src, offset);
        }
    }

    /// Write a slice to the transmit buffer without checking bounds. The caller must
    /// ensure that `src.len() + offset` does not exceed the buffer size.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it can cause out-of-bounds memory access if the caller
    /// does not ensure that `src.len() + offset` is within the bounds of the transmit buffer.
    pub unsafe fn write_slice_unchecked(&self, src: &[[u8; 8]], offset: usize) {
        trace!("TransmitRingbuffer::write_slice_unchecked: about to write {} bytes starting at offset {}", src.len(), offset);
        let mut off = offset;
        for &val in src {
            unsafe {
                self.set_data_unchecked(off, val);
            }
            off += 1;
        }
    }

    /// Write a slice to the transmit buffer with automatic wrapping. If we write more bytes
    /// than the buffer size, we wrap back to the beginning of the buffer.
    ///
    /// # Panics
    ///
    /// This function will panic if `src.len()` is greater than the buffer size.
    pub fn write_slice_with_wrap(&self, src: &[[u8; 8]], offset: usize) {
        assert!(src.len() <= Self::DATA_LEN);
        unsafe {
            self.write_slice_with_wrap_unchecked(src, offset);
        }
    }

    /// Write a slice to the transmit buffer with automatic wrapping. If we write more bytes
    /// than the buffer size, we wrap back to the beginning of the buffer.
    ///
    /// # Safety
    /// This function is unsafe because it can cause out-of-bounds memory access if the caller
    /// does not ensure that `src.len()` is smaller or equal to the buffer size.
    pub unsafe fn write_slice_with_wrap_unchecked(&self, src: &[[u8; 8]], offset: usize) {
        if src.len() + offset <= Self::DATA_LEN {
            // No wrapping needed
            trace!(
                "TransmitRingbuffer::write_slice_with_wrap: no wrap needed, len={}, offset={}",
                src.len(),
                offset
            );
            self.write_slice(src, offset);
        } else {
            // Wrapping needed - split into two writes
            let first_part_len = Self::DATA_LEN - offset;
            let (first, second) = src.split_at(first_part_len);
            debug!("TransmitRingbuffer::write_slice_with_wrap: wrapping at offset={}, first_len={}, second_len={}", offset, first.len(), second.len());
            self.write_slice(first, offset);
            self.write_slice(second, 0);
        }
    }

    /// Clear the entire transmit buffer by writing zeros to all entries.
    pub fn clear(&self) {
        debug!(
            "TransmitRingbuffer::clear: about to clear {} entries",
            Self::DATA_LEN
        );
        for i in 0..Self::DATA_LEN {
            unsafe {
                self.set_data_unchecked(i, [0u8; 8]);
            }
        }
        debug!("TransmitRingbuffer::clear: completed");
    }
}

impl ReceiveRingbuffer {
    /// Read a slice from the receive buffer.
    ///
    /// # Panics
    ///
    /// The destination memory size must be smaller or equal to the memory size
    ///  of the `ReceiveRingbuffer`.
    pub fn read_slice(&self, dst: &mut [[u8; 8]], offset: usize) {
        trace!(
            "ReceiveRingbuffer::read_slice: len={}, offset={}",
            dst.len(),
            offset
        );
        assert!(dst.len() + offset <= Self::DATA_LEN);
        unsafe {
            self.read_slice_unchecked(dst, offset);
        }
    }

    /// Reads a slice from the receive buffer without checking bounds. The caller must
    /// ensure that `dst.len() + offset` does not exceed the buffer size.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it can cause out-of-bounds memory access if the caller
    /// does not ensure that `dst.len() + offset` is within the bounds of the receive buffer.
    pub unsafe fn read_slice_unchecked(&self, dst: &mut [[u8; 8]], offset: usize) {
        trace!(
            "ReceiveRingbuffer::read_slice_unchecked: about to read {} bytes starting at offset {}",
            dst.len(),
            offset
        );
        let mut off = offset;
        for val in dst {
            unsafe {
                *val = self.data_unchecked(off);
            }
            off += 1;
        }
    }

    /// Reads a slice from the receive buffer with automatic wrapping. If we read more bytes
    /// than the buffer size, we wrap back to the beginning of the buffer.
    ///
    /// # Panics
    ///
    /// This function will panic if `dst.len()` is greater than the buffer size.
    pub fn read_slice_with_wrap(&self, dst: &mut [[u8; 8]], offset: usize) {
        assert!(dst.len() <= Self::DATA_LEN);
        unsafe {
            self.read_slice_with_wrap_unchecked(dst, offset);
        }
    }
    /// Reads a slice from the receive buffer with automatic wrapping. If we read more bytes
    /// than the buffer size, we wrap back to the beginning of the buffer.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it can cause out-of-bounds memory access if the caller
    /// does not ensure that `dst.len()` is smaller or equal to the buffer size.
    pub unsafe fn read_slice_with_wrap_unchecked(&self, dst: &mut [[u8; 8]], offset: usize) {
        if dst.len() + offset <= Self::DATA_LEN {
            // No wrapping needed
            trace!(
                "ReceiveRingbuffer::read_slice_with_wrap: no wrap needed, len={}, offset={}",
                dst.len(),
                offset
            );
            self.read_slice(dst, offset);
        } else {
            // Wrapping needed - split into two reads
            let first_part_len = Self::DATA_LEN - offset;
            let (first, second) = dst.split_at_mut(first_part_len);
            debug!("ReceiveRingbuffer::read_slice_with_wrap: wrapping at offset={}, first_len={}, second_len={}", offset, first.len(), second.len());
            self.read_slice(first, offset);
            self.read_slice(second, 0);
        }
    }
}

pub struct AlignedReceiveBuffer {
    pub rx: ReceiveRingbuffer,
    rx_alignment_offset: Option<usize>,
    tx_reference: usize, // Allows us to verify the tx_buffer we are aligned to.
}
impl AlignedReceiveBuffer {
    /// Perform the alignment discovery protocol and return a new `AlignedReceiveBuffer`
    /// with the discovered RX alignment offset.
    ///
    /// # Panics
    ///
    /// This function will panic if the transmit and receive buffers do not have the same size,
    /// since that is a requirement for the alignment protocol to work.
    pub fn new(rx: ReceiveRingbuffer) -> Self {
        debug!("AlignedReceiveBuffer::new: creating new buffer");
        Self {
            rx,
            rx_alignment_offset: None,
            tx_reference: 0,
        }
    }

    /// Returns true if the alignment offset has been discovered and the buffer is aligned.
    pub fn is_aligned(&self) -> bool {
        self.rx_alignment_offset.is_some()
    }

    /// Returns the discovered alignment offset, or None if the offset has not been discovered yet.
    pub fn get_alignment_offset(&self) -> Option<usize> {
        self.rx_alignment_offset
    }

    /// Performs the alignment discovery protocol. After this function completes, the `rx_alignment_offset`
    /// field will be set with the discovered offset, and the RX buffer will be aligned to the neighbor's TX buffer.
    pub fn align(&mut self, tx: &TransmitRingbuffer) {
        debug!("AlignedReceiveBuffer::align: starting alignment protocol");
        // Initialize TX buffer: write ANNOUNCE at index 0, clear the rest
        debug!("AlignedReceiveBuffer::align: about to write ANNOUNCE pattern to TX");
        let announce_pattern = [ALIGNMENT_ANNOUNCE.to_le_bytes()];
        tx.write_slice(&announce_pattern, 0);
        debug!("AlignedReceiveBuffer::align: sent ANNOUNCE pattern");

        debug!("AlignedReceiveBuffer::align: about to clear rest of TX buffer");
        let empty_pattern: [[u8; 8]; 1] = [ALIGNMENT_EMPTY.to_le_bytes()];
        for i in 1..TransmitRingbuffer::DATA_LEN {
            tx.write_slice(&empty_pattern, i);
        }
        debug!("AlignedReceiveBuffer::align: cleared rest of TX buffer");

        debug!("AlignedReceiveBuffer::align: cleared rest of TX buffer");

        // Phase 1: Scan RX buffer to find ANNOUNCE or ACKNOWLEDGE
        // Read directly from scatter memory using read_slice with offset 0
        debug!("AlignedReceiveBuffer::align: phase 1 - scanning RX buffer for alignment marker");
        let rx_offset = 'outer: loop {
            for rx_idx in 0..ReceiveRingbuffer::DATA_LEN {
                let mut data_buf = [[0u8; 8]; 1];
                // Read directly from physical index by using scatter's read_slice
                trace!(
                    "AlignedReceiveBuffer::align: about to read RX at index {}",
                    rx_idx
                );
                self.rx.read_slice(&mut data_buf, rx_idx);
                let value = u64::from_le_bytes(data_buf[0]);

                if value == ALIGNMENT_ANNOUNCE || value == ALIGNMENT_ACKNOWLEDGE {
                    debug!("AlignedReceiveBuffer::align: found alignment marker at rx_idx={}, value=0x{:x}", rx_idx, value);
                    break 'outer rx_idx;
                }
            }
        };
        debug!(
            "AlignedReceiveBuffer::align: discovered rx_offset={}",
            rx_offset
        );

        // Phase 2: Send ACKNOWLEDGE and wait for confirmation
        debug!("AlignedReceiveBuffer::align: phase 2 - about to send ACKNOWLEDGE");
        let ack_pattern = [ALIGNMENT_ACKNOWLEDGE.to_le_bytes()];
        tx.write_slice(&ack_pattern, 0);
        debug!("AlignedReceiveBuffer::align: sent ACKNOWLEDGE, waiting for confirmation");

        loop {
            let mut data_buf = [[0u8; 8]; 1];
            // Read directly from physical index
            trace!(
                "AlignedReceiveBuffer::align: polling RX at offset {} for ACKNOWLEDGE confirmation",
                rx_offset
            );
            self.rx.read_slice(&mut data_buf, rx_offset);
            let value = u64::from_le_bytes(data_buf[0]);

            if value == ALIGNMENT_ACKNOWLEDGE {
                debug!("AlignedReceiveBuffer::align: received ACKNOWLEDGE confirmation");
                break;
            }
        }
        self.rx_alignment_offset = Some(rx_offset);
        self.tx_reference = tx.0 as *const _ as usize;
        debug!(
            "AlignedReceiveBuffer::align: alignment complete, offset={}, tx_ref=0x{:x}",
            rx_offset, self.tx_reference
        );
    }

    /// Unsets the discovered alignment offset.
    pub fn clear_alignment(&mut self) {
        debug!("AlignedReceiveBuffer::clear_alignment: clearing alignment");
        self.rx_alignment_offset = None;
        self.tx_reference = 0;
    }

    /// Returns true if the RX buffer is aligned to the provided TX buffer.
    pub fn verify_aligned_to(&self, tx: &TransmitRingbuffer) -> bool {
        let tx_addr = tx.0 as *const _ as usize;
        let result = self.is_aligned() && self.tx_reference == tx_addr;
        if !result {
            warn!("AlignedReceiveBuffer::verify_aligned_to: verification failed, aligned={}, tx_ref=0x{:x}, tx_addr=0x{:x}", self.is_aligned(), self.tx_reference, tx_addr);
        }
        result
    }

    /// Returns the reference address of the TX buffer that this RX buffer is aligned to.
    pub fn get_alignment_reference(&self) -> usize {
        self.tx_reference
    }
    /// Read a slice from the receive buffer using the discovered alignment offset.
    /// After aligning the buffer pair, the user can use this function to read from the receive
    /// buffer without needing to worry about the physical alignment offset.
    ///
    /// # Panics
    ///
    /// This function will panic if `dst.len()` is greater than the buffer size.
    /// This function will also panic if the caller tries to read beyond the end of the buffer.
    /// This function will also panic if `align()` has not been called yet to discover the alignment offset.
    pub fn read_slice(&self, dst: &mut [[u8; 8]], offset: usize) {
        trace!(
            "AlignedReceiveBuffer::read_slice: len={}, offset={}",
            dst.len(),
            offset
        );
        assert!(dst.len() + offset <= ReceiveRingbuffer::DATA_LEN);
        let rx_offset = self
            .rx_alignment_offset
            .expect("Alignment offset not discovered yet. Call align() first.");
        let mut aligned_offset = offset + rx_offset;
        if aligned_offset >= ReceiveRingbuffer::DATA_LEN {
            aligned_offset -= ReceiveRingbuffer::DATA_LEN;
        }
        trace!(
            "AlignedReceiveBuffer::read_slice: using aligned_offset={}",
            aligned_offset
        );
        trace!("AlignedReceiveBuffer::read_slice: about to call read_slice_with_wrap");
        self.rx.read_slice_with_wrap(dst, aligned_offset)
    }
}
