use log::trace;

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::hals::ringbuffer_test::devices::{ReceiveRingbuffer, TransmitRingbuffer};

pub type AlignedReceiveBuffer = crate::manual_additions::ringbuffer::AlignedReceiveBuffer<
    ReceiveRingbuffer,
    TransmitRingbuffer,
>;

impl TransmitRingbuffer {
    /// Write a slice to the transmit buffer.
    ///
    /// # Panics
    ///
    /// The source memory size must be smaller or equal to the memory size of
    /// the `TransmitRingbuffer` memory.
    pub fn write_slice(&self, src: &[[u8; 8]], offset: usize) {
        // trace!("write_slice called with offset {} and src {:02x?}", offset, src);
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
        let src_ptr = src.as_ptr();
        let dst_ptr = self.0.add(offset) as *mut [u8; 8];
        // trace!("Writing slice to transmit buffer at offset {}: {:02x?}", offset, src);
        unsafe {
            core::ptr::copy_nonoverlapping(src_ptr, dst_ptr, src.len());
        }
        // trace!("Done");
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
            self.write_slice(src, offset);
        } else {
            // Wrapping needed - split into two writes
            let first_part_len = Self::DATA_LEN - offset;
            let (first, second) = src.split_at(first_part_len);
            self.write_slice(first, offset);
            self.write_slice(second, 0);
        }
    }

    /// Clear the entire transmit buffer by writing zeros to all entries.
    pub fn clear(&self) {
        for i in 0..Self::DATA_LEN {
            unsafe {
                self.set_data_unchecked(i, [0u8; 8]);
            }
        }
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
            "read_slice called with offset {} and dst len {}",
            offset,
            dst.len()
        );
        assert!(dst.len() + offset <= Self::DATA_LEN);
        unsafe {
            self.read_slice_unchecked(dst, offset);
        }
        trace!("Done");
    }

    /// Reads a slice from the receive buffer without checking bounds. The caller must
    /// ensure that `dst.len() + offset` does not exceed the buffer size.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it can cause out-of-bounds memory access if the caller
    /// does not ensure that `dst.len() + offset` is within the bounds of the receive buffer.
    pub unsafe fn read_slice_unchecked(&self, dst: &mut [[u8; 8]], offset: usize) {
        let dst_ptr = dst.as_mut_ptr();
        let src_ptr = self.0 as *const [u8; 8];
        unsafe {
            core::ptr::copy_nonoverlapping(src_ptr.add(offset), dst_ptr, dst.len());
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
            self.read_slice(dst, offset);
        } else {
            // Wrapping needed - split into two reads
            let first_part_len = Self::DATA_LEN - offset;
            let (first, second) = dst.split_at_mut(first_part_len);
            self.read_slice(first, offset);
            self.read_slice(second, 0);
        }
    }
}
