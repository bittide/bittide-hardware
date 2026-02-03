// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::shared_devices::addressable_buffer::AddressableBuffer;
use log::warn;

/// A 4-byte aligned array wrapper for efficient memory transfers.
///
/// This type ensures that byte arrays are properly aligned for word-based
/// operations, enabling efficient bulk transfers using `copy_nonoverlapping`.
#[repr(align(4))]
#[derive(Copy, Clone)]
pub struct AlignedArray<const N: usize>([u8; N]);

impl<const N: usize> AlignedArray<N> {
    /// Creates a new aligned block initialized with zeros.
    pub const fn new() -> Self {
        Self([0u8; N])
    }

    /// Creates a new aligned block from a byte array.
    pub const fn from_bytes(bytes: [u8; N]) -> Self {
        Self(bytes)
    }

    /// Returns a reference to the underlying byte array.
    pub const fn as_bytes(&self) -> &[u8; N] {
        &self.0
    }

    /// Returns a mutable reference to the underlying byte array.
    pub fn as_bytes_mut(&mut self) -> &mut [u8; N] {
        &mut self.0
    }

    /// Returns a slice view of the byte array.
    pub fn as_slice(&self) -> &[u8] {
        &self.0
    }

    /// Returns a mutable slice view of the byte array.
    pub fn as_slice_mut(&mut self) -> &mut [u8] {
        &mut self.0
    }
}

impl<const N: usize> Default for AlignedArray<N> {
    fn default() -> Self {
        Self::new()
    }
}

impl AddressableBuffer {
    /// Returns a reference to the buffer as a contiguous byte slice.
    ///
    /// # Safety
    ///
    /// The returned slice points to volatile memory. Based on the underlying component
    /// the content may change at any time after the call to this method.
    ///
    /// This method reinterprets the buffer's word-based storage as a byte slice.
    pub unsafe fn as_slice(&self) -> &[u8] {
        core::slice::from_raw_parts(self.0 as *const u8, Self::BUFFER_LEN * 4)
    }

    /// Returns a mutable reference to the buffer as a contiguous byte slice.
    ///
    /// # Safety
    ///
    /// The returned slice points to volatile memory. Based on the underlying component
    /// the content may change at any time after the call to this method.
    ///
    /// This method reinterprets the buffer's word-based storage as a byte slice.
    pub unsafe fn as_slice_mut(&mut self) -> &mut [u8] {
        core::slice::from_raw_parts_mut(self.0, Self::BUFFER_LEN * 4)
    }

    /// Clears the contents of the entire buffer by setting all bytes to zero.
    pub fn clear(&self) {
        for i in 0..Self::BUFFER_LEN {
            unsafe { self.set_buffer_unchecked(i, [0u8; 4]) };
        }
    }

    /// Write a byte slice to the buffer memory.
    ///
    /// This method supports both aligned and unaligned writes. For aligned writes
    /// (offset and length are multiples of 4), it uses efficient word-level operations.
    /// For unaligned writes, it performs read-modify-write on affected words.
    ///
    /// # Panics
    ///
    /// Panics if the slice would extend beyond the buffer bounds.
    pub fn write_slice(&self, src: &[u8], offset: usize) {
        assert!(
            offset + src.len() <= Self::BUFFER_LEN * 4,
            "Slice extends beyond buffer bounds"
        );

        // Fast path: aligned access
        if offset & 3 == 0 && src.len() & 3 == 0 {
            let src_ptr = src.as_ptr();
            let dst_ptr = unsafe { self.0.add(offset) };
            unsafe {
                core::ptr::copy_nonoverlapping(src_ptr, dst_ptr, src.len());
            }
            return;
        }

        // Slow path: unaligned access with read-modify-write
        warn!(
            "Unaligned write detected: offset={} (aligned: {}), len={} (aligned: {}). This will be slow.",
            offset,
            offset & 3 == 0,
            src.len(),
            src.len() & 3 == 0
        );
        for (i, &byte) in src.iter().enumerate() {
            let byte_offset = offset + i;
            let word_index = byte_offset / 4;
            let byte_in_word = byte_offset % 4;

            let mut word = unsafe { self.buffer_unchecked(word_index) };
            word[byte_in_word] = byte;
            unsafe { self.set_buffer_unchecked(word_index, word) };
        }
    }

    /// Read a byte slice from the buffer memory.
    ///
    /// This method supports both aligned and unaligned reads. For aligned reads
    /// (offset and length are multiples of 4), it uses efficient word-level operations.
    /// For unaligned reads, it reads individual bytes from the affected words.
    ///
    /// # Panics
    ///
    /// Panics if the slice would extend beyond the buffer bounds.
    pub fn read_slice(&self, dst: &mut [u8], offset: usize) {
        assert!(
            offset + dst.len() <= Self::BUFFER_LEN * 4,
            "Slice extends beyond buffer bounds"
        );

        // Fast path: aligned access
        if offset & 3 == 0 && dst.len() & 3 == 0 {
            let src_ptr = unsafe { self.0.add(offset) };
            let dst_ptr = dst.as_mut_ptr();
            unsafe {
                core::ptr::copy_nonoverlapping(src_ptr, dst_ptr, dst.len());
            }
            return;
        }

        // Slow path: unaligned access
        warn!(
            "Unaligned read detected: offset={} (aligned: {}), len={} (aligned: {}). This will be slow.",
            offset,
            offset & 3 == 0,
            dst.len(),
            dst.len() & 3 == 0
        );
        for (i, byte) in dst.iter_mut().enumerate() {
            let byte_offset = offset + i;
            let word_index = byte_offset / 4;
            let byte_in_word = byte_offset % 4;

            let word = unsafe { self.buffer_unchecked(word_index) };
            *byte = word[byte_in_word];
        }
    }
}
