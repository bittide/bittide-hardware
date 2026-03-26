// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::shared_devices::addressable_buffer::AddressableBuffer;

impl AddressableBuffer {
    /// Returns a reference to the data as a contiguous byte slice.
    ///
    /// # Safety
    ///
    /// The returned slice points to volatile memory. Based on the underlying component
    /// the content may change at any time after the call to this method.
    ///
    /// This method reinterprets the word-based storage as a byte slice.
    pub unsafe fn as_slice(&self) -> &[u8] {
        core::slice::from_raw_parts(self.0 as *const u8, Self::DATA_LEN * 4)
    }

    /// Returns a mutable reference to the data as a contiguous byte slice.
    ///
    /// # Safety
    ///
    /// The returned slice points to volatile memory. Based on the underlying component
    /// the content may change at any time after the call to this method.
    ///
    /// This method reinterprets the word-based storage as a byte slice.
    pub unsafe fn as_slice_mut(&mut self) -> &mut [u8] {
        core::slice::from_raw_parts_mut(self.0, Self::DATA_LEN * 4)
    }

    /// Clears the contents of the entire data by setting all bytes to zero.
    pub fn clear(&self) {
        for i in 0..Self::DATA_LEN {
            unsafe { self.set_data_unchecked(i, [0u8; 4]) };
        }
    }

    /// Write a byte slice to the data memory using `copy_nonoverlapping`.
    ///
    /// # Panics
    ///
    /// Panics if the slice would extend beyond the data bounds.
    pub fn write_slice(&self, src: &[u8], offset: usize) {
        assert!(
            offset + src.len() <= Self::DATA_LEN * 4,
            "Slice extends beyond data bounds"
        );
        unsafe {
            self.write_slice_unchecked(src, offset);
        }
    }

    /// Write a byte slice to the data memory without checking bounds using `copy_nonoverlapping`.
    ///
    /// # Safety
    ///
    /// The caller must ensure that the slice does not extend beyond the data bounds.
    pub unsafe fn write_slice_unchecked(&self, src: &[u8], offset: usize) {
        core::ptr::copy_nonoverlapping(src.as_ptr(), self.0.add(offset), src.len());
    }

    /// Read a byte slice from the data memory using `copy_nonoverlapping`.
    ///
    /// # Panics
    ///
    /// Panics if the slice would extend beyond the data bounds.
    pub fn read_slice(&self, dst: &mut [u8], offset: usize) {
        assert!(
            offset + dst.len() <= Self::DATA_LEN * 4,
            "Slice extends beyond data bounds"
        );
        unsafe {
            self.read_slice_unchecked(dst, offset);
        }
    }

    /// Read a byte slice from the data memory using `copy_nonoverlapping`.
    ///
    /// # Safety
    ///
    /// The caller must ensure that the slice does not extend beyond the data bounds.
    pub unsafe fn read_slice_unchecked(&self, dst: &mut [u8], offset: usize) {
        core::ptr::copy_nonoverlapping(self.0.add(offset), dst.as_mut_ptr(), dst.len());
    }
}
