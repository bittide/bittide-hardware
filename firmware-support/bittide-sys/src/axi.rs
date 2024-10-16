// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use ufmt::derive::uDebug;
pub mod self_test;

pub struct AxiRxStatus {
    pub buffer_full: bool,
    pub packet_complete: bool,
}

#[derive(uDebug, Copy, Clone)]
pub struct AxiRx<const BUF_SIZE: usize> {
    base_addr: *const u8,
}

impl<const BUF_SIZE: usize> AxiRx<BUF_SIZE> {
    const PACKET_LENGTH_OFFSET: usize = BUF_SIZE;
    const STATUS_OFFSET: usize = BUF_SIZE + 4;

    /// Creates a new instance of `AxiRx`.
    ///
    /// # Safety
    ///

    /// - `base_addr` must post to a memory mapped AXI Rx peripheral.
    /// - `BUF_SIZE` must be a valid buffer size.
    ///
    ///
    pub unsafe fn new(addr: *const ()) -> Self {
        AxiRx {
            base_addr: addr as *const u8,
        }
    }

    // Returns true if there is a packet in the buffer or the buffer is full.
    pub fn has_data(&self) -> bool {
        self.read_status_raw() != 0
    }

    // Reads the raw bits of the status register.
    pub fn read_status_raw(&self) -> u8 {
        // If the instantiation of the AxiRx struct is correct,
        // the status register should be located at the base address + (4 * (BUF_SIZE + 1))
        unsafe { self.base_addr.add(Self::STATUS_OFFSET).read_volatile() }
    }

    // Returns a struct with the status of the buffer.
    pub fn read_status(&self) -> AxiRxStatus {
        let bits = self.read_status_raw();
        AxiRxStatus {
            buffer_full: bits & 0b1 == 0b1,
            packet_complete: bits & 0b10 == 0b10,
        }
    }

    // Clears the bits in the status register.
    pub fn clear_status(&self) {
        unsafe {
            self.base_addr
                .add(Self::STATUS_OFFSET)
                .cast_mut()
                .write_volatile(0);
        }
    }

    pub fn packet_length(&self) -> usize {
        unsafe {
            self.base_addr
                .add(Self::PACKET_LENGTH_OFFSET)
                .cast::<usize>()
                .read_volatile()
        }
    }

    pub fn clear_packet_register(&self) {
        unsafe {
            self.base_addr
                .add(Self::PACKET_LENGTH_OFFSET)
                .cast_mut()
                .write_volatile(0);
        }
    }
    pub fn receive_with_timeout(&self, buffer: &mut [u8], attempts: usize) -> Option<usize> {
        for _ in 0..attempts {
            if let Some(s) = self.try_receive(buffer) {
                return Some(s);
            }
        }
        None
    }
    pub fn receive(&self, buffer: &mut [u8]) -> usize {
        loop {
            if let Some(s) = self.try_receive(buffer) {
                return s;
            }
        }
    }

    pub fn try_receive(&self, buffer: &mut [u8]) -> Option<usize> {
        // Check if there is data to receive, this means either the buffer is full or a packet is complete
        // We can check by using the utility function read_status
        if self.read_status_raw() == 0 {
            return None;
        }

        // Get length of the incoming data
        let len = self.packet_length();

        debug_assert!(len <= buffer.len(), "Buffer too small to receive packet");

        unsafe {
            core::ptr::copy_nonoverlapping(self.base_addr, buffer.as_mut_ptr(), len);
        }
        Some(len)
    }

    pub fn clear_packet(&self) {
        self.clear_packet_register();
        self.clear_status();
    }

    pub fn get_slice(&self) -> &[u8] {
        let l = self.packet_length();
        unsafe {
            let slice = core::slice::from_raw_parts_mut(self.base_addr as *mut u8, l);
            slice
        }
    }
}

#[derive(uDebug, Copy, Clone)]
pub struct AxiTx {
    base_addr: *mut u8,
}

impl AxiTx {
    /// Creates a new instance of `AxiTx`.
    /// # Safety
    /// - `base_addr` Must be the base address of the Axi Tx peripheral.
    pub unsafe fn new(base_addr: *const ()) -> Self {
        AxiTx {
            base_addr: base_addr as *mut u8,
        }
    }

    pub fn send(&mut self, packet: &[u8]) {
        // Deal with unaligned packets by splitting them into 3 parts
        // The use of align_to is safe because the binary representation of 4 bytes is the same as 1 word
        let (bytes_slice_prefix, words_slice, bytes_slice_suffix) = unsafe { packet.align_to() };

        for byte in bytes_slice_prefix {
            unsafe {
                self.base_addr.write_volatile(*byte);
            }
        }
        // Coerce the payload address to a u32 pointer
        let dst = self.base_addr as *mut u32;
        for word in words_slice {
            unsafe {
                dst.write_volatile(*word);
            }
        }
        for byte in bytes_slice_suffix {
            unsafe {
                self.base_addr.write_volatile(*byte);
            }
        }

        // Initiate transmission by writing the packet size in words to the send_bytes register
        unsafe {
            self.base_addr.add(4).write_volatile(0);
        }
    }
}
