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
pub struct AxiRx {
    base_addr: *mut u8,
    buffer_size: usize,
    packet_length: *mut usize,
    status: *mut u8,
}

impl AxiRx {
    /// Creates a new instance of `AxiRx`.
    ///
    /// # Safety
    ///
    /// - `base_addr` must be a valid mutable pointer to `usize`.
    /// - `buffer_size` must be a valid buffer size.
    ///
    pub unsafe fn new(base_addr: *mut usize, buffer_size: usize) -> Self {
        unsafe {
            let buffer_addrs = buffer_size / 4;
            let packet_length_addr = base_addr.add(buffer_addrs);
            let status_addr = base_addr.add(buffer_addrs + 1);
            AxiRx {
                base_addr: base_addr as *mut u8,
                buffer_size,
                packet_length: packet_length_addr as *mut usize,
                status: status_addr as *mut u8,
            }
        }
    }

    pub fn has_data(&self) -> bool {
        unsafe { self.status.read_volatile() != 0 }
    }

    pub fn read_status(&self) -> AxiRxStatus {
        let bits = unsafe { self.status.read_volatile() };
        AxiRxStatus {
            buffer_full: bits & 0b1 == 0b1,
            packet_complete: bits & 0b10 == 0b10,
        }
    }

    pub fn clear_status(&self) {
        unsafe {
            self.status.write_volatile(0);
        }
    }

    pub fn packet_length(&self) -> usize {
        unsafe { self.packet_length.read_volatile() }
    }

    pub fn clear_packet_register(&self) {
        unsafe {
            self.packet_length.write_volatile(0);
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
        if unsafe { self.status.read_volatile() == 0 } {
            return None;
        }

        // Get length of the incoming data
        let len = self.packet_length();
        unsafe {
            core::ptr::copy_nonoverlapping(self.base_addr, buffer.as_mut_ptr(), len);
        }
        Some(len)
    }

    pub fn clear_packet(&self) {
        unsafe {
            self.packet_length.write_volatile(0);
            self.status.write_volatile(0);
        }
    }

    pub fn get_slice(&self) -> &[u8] {
        let l = self.packet_length();
        unsafe {
            let ptr = self.base_addr as *const u8;
            let slice = core::slice::from_raw_parts_mut(ptr as *mut u8, l);
            slice
        }
    }
}

#[derive(uDebug, Copy, Clone)]
pub struct AxiTx {
    payload_addr: *mut u8,
}

impl AxiTx {
    pub fn new(payload_addr: *mut u8) -> Self {
        AxiTx { payload_addr }
    }

    pub fn send(&mut self, packet: &[u8]) {
        // Split packet into a slice of words and a slice of bytes
        let len = packet.len();
        let words = len / 4;
        let (words_slice, bytes_slice) = packet.split_at(words * 4);

        // Coerce the payload address to a u32 pointer
        let dst = self.payload_addr as *mut u32;

        // Separately write each 4-byte chunk to the payload address.
        for chunk in words_slice.chunks_exact(4) {
            // Convert each 4-byte chunk to a u32 word considering endianness
            let word = u32::from_ne_bytes(chunk.try_into().unwrap());
            // Perform the volatile write
            unsafe {
                dst.write_volatile(word);
            }
        }

        for byte in bytes_slice {
            unsafe {
                self.payload_addr.write_volatile(*byte);
            }
        }

        // Initiate transmission by writing the packet size in words to the send_bytes register
        unsafe {
            self.payload_addr.add(4).write_volatile(0);
        }
    }
}
