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
    pub fn new(base_addr: *mut usize, buffer_size: usize) -> Self {
        let packet_length_addr = unsafe { base_addr.add(buffer_size) };
        let status_addr = unsafe { base_addr.add(buffer_size + 1) };

        AxiRx {
            base_addr: base_addr as *mut u8,
            buffer_size: buffer_size * 4,
            packet_length: packet_length_addr as *mut usize,
            status: status_addr as *mut u8,
        }
    }

    pub fn read_status(&self) -> AxiRxStatus {
        let bits = unsafe { self.status.read_volatile() };
        return AxiRxStatus {
            buffer_full: bits & 0b1 == 0b1,
            packet_complete: bits & 0b10 == 0b10,
        };
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
        if unsafe { self.status.read_volatile() == 0 } {
            return None;
        }
        let packet_size = self.packet_length();
        let packet_words = (packet_size + 3) / 4;
        let mut src = self.base_addr as *mut u32;
        let mut dst = buffer.as_mut_ptr();
        for _ in 0..packet_words {
            unsafe {
                core::ptr::write_volatile(dst as *mut u32, *src);
                src = src.add(4);
                dst = dst.add(4);
            }
        }
        Some(packet_size)
    }

    pub fn clear_packet(&self) {
        unsafe {
            self.packet_length.write_volatile(0);
            self.status.write_volatile(0);
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

    pub fn send(&self, packet: &[u8]) {
        let packet_size = packet.len();
        let words = packet_size / 4;
        let remainder = words * 4;
        let word_packet: &[u32] =
            unsafe { core::slice::from_raw_parts(packet.as_ptr() as *const u32, words) };

        for i in 0..words {
            unsafe {
                // let d = packet
                core::ptr::write_volatile(self.payload_addr as *mut u32, word_packet[i]);
            }
        }
        // Write the packet to the buffer
        for i in remainder..packet_size {
            unsafe {
                core::ptr::write_volatile(self.payload_addr, packet[i]);
            }
        }

        // Initiate transmission by writing the packet size in words to the send_bytes register
        unsafe {
            self.payload_addr.add(4).write_volatile(0);
        }
    }
}
