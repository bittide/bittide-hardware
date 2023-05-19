use core::ptr;

#[derive(Debug)]
pub struct AxiRxBuffer {
    base_addr: *mut u8,
    packet_length: *mut usize,
    status: *mut u8,
}

impl AxiRxBuffer {
    pub fn new(base_addr: *mut u8, buffer_size: usize) -> Self {
        let packet_length_addr = unsafe { base_addr.add(buffer_size) };
        let status_addr = unsafe { base_addr.add(buffer_size + 4) };

        AxiRxBuffer {
            base_addr,
            packet_length: packet_length_addr as *mut usize,
            status: status_addr as *mut u8,
        }
    }

    pub fn read_status(&self) -> u8 {
        unsafe { self.status.read_volatile()}
    }
    pub fn packet_length(&self) -> usize {
        unsafe {self.packet_length.read_volatile()}
    }
    pub fn is_packet_available(&self) -> bool {
        self.read_status() & 0b10 == 0b10
    }
    pub fn read_packet(&self, buffer: &mut [u8]) -> Option<usize> {
        if !self.is_packet_available() {
            return None;
        }

        let packet_size = self.packet_length();
        if !packet_size < buffer.len() {
            self.clear_packet();
            return None;
        }
        for i in 0..packet_size {
            unsafe {buffer[i] = self.base_addr.add(i).read_volatile()};
        }
        self.clear_packet();
        Some(packet_size)
    }
    pub fn clear_packet(&self) {
        unsafe {
            // Clear the byte count register
            ptr::write_volatile(self.packet_length as *mut u32, 0x00);
            // Clear the status register
            ptr::write_volatile(self.status, 0b11);
        }
    }
}

#[derive(Debug)]
pub struct AxiTxBuffer {
    payload_addr: *mut u8,
}

impl AxiTxBuffer {
    pub fn new(payload_addr: *mut u8) -> Self {
        AxiTxBuffer {
            payload_addr,
        }
    }

    pub fn write_packet(&self, packet: &[u8]) -> bool {
        let packet_size = packet.len();

        // Write the packet to the buffer
        for i in 0..packet_size {
            unsafe {
                *self.payload_addr = packet[i];
            }
        }

        // Initiate transmission by writing the packet size in words to the send_bytes register
        unsafe {
            ptr::write_volatile(self.payload_addr.add(4), 0);
        }

        true
    }
}
