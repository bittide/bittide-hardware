pub struct Uart {
    payload_addr: *mut u8,
    flags_addr: *const u8,
}

impl Uart {
    pub const unsafe fn new(base_addr: *mut u8) -> Uart {
        Uart {
            payload_addr: base_addr.cast(),
            flags_addr: base_addr.cast::<u8>().cast_const().add(4),
        }
    }
    pub fn read_status(&mut self) -> u8 {
        unsafe {self.flags_addr.read_volatile()}
    }

    pub fn receive(&mut self) -> u8 {
        loop {
            if let Some(val) = self.try_receive() {
                return val;
            }
        }
    }

    pub fn try_receive(&mut self) -> Option<u8> {
        if self.has_rec_data() {
            unsafe {
                let data:u8 = self.payload_addr.read_volatile();
                Some(data)
            }
        } else {
            None
        }
    }

    pub fn send(&mut self, data: u8) {
        loop {
            if let Ok(()) = self.try_send(data) {
                return;
            }
        }
    }

    pub fn try_send(&mut self, data: u8) -> Result<(), ()> {
        if self.can_send() {
            unsafe {
                self.payload_addr
                    .write_volatile(data);
                Ok(())
            }
        } else {
            Err(())
        }
    }

    pub fn has_rec_data(&mut self) -> bool {
        let mask = 1;
        let flags = self.read_status();
        let rx_empty = flags & mask;
        rx_empty != mask
    }

    pub fn can_send(&mut self) -> bool {
        let mask = 8;
        let flags = self.read_status();
        let tx_full = flags & mask;
        tx_full != mask
    }
}

impl core::fmt::Write for Uart {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        for b in s.bytes() {
            self.send(b);
        }
        Ok(())
    }
}
