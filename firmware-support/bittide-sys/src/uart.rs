// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

/// `Uart` is a structure representing a universal asynchronous receiver-transmitter.
pub struct Uart {
    /// `payload_addr` is a mutable pointer to the address of the data payload.
    payload_addr: *mut u8,
    /// `flags_addr` is a constant pointer to the address of the flags.
    flags_addr: *const u8,
}

pub struct UartStatus {
    rx_empty: bool,
    tx_full: bool,
}

impl Uart {
    /// The `new` function creates a new `Uart` instance with given base
    /// address. It's an unsafe function due to the usage of raw pointers.
    pub const unsafe fn new(base_addr: *mut u8) -> Uart {
        Uart {
            payload_addr: base_addr.cast(),
            flags_addr: base_addr.cast::<u8>().cast_const().add(4),
        }
    }

    /// UART status register output
    pub fn read_status(&mut self) -> UartStatus {
        let flags: u8 = unsafe { self.flags_addr.read_volatile() };

        let rx_mask = 0b10;
        let rx_empty = flags & rx_mask;

        let tx_mask = 0b01;
        let tx_full = flags & tx_mask;

        UartStatus {
            rx_empty: rx_empty != 0,
            tx_full: tx_full != 0,
        }
    }

    /// The `receive` function attempts to receive data from the UART. If no
    /// data is available, it keeps looping until data is available.
    pub fn receive(&mut self) -> u8 {
        loop {
            if let Some(val) = self.try_receive() {
                return val;
            }
        }
    }

    /// The `try_receive` function attempts to receive data from the UART. If no
    /// data is available, it returns None.
    pub fn try_receive(&mut self) -> Option<u8> {
        if self.read_status().rx_empty {
            None
        } else {
            unsafe {
                let data: u8 = self.payload_addr.read_volatile();
                Some(data)
            }
        }
    }

    /// The `send` function sends the given data to the UART. If the UART is
    /// unable to accept the data, it keeps looping until it can send the data.
    pub fn send(&mut self, data: u8) {
        loop {
            if let Ok(()) = self.try_send(data) {
                return;
            }
        }
    }

    /// The `try_send` function attempts to send the given data to the UART. If
    /// the UART is unable to accept the data, it returns an error.
    pub fn try_send(&mut self, data: u8) -> Result<(), ()> {
        if self.read_status().tx_full {
            Err(())
        } else {
            unsafe {
                self.payload_addr.write_volatile(data);
                Ok(())
            }
        }
    }
}

impl ufmt::uWrite for Uart {
    fn write_str(&mut self, s: &str) -> Result<(), Self::Error> {
        for b in s.bytes() {
            self.send(b);
        }
        Ok(())
    }

    type Error = ();
}
