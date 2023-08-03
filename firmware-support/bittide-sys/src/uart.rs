// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

pub struct UartStatus {
    pub receive_buffer_empty: bool,
    pub transmit_buffer_full: bool,
}

pub struct TransmitBufferFull;
pub struct ReceiveBufferEmpty;

/// `Uart` is a structure representing a universal asynchronous receiver-transmitter.
pub struct Uart {
    /// `payload_addr` is a mutable pointer to the address of the data payload.
    payload_addr: *mut u8,
    /// `flags_addr` is a constant pointer to the address of the flags.
    flags_addr: *const u8,
}

impl Uart {
    /// Create a new [`Uart`] instance given a base address.
    ///
    /// # Safety
    ///
    /// The `base_addr` pointer MUST BE a valid pointer that is backed
    /// by either a memory mapped UART instance or at valid read-writable memory
    /// (which will likely cause incorrect behaviour, but not break memory safety)
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
            receive_buffer_empty: rx_empty != 0,
            transmit_buffer_full: tx_full != 0,
        }
    }

    /// The `receive` function attempts to receive data from the UART. If no
    /// data is available, it keeps looping until data is available.
    pub fn receive(&mut self) -> u8 {
        loop {
            if let Ok(val) = self.try_receive() {
                return val;
            }
        }
    }

    /// The `try_receive` function attempts to receive data from the UART. If no
    /// data is available, it returns None.
    pub fn try_receive(&mut self) -> Result<u8, ReceiveBufferEmpty> {
        if self.read_status().receive_buffer_empty {
            Err(ReceiveBufferEmpty)
        } else {
            unsafe {
                let data: u8 = self.payload_addr.read_volatile();
                Ok(data)
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
    pub fn try_send(&mut self, data: u8) -> Result<(), TransmitBufferFull> {
        if self.read_status().transmit_buffer_full {
            Err(TransmitBufferFull)
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
