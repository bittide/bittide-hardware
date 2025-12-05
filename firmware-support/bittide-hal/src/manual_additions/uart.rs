// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use crate::shared_devices::uart::Uart;

pub struct UartStatus {
    pub receive_buffer_empty: bool,
    pub transmit_buffer_full: bool,
}

pub struct TransmitBufferFull;
pub struct ReceiveBufferEmpty;

impl Uart {
    /// UART status register output
    pub fn read_status(&self) -> UartStatus {
        let flags = self.status()[0];

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
    pub fn receive(&self) -> u8 {
        loop {
            if let Ok(val) = self.try_receive() {
                return val;
            }
        }
    }

    /// The `try_receive` function attempts to receive data from the UART. If no
    /// data is available, it returns None.
    pub fn try_receive(&self) -> Result<u8, ReceiveBufferEmpty> {
        if self.read_status().receive_buffer_empty {
            Err(ReceiveBufferEmpty)
        } else {
            Ok(self.data()[0])
        }
    }

    /// The `send` function sends the given data to the UART. If the UART is
    /// unable to accept the data, it keeps looping until it can send the data.
    pub fn send(&self, data: u8) {
        loop {
            if let Ok(()) = self.try_send(data) {
                return;
            }
        }
    }

    /// The `try_send` function attempts to send the given data to the UART. If
    /// the UART is unable to accept the data, it returns an error.
    pub fn try_send(&self, data: u8) -> Result<(), TransmitBufferFull> {
        if self.read_status().transmit_buffer_full {
            Err(TransmitBufferFull)
        } else {
            self.set_data([data]);
            Ok(())
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

impl core::fmt::Write for Uart {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        for b in s.bytes() {
            self.send(b);
        }
        Ok(())
    }
}

impl Clone for Uart {
    fn clone(&self) -> Self {
        unsafe { Uart::new(self.0) }
    }
}
