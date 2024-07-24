// SPDX-FileCopyrightText: 2023-2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::derive::uDebug;

use crate::i2c::{I2CError, I2C};

#[derive(uDebug)]
pub struct SI534X {
    i2c: I2C,
    slave_addr: u8,
    page: Option<u8>,
    addr: Option<u8>,
}

impl SI534X {
    /// Create a new [`SI534X`] instance given a base address and the slave
    /// address of the SI534X chip.
    ///
    /// # Safety
    ///
    /// The `base_addr` pointer MUST BE a valid pointer that is backed
    /// by either a memory mapped UART instance or at valid read-writable memory
    /// (which will likely cause incorrect behaviour, but not break memory safety)
    pub unsafe fn new(base_addr: *mut u8, slave_addr: u8) -> SI534X {
        let mut si = SI534X {
            i2c: I2C::new(base_addr),
            slave_addr,
            page: None,
            addr: None,
        };
        si.i2c.init();
        si
    }

    pub fn read_byte(&mut self, page: u8, addr: u8) -> Result<u8, I2CError> {
        self.set_page(page)?;

        self.i2c.claim_bus_with_retry()?;
        self.i2c.write_byte(self.slave_addr << 1)?;
        self.i2c.write_byte(addr)?;
        self.addr = Some(addr);
        self.i2c.release_bus();

        self.i2c.claim_bus_with_retry()?;
        self.i2c.write_byte((self.slave_addr << 1) | 1)?;
        let data = self.i2c.read_byte()?;
        self.i2c.release_bus();

        Ok(data)
    }

    pub fn write_byte(&mut self, page: u8, addr: u8, data: u8) -> Result<(), I2CError> {
        self.set_page(page)?;

        self.i2c.claim_bus_with_retry()?;
        self.i2c.write_byte(self.slave_addr << 1)?;
        self.i2c.write_byte(addr)?;
        self.addr = Some(addr);
        self.i2c.write_byte(data)?;
        self.i2c.release_bus();

        Ok(())
    }

    pub fn set_page(&mut self, page: u8) -> Result<(), I2CError> {
        if self.page != Some(page) {
            self.i2c.claim_bus_with_retry()?;
            self.i2c.write_byte(self.slave_addr << 1)?;
            self.i2c.write_byte(0x01)?;
            self.i2c.write_byte(page)?;
            self.page = Some(page);
            self.i2c.release_bus();
        }
        Ok(())
    }

    /// Clear the internal page and address to force setting the page again.
    pub fn clear_state(&mut self) {
        self.page = None;
        self.addr = None;
    }

    pub fn finc(&mut self) -> Result<(), I2CError> {
        self.write_byte(0, 0x1D, 0b01)
    }

    pub fn fdec(&mut self) -> Result<(), I2CError> {
        self.write_byte(0, 0x1D, 0b10)
    }

    pub fn get_clock_divider(&mut self) -> u16 {
        self.i2c.get_clock_divider()
    }

    pub fn set_clock_divider(&mut self, clk_div: u16) {
        self.i2c.set_clock_divider(clk_div)
    }
}
