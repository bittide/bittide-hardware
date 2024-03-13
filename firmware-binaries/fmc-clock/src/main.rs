#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;
use ufmt::{derive::uDebug, uwriteln};

use bittide_sys::{
    clock_config::{self, ConfigEntry, ParserState},
    data_counts::DataCounts,
    i2c::I2CError,
    si534x::SI534X,
    time::{Clock, Duration},
    uart::Uart,
};
#[cfg(not(test))]
use riscv_rt::entry;

#[derive(uDebug)]
pub enum TestState {
    Busy,
    Fail,
    Success,
}

#[derive(uDebug)]
pub struct ControlRegister {
    addr : *mut u8,
    clock_init_done: bool,
    test_state: TestState,
}

impl ControlRegister {
    pub fn new(base_addr: *mut u8) -> ControlRegister {
        ControlRegister {
            addr: base_addr,
            clock_init_done: false,
            test_state: TestState::Busy,
        }
    }

    pub fn set_clock_init_done(&mut self) {
        self.clock_init_done = true;
        self.update();
    }

    pub fn set_test_state(&mut self, ts: TestState) {
        self.test_state = ts;
        self.update();
    }

    fn update(&mut self) {
        let mut p:u8 = 0x00;
        if self.clock_init_done {
            p |= 0b100;
        }
        match self.test_state {
            TestState::Busy => p |= 0b000,
            TestState::Fail => p |= 0b001,
            TestState::Success => p |= 0b010,
        }
        unsafe { self.addr.write_volatile(p)};
    }
}

pub enum Test {
    Idle,
    /// Keep pressing FDEC, see if counter falls below certain threshold
    FDec,
    /// Keep pressing FINC, see if counter exceeds certain threshold
    FInc,
    /// 'FDec' test followed by an 'FInc' one
    FDecInc,
    /// 'FInc' test followed by an 'FDec' one
    FIncDec,
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let clock = unsafe { Clock::new((0b11_000 << 27) as *const u32) };
    let mut si534x = unsafe { SI534X::new((0b11_001 << 27) as *mut u8, 0x69) };
    let mut control_reg = ControlRegister::new((0b11_010 << 27) as *mut u8);
    let status_reg_addr = (0b11_011 << 27) as *mut u8;
    let datacounts = unsafe { DataCounts::from_base_addr((0b11_100 << 27) as *mut u32)};
    let mut uart = unsafe { Uart::new((0b11_101 << 27) as *mut u8) };

    uwriteln!(uart, "Starting RiscV core").unwrap();

    // Set up clock chip i2c interface.
    let clk_div = 300;
    si534x.set_clock_divider(clk_div);

    // Parse config.csv using bittide::clock_config
    uwriteln!(uart, "Starting clock chip configuration").unwrap();
    let si534x_config = include_str!("config.csv");
    let mut parser = clock_config::ClockConfigParser::new();
    for line in si534x_config.lines() {
        if !parser.is_done() {
            let old_state = parser.state;
            if let Ok(Some(ConfigEntry { page, addr, data })) = parser.parse_line(line) {
                match si534x.write_byte(page, addr, data) {
                    Ok(()) => {}
                    Err(I2CError::ArbitrationLost) => {
                        uwriteln!(uart, "I2CError::ArbitrationLost while writing to register at {:02X}{:02X}", page, addr).unwrap();
                    }
                    Err(I2CError::NotAcknowledged) => {
                        uwriteln!(uart, "I2CError::NotAcknowledged while writing to register at {:02X}{:02X}", page, addr).unwrap();
                    }
                    Err(I2CError::BusClaimedByOther) => {
                        uwriteln!(uart, "I2CError::BusClaimedByOther while writing to register at {:02X}{:02X}", page, addr).unwrap();
                    }
                }
                // Read back the register
                match si534x.read_byte(page, addr) {
                    Ok(read_data) => {
                        if read_data != data {
                            uwriteln!(
                                uart,
                                "At {:02X}{:02X} got {:02X}, but got {:02X}",
                                page,
                                addr,
                                read_data,
                                data
                            )
                            .unwrap();
                        }
                    }
                    Err(I2CError::ArbitrationLost) => {
                        uwriteln!(
                            uart,
                            "I2CError::ArbitrationLost while reading back register at {:02X}{:02X}",
                            page,
                            addr
                        )
                        .unwrap();
                    }
                    Err(I2CError::NotAcknowledged) => {
                        uwriteln!(uart, "I2CError::NotAcknowledged while reading back register at {:02X}{:02X}", page, addr).unwrap();
                    }
                    Err(I2CError::BusClaimedByOther) => {
                        uwriteln!(uart, "I2CError::BusClaimedByOther while reading back register at {:02X}{:02X}", page, addr).unwrap();
                    }
                }
            };
            if old_state == ParserState::Preamble && parser.state == ParserState::PostPreambleDelay
            {
                // Wait 300ms after writing the preamble
                clock.wait(Duration::from_millis(300));
            }
        }
    }
    uwriteln!(uart, "Configured clock chip").unwrap();

    // Communicate to hardware that the CPU is done programming the I2C chip
    control_reg.set_clock_init_done();

    // Do the FInc FDec tests

    uwriteln!(uart, "Going in echo mode!").unwrap();
    loop {
        let c = uart.receive();
        uart.send(c);
    }
}

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    loop {
        continue;
    }
}
