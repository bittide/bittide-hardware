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

#[derive(PartialEq, uDebug)]
pub enum TestState {
    Busy,
    Fail,
    Success,
}

// A Control Register is a register which controls peripherals outside the CPU, and which
// can only be written to by the CPU.
#[derive(uDebug)]
pub struct ControlRegister {
    addr: *mut u8,
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

    pub fn clear_clock_init_done(&mut self) {
        self.clock_init_done = false;
        self.update();
    }

    pub fn set_test_state(&mut self, ts: TestState) {
        self.test_state = ts;
        self.update();
    }

    fn update(&mut self) {
        let mut p: u8 = 0x00;
        if self.clock_init_done {
            p |= 0b100;
        }
        match self.test_state {
            TestState::Busy => p |= 0b000,
            TestState::Fail => p |= 0b001,
            TestState::Success => p |= 0b010,
        }
        unsafe { self.addr.write_volatile(p) };
    }
}

#[derive(Copy, Clone, PartialEq)]
pub enum Test {
    /// Reconfigure the clock chip and reset test status
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

#[derive(uDebug)]
pub struct InvalidStatusRegister {
    pub val: u8,
}

// A Status Register is a register with which a peripheral can communicate its status
// to the CPU. This register can only be written to by the peripheral.
pub struct StatusRegister {
    pub addr: *mut u8,
    pub test: Test,
}

impl StatusRegister {
    pub fn new(base_addr: *mut u8) -> StatusRegister {
        StatusRegister {
            addr: base_addr,
            test: Test::Idle,
        }
    }

    pub fn get_test(&mut self) -> Result<Test, InvalidStatusRegister> {
        self.refresh()?;
        Ok(self.test)
    }

    fn refresh(&mut self) -> Result<(), InvalidStatusRegister> {
        let data = unsafe { self.addr.read_volatile() };
        self.test = Self::parse_reg(data)?;
        Ok(())
    }

    fn parse_reg(data: u8) -> Result<Test, InvalidStatusRegister> {
        if (data & 0b100) == 0 {
            Ok(Test::Idle)
        } else if (data & 0b100) == 0b100 {
            match data & 0b011 {
                0b00 => Ok(Test::FDec),
                0b01 => Ok(Test::FInc),
                0b10 => Ok(Test::FDecInc),
                0b11 => Ok(Test::FIncDec),
                _ => Err(InvalidStatusRegister { val: data }),
            }
        } else {
            Err(InvalidStatusRegister { val: data })
        }
    }
}

const THRESHOLD: i32 = 20_000;

pub fn test_fdec(
    data_counts: &DataCounts,
    si534x: &mut SI534X,
    threshold_up: i32,
    threshold_down: i32,
) -> Option<bool> {
    let dc = data_counts.data_counts().next().unwrap();
    if dc > threshold_up {
        return Some(false);
    } else if dc < threshold_down {
        return Some(true);
    }
    if si534x.fdec().is_err() {
        Some(false)
    } else {
        None
    }
}

pub fn test_finc(
    data_counts: &DataCounts,
    si534x: &mut SI534X,
    threshold_up: i32,
    threshold_down: i32,
) -> Option<bool> {
    let dc = data_counts.data_counts().next().unwrap();
    if dc > threshold_up {
        return Some(true);
    } else if dc < threshold_down {
        return Some(false);
    }
    if si534x.finc().is_err() {
        Some(false)
    } else {
        None
    }
}

pub fn configure_clock_chip(
    uart: &mut Uart,
    si534x: &mut SI534X,
    clock: &Clock,
) -> Result<(), I2CError> {
    // Parse config.csv using bittide::clock_config
    uwriteln!(uart, "Starting clock chip configuration").unwrap();
    let si534x_config = include_str!("config.csv");
    let mut parser = clock_config::ClockConfigParser::new();
    for line in si534x_config.lines() {
        if !parser.is_done() {
            let old_state = parser.state;
            if let Ok(Some(ConfigEntry { page, addr, data })) = parser.parse_line(line) {
                si534x.write_byte(page, addr, data)?;
            }
            if old_state == ParserState::Preamble && parser.state == ParserState::PostPreambleDelay
            {
                // Wait 300ms after writing the preamble
                clock.wait(Duration::from_millis(300));
            }
        }
    }
    uwriteln!(uart, "Configured clock chip").unwrap();
    Ok(())
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let clock = unsafe { Clock::new((0b0001 << 28) as *const u32) };
    let mut si534x = unsafe { SI534X::new((0b0010 << 28) as *mut u8, 0x69) };
    let mut control_reg = ControlRegister::new((0b0011 << 28) as *mut u8);
    let mut status_reg = StatusRegister::new((0b0101 << 28) as *mut u8);
    let data_counts = unsafe { DataCounts::from_base_addr((0b0110 << 28) as *mut u32) };
    let mut uart = unsafe { Uart::new((0b0111 << 28) as *mut u8) };

    uwriteln!(uart, "Starting RiscV core").unwrap();

    // Set up clock chip i2c interface.
    let clk_div = 300;
    si534x.set_clock_divider(clk_div);

    if configure_clock_chip(&mut uart, &mut si534x, &clock).is_ok() {
        // Communicate to hardware that the CPU is done programming the I2C chip
        control_reg.set_clock_init_done();
    }

    // Do the FInc FDec tests
    let mut last_status_reg;
    let mut sub_test = Test::Idle;
    let mut finc_decs: i32 = 0;
    loop {
        last_status_reg = status_reg.test;
        match status_reg.get_test() {
            Err(e) => {
                uwriteln!(
                    uart,
                    "Could not parse status register with data {:02X}",
                    e.val
                )
                .unwrap();
            }
            Ok(Test::Idle) => {
                if last_status_reg != status_reg.test {
                    control_reg.set_test_state(TestState::Busy);
                    finc_decs = 0;
                    control_reg.clear_clock_init_done();
                    match configure_clock_chip(&mut uart, &mut si534x, &clock) {
                        Ok(()) => {
                            control_reg.set_clock_init_done();
                        }
                        Err(e) => {
                            uwriteln!(uart, "Failed to configure clock: {:?}", e).unwrap();
                            control_reg.set_test_state(TestState::Fail);
                        }
                    }
                }
            }
            Ok(Test::FDec) => {
                if last_status_reg != status_reg.test {
                    uwriteln!(uart, "\nStarting FDec test").unwrap();
                    control_reg.set_test_state(TestState::Busy);
                }
                if control_reg.test_state != TestState::Busy {
                    continue;
                }
                finc_decs -= 1;
                match test_fdec(&data_counts, &mut si534x, THRESHOLD, -THRESHOLD) {
                    Some(true) => {
                        if control_reg.test_state == TestState::Busy {
                            uwriteln!(uart, "Pressed finc_dec {} times", finc_decs).unwrap();
                        }
                        control_reg.set_test_state(TestState::Success);
                    }
                    Some(false) => {
                        if control_reg.test_state == TestState::Busy {
                            uwriteln!(uart, "Pressed finc_dec {} times", finc_decs).unwrap();
                        }
                        control_reg.set_test_state(TestState::Fail);
                    }
                    None => {}
                }
            }
            Ok(Test::FInc) => {
                if last_status_reg != status_reg.test {
                    uwriteln!(uart, "\nStarting FInc test").unwrap();
                    control_reg.set_test_state(TestState::Busy);
                }
                if control_reg.test_state != TestState::Busy {
                    continue;
                }
                finc_decs += 1;
                match test_finc(&data_counts, &mut si534x, THRESHOLD, -THRESHOLD) {
                    Some(true) => {
                        if control_reg.test_state == TestState::Busy {
                            uwriteln!(uart, "Pressed finc_dec {} times", finc_decs).unwrap();
                        }
                        control_reg.set_test_state(TestState::Success);
                    }
                    Some(false) => {
                        if control_reg.test_state == TestState::Busy {
                            uwriteln!(uart, "Pressed finc_dec {} times", finc_decs).unwrap();
                        }
                        control_reg.set_test_state(TestState::Fail);
                    }
                    None => {}
                }
            }
            Ok(Test::FDecInc) => {
                if last_status_reg != status_reg.test {
                    uwriteln!(uart, "\nStarting FDecInc test").unwrap();
                    control_reg.set_test_state(TestState::Busy);
                    sub_test = Test::FDec;
                }
                if control_reg.test_state != TestState::Busy {
                    continue;
                }
                if sub_test == Test::FDec {
                    finc_decs -= 1;
                    match test_fdec(&data_counts, &mut si534x, THRESHOLD, -THRESHOLD) {
                        Some(true) => {
                            if control_reg.test_state == TestState::Busy {
                                uwriteln!(uart, "Pressed finc_dec {} times", finc_decs).unwrap();
                                finc_decs = 0;
                            }
                            sub_test = Test::FInc;
                        }
                        Some(false) => {
                            if control_reg.test_state == TestState::Busy {
                                uwriteln!(uart, "Pressed finc_dec {} times", finc_decs).unwrap();
                                finc_decs = 0;
                            }
                            control_reg.set_test_state(TestState::Fail);
                        }
                        None => {}
                    }
                } else if sub_test == Test::FInc {
                    finc_decs += 1;
                    match test_finc(&data_counts, &mut si534x, 0, 3 * -THRESHOLD) {
                        Some(true) => {
                            if control_reg.test_state == TestState::Busy {
                                uwriteln!(uart, "Pressed finc_dec {} times", finc_decs).unwrap();
                            }
                            control_reg.set_test_state(TestState::Success);
                        }
                        Some(false) => {
                            if control_reg.test_state == TestState::Busy {
                                uwriteln!(uart, "Pressed finc_dec {} times", finc_decs).unwrap();
                            }
                            control_reg.set_test_state(TestState::Fail);
                        }
                        None => {}
                    }
                }
            }
            Ok(Test::FIncDec) => {
                if last_status_reg != status_reg.test {
                    uwriteln!(uart, "\nStarting FIncDec test").unwrap();
                    control_reg.set_test_state(TestState::Busy);
                    sub_test = Test::FInc;
                }
                if control_reg.test_state != TestState::Busy {
                    continue;
                }
                if sub_test == Test::FInc {
                    finc_decs += 1;
                    match test_finc(&data_counts, &mut si534x, THRESHOLD, -THRESHOLD) {
                        Some(true) => {
                            if control_reg.test_state == TestState::Busy {
                                uwriteln!(uart, "Pressed finc_dec {} times", finc_decs).unwrap();
                                finc_decs = 0;
                            }
                            sub_test = Test::FDec;
                        }
                        Some(false) => {
                            if control_reg.test_state == TestState::Busy {
                                uwriteln!(uart, "Pressed finc_dec {} times", finc_decs).unwrap();
                                finc_decs = 0;
                            }
                            control_reg.set_test_state(TestState::Fail);
                        }
                        None => {}
                    }
                } else if sub_test == Test::FDec {
                    finc_decs -= 1;
                    match test_fdec(&data_counts, &mut si534x, 3 * THRESHOLD, 0) {
                        Some(true) => {
                            if control_reg.test_state == TestState::Busy {
                                uwriteln!(uart, "Pressed finc_dec {} times", finc_decs).unwrap();
                            }
                            control_reg.set_test_state(TestState::Success);
                        }
                        Some(false) => {
                            if control_reg.test_state == TestState::Busy {
                                uwriteln!(uart, "Pressed finc_dec {} times", finc_decs).unwrap();
                            }
                            control_reg.set_test_state(TestState::Fail);
                        }
                        None => {}
                    }
                }
            }
        }
    }
}

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    loop {
        continue;
    }
}
