#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::uwriteln;

use bittide_sys::i2c::I2CError;
use bittide_sys::si534x::SI534X;
use bittide_sys::uart::Uart;

#[cfg(not(test))]
use riscv_rt::entry;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = unsafe { Uart::new(0x8000_0000 as *mut u8) };
    let mut si534x = unsafe { SI534X::new(0xc000_0000 as *mut u8, 0x69) };

    let names = ["Rust", "RISC-V", "Haskell"];
    for name in names {
        uwriteln!(uart, "Hello from {}!", name).unwrap();
    }
    uwriteln!(uart, "This can also do {} {:#x}", "debug prints", 42).unwrap();
    _ = uart.receive();

    // Getting and setting clock divider
    let mut clk_div = si534x.get_clock_divider();
    uwriteln!(uart, "i2c clk div:\n{:?}", clk_div).unwrap();
    clk_div = 300;
    uwriteln!(uart, "Changing i2c clk div to {}", clk_div).unwrap();
    si534x.set_clock_divider(clk_div);
    clk_div = si534x.get_clock_divider();
    uwriteln!(uart, "i2c clk div:\n{:?}", clk_div).unwrap();

    // Read data at all addresses on page 0
    for addr in 0..255 {
        match si534x.read_byte(0x00, addr) {
            Ok(data) => uwriteln!(uart, "Read address {:X} with {:X}", addr, data).unwrap(),
            Err(e) => match e {
                I2CError::ArbitrationLost => uwriteln!(uart, "I2CError::ArbitrationLost").unwrap(),
                I2CError::NotAcknowledged => uwriteln!(uart, "I2CError::NotAcknowledged").unwrap(),
                _ => (),
            },
        };
    }

    uwriteln!(uart, "Going in echo mode!").unwrap();
    loop {
        let c = uart.receive();
        uart.send(c);
    }
}

#[panic_handler]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    loop {
        continue;
    }
}
