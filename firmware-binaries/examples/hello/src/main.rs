#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::uwriteln;

use bittide_sys::i2c::{I2CError, I2C};
use bittide_sys::uart::Uart;

#[cfg(not(test))]
use riscv_rt::entry;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals.
    let mut uart = unsafe { Uart::new((2 << 29) as *mut u8) };
    let mut i2c = unsafe { I2C::new((3 << 29) as *mut u8) };

    // Print Hello Worlds
    let names = ["Rust", "RISC-V", "Haskell"];
    for name in names {
        uwriteln!(uart, "Hello from {}!", name).unwrap();
    }
    uwriteln!(uart, "This can also do {} {:#x}", "debug prints", 42).unwrap();
    uwriteln!(uart, "i2c device:\n{:?}", i2c).unwrap();
    let mut flags = i2c.read_flags();
    uwriteln!(uart, "i2c flags:\n{:?}", flags).unwrap();

    uwriteln!(uart, "Deasserting i2c statemachine reset").unwrap();
    flags.statemachine_reset = false;
    i2c.write_flags(flags);
    uwriteln!(uart, "i2c flags:\n{:?}", flags).unwrap();
    uwriteln!(uart, "Claiming i2c bus").unwrap();
    if i2c.claim_bus().is_err() {
        uwriteln!(uart, "I2CError").unwrap();
    };
    uwriteln!(uart, "i2c bus claimed, status:\n{:?}", i2c.read_flags()).unwrap();
    let a = 0x69;
    match i2c.write_byte((a << 1) + 1) {
        Ok(_) => (),
        Err(e) => match e {
            I2CError::ArbitrationLost => uwriteln!(uart, "I2CError::ArbitrationLost").unwrap(),
            I2CError::NotAcknowledged => uwriteln!(uart, "I2CError::NotAcknowledged").unwrap(),
            _ => (),
        },
    };
    match i2c.read_byte() {
        Ok(d) => uwriteln!(uart, "Read {:X} from {:X}", d, a).unwrap(),
        Err(e) => match e {
            I2CError::ArbitrationLost => uwriteln!(uart, "I2CError::ArbitrationLost").unwrap(),
            I2CError::NotAcknowledged => uwriteln!(uart, "I2CError::NotAcknowledged").unwrap(),
            _ => (),
        },
    };

    i2c.release_bus();
    uwriteln!(uart, "i2c bus released, status: {:?}", i2c.read_flags()).unwrap();
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
