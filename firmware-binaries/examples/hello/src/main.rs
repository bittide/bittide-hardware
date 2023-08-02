#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::uwriteln;

use bittide_sys::uart::Uart;

#[cfg(not(test))]
use riscv_rt::entry;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = unsafe { Uart::new(0x8000_0000 as *mut u8) };

    let names = ["Rust", "RISC-V", "Haskell"];
    for name in names {
        uwriteln!(uart, "Hello from {}!", name).unwrap();
    }
    uwriteln!(uart, "This can also do {} {:#x}", "debug prints", 42).unwrap();
    uwriteln!(uart, "Going in echo mode!").unwrap();
    loop {
        let c = uart.receive();
        uart.send(c);
    }
}
