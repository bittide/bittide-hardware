#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::uwriteln;

use bittide_sys::uart::Uart;

#[cfg(not(test))]
use riscv_rt::entry;

const STATUS_REG_ADDR: *mut u32 = 0xE000_0000 as *mut u32;
const UART_ADDR: *const () = 0xC000_0000 as *const ();

fn test_success() {
    unsafe {
        STATUS_REG_ADDR.write_volatile(1);
    }
}

fn test_failure() {
    unsafe {
        STATUS_REG_ADDR.write_volatile(2);
    }
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = unsafe { Uart::new(UART_ADDR) };

    let names = ["Rust", "RISC-V", "Haskell"];
    for name in names {
        uwriteln!(uart, "Hello from {}!", name).unwrap();
    }
    uwriteln!(uart, "This can also do {} {:#x}", "debug prints", 42).unwrap();
    uwriteln!(uart, "Going in echo mode!").unwrap();

    test_success();

    loop {
        let c = uart.receive();
        uart.send(c);
    }
}
