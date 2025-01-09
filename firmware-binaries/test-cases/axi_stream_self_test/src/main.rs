#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::fmt::Write;
use ufmt::uwriteln;

use bittide_sys::axi::self_test::self_test;
use bittide_sys::axi::{AxiRx, AxiTx};
use bittide_sys::uart::Uart;
#[cfg(not(test))]
use riscv_rt::entry;

const UART_ADDR: *const () = (2 << 29) as *const ();

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals.
    let uart = unsafe { Uart::new(UART_ADDR) };
    let tx = unsafe { AxiTx::new((3 << 29) as *const ()) };
    let rx: AxiRx<128> = unsafe { AxiRx::new((5 << 29) as *const ()) };
    self_test(uart, tx, rx);
    loop {
        continue;
    }
}

#[panic_handler]
fn panic_handler(info: &core::panic::PanicInfo) -> ! {
    let mut uart = unsafe { Uart::new(UART_ADDR) };
    uwriteln!(uart, "Woops, I panicked!").unwrap();
    writeln!(uart, "Info: {:?}", info).unwrap();
    loop {
        continue;
    }
}
