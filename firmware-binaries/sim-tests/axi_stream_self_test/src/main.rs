#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::uwriteln;

use bittide_hal::shared_devices::uart::Uart;
use bittide_sys::axi::self_test::self_test;
use bittide_sys::axi::{AxiRx, AxiTx};
#[cfg(not(test))]
use riscv_rt::entry;

const UART_ADDR: *mut u8 = (2 << 29) as *mut u8;

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
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    let mut uart = unsafe { Uart::new(UART_ADDR) };
    uwriteln!(uart, "Woops, I panicked!").unwrap();
    loop {
        continue;
    }
}
