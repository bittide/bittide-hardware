// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use ufmt::uwriteln;

use bittide_hal::shared::devices::uart::Uart;
use bittide_sys::ugn::Ugn;
#[cfg(not(test))]
use riscv_rt::entry;

const UART_ADDR: *mut u8 = (0b10 << 30) as *mut u8;
const UGN_ADDR: *const Ugn = (0b11 << 30) as *const Ugn;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals.
    let mut uart = unsafe { Uart::new(UART_ADDR) };
    let ugn = unsafe { UGN_ADDR.read_volatile() };

    uwriteln!(
        uart,
        "(0x{:16X},0x{:16X})",
        ugn.local_counter,
        ugn.remote_counter
    )
    .unwrap();
    loop {
        continue;
    }
}

#[panic_handler]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    loop {
        continue;
    }
}
