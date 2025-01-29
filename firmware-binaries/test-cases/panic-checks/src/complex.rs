#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_sys::uart::Uart;

#[cfg(not(test))]
use riscv_rt::entry;

const UART_ADDR: *const () = (0b11 << 30) as *const ();

gdb_trace::gdb_panic! {
    unsafe { Uart::new(UART_ADDR) }
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // not sure what to do for this one yet
    loop {}
}
