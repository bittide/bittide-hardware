#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_sys::uart::Uart;
use core::fmt::Write;

#[cfg(not(test))]
use riscv_rt::entry;

const UART_ADDR: *const () = (0b11 << 30) as *const ();

gdb_trace::gdb_panic! {
    unsafe { Uart::new(UART_ADDR) }
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = unsafe { Uart::new(UART_ADDR) };
    let result = collatz(27, 1);
    writeln!(uart, "{result}").unwrap();
    loop {}
}

#[inline(never)]
fn collatz(n: usize, c: usize) -> usize {
    if n == 1 {
        c
    } else if n >= 100 {
        panic!("woah, big!");
    } else if n % 2 == 1 {
        collatz(n / 2, c + 1)
    } else {
        collatz(3 * n + 1, c + 1)
    }
}
