#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::uwriteln;

use bittide_sys::time::self_test::self_test;
use bittide_sys::{time::Clock, uart::Uart};
#[cfg(not(test))]
use riscv_rt::entry;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals.
    let mut uart = unsafe { Uart::new(0x8000_0000 as *const ()) };
    let clock = unsafe { Clock::new(0xc000_0000 as *const ()) };
    let test_results = self_test(clock);
    uwriteln!(uart, "Start time self test").unwrap();
    for (name, result) in test_results {
        match result {
            Some(s) => uwriteln!(uart, "{}: Some({})", name, s).unwrap(),
            None => uwriteln!(uart, "{}: None", name).unwrap(),
        };
    }
    uwriteln!(uart, "Done").unwrap();
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
