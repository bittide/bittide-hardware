#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::uwriteln;

use bittide_hal::manual_additions::timer::self_test::self_test;
use bittide_hal::shared_devices::timer::Timer;
use bittide_hal::shared_devices::uart::Uart;
#[cfg(not(test))]
use riscv_rt::entry;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals.
    let mut uart = unsafe { Uart::new((0b010 << 29) as *mut u8) };
    let timer = unsafe { Timer::new((0b011 << 29) as *mut u8) };
    let test_results = self_test(timer);
    uwriteln!(uart, "Start time self test").unwrap();
    for (name, result) in test_results {
        match result {
            Some((s, None)) => uwriteln!(uart, "{}: Some({})", name, s).unwrap(),
            Some((s, Some(fail))) => uwriteln!(uart, "{}: Some({}. {:?})", name, s, fail).unwrap(),
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
