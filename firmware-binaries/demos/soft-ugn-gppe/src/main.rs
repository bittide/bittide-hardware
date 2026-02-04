#![no_std]
#![cfg_attr(not(test), no_main)]
// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::hals::soft_ugn_demo_gppe as hal;

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: hal::DeviceInstances = unsafe { hal::DeviceInstances::new() };

// Declare the external C main function
extern "C" {
    fn c_main() -> !;
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let uart = &mut INSTANCES.uart;

    ufmt::uwriteln!(uart, "Hello from Rust!").unwrap();
    ufmt::uwriteln!(uart, "Calling C..").unwrap();

    unsafe { c_main() }
}

#[panic_handler]
fn panic_handler(_: &core::panic::PanicInfo) -> ! {
    loop {
        continue;
    }
}
