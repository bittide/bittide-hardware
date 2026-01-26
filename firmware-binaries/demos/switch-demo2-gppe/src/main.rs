#![no_std]
#![cfg_attr(not(test), no_main)]
// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::hals::switch_demo_gppe_gppe as hal;

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: hal::DeviceInstances = unsafe { hal::DeviceInstances::new() };

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let uart = &mut INSTANCES.uart;

    ufmt::uwriteln!(uart, "Hello!").unwrap();

    loop {
        continue;
    }
}

#[panic_handler]
fn panic_handler(_: &core::panic::PanicInfo) -> ! {
    loop {
        continue;
    }
}
