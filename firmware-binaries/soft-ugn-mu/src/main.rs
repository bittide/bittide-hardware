#![no_std]
#![cfg_attr(not(test), no_main)]
// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::hals::soft_ugn_demo_mu as hal;

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: hal::DeviceInstances = unsafe { hal::DeviceInstances::new() };

#[cfg_attr(not(test), entry)]
#[allow(clippy::empty_loop)]
fn main() -> ! {
    let uart = &mut INSTANCES.uart;

    ufmt::uwriteln!(uart, "Hello!").unwrap();

    loop {}
}

#[panic_handler]
#[allow(clippy::empty_loop)]
fn panic_handler(_: &core::panic::PanicInfo) -> ! {
    loop {}
}
