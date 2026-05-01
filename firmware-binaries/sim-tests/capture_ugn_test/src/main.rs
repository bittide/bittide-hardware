// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use ufmt::uwriteln;

use bittide_hal::hals::capture_ugn_test::DeviceInstances;
#[cfg(not(test))]
use riscv_rt::entry;

const DEVICES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals.
    let mut uart = DEVICES.uart;
    let capture_ugn = DEVICES.capture_ugn;

    while !capture_ugn.has_captured() {
        continue;
    }
    uwriteln!(
        uart,
        "({},{})",
        capture_ugn.local_counter(),
        capture_ugn.remote_counter(),
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
