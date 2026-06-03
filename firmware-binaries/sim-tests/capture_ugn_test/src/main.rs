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
    let capture_ugns = DEVICES.capture_ugns;

    while !capture_ugns.has_captured().get(0).unwrap() {
        continue;
    }
    uwriteln!(
        uart,
        "({},{})",
        capture_ugns.local_counter(0).unwrap(),
        capture_ugns.remote_counter(0).unwrap(),
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
