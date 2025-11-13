// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use bittide_hal::wb_to_df_test::DeviceInstances;
#[cfg(not(test))]
use riscv_rt::entry;
// use ufmt::{uwrite, uwriteln}; // Uncomment for debugging

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals.
    // let mut uart = INSTANCES.uart;
    // uwriteln!(uart, "Starting WbToDfTest").unwrap();
    let source: bittide_hal::shared_devices::WbToDfReference = INSTANCES.wb_to_df_reference;
    let destination = INSTANCES.wb_to_df_test;
    for i in source.value_volatile_iter() {
        // uwrite!(uart, "Writing {:?}", i).unwrap();
        destination.set_data(i);
        // uwriteln!(uart, ".").unwrap();
        destination.set_commit(());
    }

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
