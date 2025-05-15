#![no_std]
#![cfg_attr(not(test), no_main)]
#![allow(const_item_mutation)]
// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::uwriteln;

use bittide_hal::hals::vexriscv as hal;

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: hal::DeviceInstances = unsafe { hal::DeviceInstances::new() };

// this is only a function so that we can breakpoint on it
fn test_success() {
    INSTANCES
        .statusregister
        .set_status(hal::TestStatus::Success);
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let uart = &mut INSTANCES.uart;

    let names = ["Rust", "RISC-V", "Haskell"];
    for name in names {
        uwriteln!(uart, "Hello from {}!", name).unwrap();
    }
    uwriteln!(uart, "This can also do {} {:#x}", "debug prints", 42).unwrap();

    test_success();

    uwriteln!(uart, "Going in echo mode!").unwrap();

    loop {
        let c = uart.data();
        uart.set_data(c);
    }
}

#[panic_handler]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    INSTANCES.statusregister.set_status(hal::TestStatus::Fail);
    loop {
        continue;
    }
}
