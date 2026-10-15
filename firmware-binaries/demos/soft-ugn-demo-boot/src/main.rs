#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;

use bittide_hal::manual_additions::clock_chip::Config;
use bittide_hal::soft_ugn_demo_boot::DeviceInstances;
use bittide_macros::load_clock_config_csv;

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };
const CONFIG_200: Config<3, 524, 5> =
    load_clock_config_csv!("../../../bittide/data/clock_configs/Si5345-200MHz-10ppb-Registers.csv");

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    bittide_cpus::boot::run(
        INSTANCES.i2c_controller,
        INSTANCES.si5345_i2c,
        INSTANCES.timer,
        INSTANCES.transceivers,
        &mut uart,
        &CONFIG_200,
    )
}

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    loop {
        continue;
    }
}
