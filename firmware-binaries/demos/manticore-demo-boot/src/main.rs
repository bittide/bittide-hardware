#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2026 QBayLogic
//
// SPDX-License-Identifier: Apache-2.0

// Boot CPU for the Manticore demo: identical to the other demos' boot
// (configure the Si539x clock chip + transceivers); it is independent of the
// user core.

use core::panic::PanicInfo;

use bittide_hal::hals::manticore_demo_boot::DeviceInstances;
use bittide_hal::manual_additions::si539x_spi::Config;
use bittide_macros::load_clock_config_csv;

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };
const CONFIG_200: Config<3, 590, 5> = load_clock_config_csv!(
    "../../../bittide/data/clock_configs/Si5395J-200MHz-10ppb-Registers.csv"
);

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    bittide_cpus::boot::run(
        INSTANCES.si539x_spi,
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
