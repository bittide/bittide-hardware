#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;
use ufmt::uwriteln;

use bittide_hal::manual_additions::si539x_spi::Config;
use bittide_hal::si539x_spi::DeviceInstances;

use bittide_macros::load_clock_config_csv;

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };
const CONFIG_200: Config<3, 590, 5> =
    load_clock_config_csv!("../../../bittide/data/clock_configs/Si5395J-200MHz-1ppb-Registers.csv");

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let si539x_spi = INSTANCES.si539x_spi;
    let timer = INSTANCES.timer;
    let mut uart = INSTANCES.uart;

    uwriteln!(uart, "Writing configuration with f = 200 MHz...").unwrap();
    let start = timer.now();
    si539x_spi.write_configuration(&timer, &CONFIG_200, &mut uart);
    let end = timer.now();
    uwriteln!(uart, "Writing configuration took {}", end - start).unwrap();

    loop {
        continue;
    }
}

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    loop {
        continue;
    }
}
