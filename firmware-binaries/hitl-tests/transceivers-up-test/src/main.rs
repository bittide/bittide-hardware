#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;
use ufmt::uwriteln;

use bittide_cpus::boot::{enable_transceivers, program_clock_board};
use bittide_cpus::transceivers::{monitor_links, print_statistics, wait_links_up};
use bittide_hal::hals::transceivers_up_test::DeviceInstances;
use bittide_hal::manual_additions::si539x_spi::Config;
use bittide_hal::manual_additions::timer::Duration;
use bittide_macros::load_clock_config_csv;

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };
const CONFIG_200: Config<3, 590, 5> = load_clock_config_csv!(
    "../../../bittide/data/clock_configs/Si5395J-200MHz-10ppb-Registers.csv"
);

/// Maximum time all links get to come up after enabling the transceivers.
const LINK_UP_TIMEOUT_SECS: u64 = 20;

/// Time all links should stay up for the test to pass.
const MONITOR_SECS: u64 = 10;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let si539x_spi = INSTANCES.si539x_spi;
    let timer = INSTANCES.timer;
    let transceivers = INSTANCES.transceivers;
    let transceiver_check = INSTANCES.transceiver_check;
    let mut uart = INSTANCES.uart;

    program_clock_board(&si539x_spi, &timer, &mut uart, &CONFIG_200);
    enable_transceivers(&transceivers, &mut uart);

    uwriteln!(uart, "Waiting for links to come up..").unwrap();
    let mut result = wait_links_up(
        &transceivers,
        &timer,
        Duration::from_secs(LINK_UP_TIMEOUT_SECS),
    );

    if result.is_ok() {
        uwriteln!(
            uart,
            "All links up, monitoring for {} seconds..",
            MONITOR_SECS
        )
        .unwrap();
        result = monitor_links(&transceivers, &timer, Duration::from_secs(MONITOR_SECS));
    }

    print_statistics(&transceivers, &mut uart);

    let mut all_passed = true;
    if let Err(err) = result {
        uwriteln!(uart, "FAIL: {:?}", err).unwrap();
        all_passed = false;
    }

    for (link, counter_error) in transceiver_check.counter_errors_volatile_iter().enumerate() {
        if counter_error {
            uwriteln!(uart, "FAIL: unexpected data received on link {}", link).unwrap();
            all_passed = false;
        }
    }

    if all_passed {
        uwriteln!(uart, "All tests passed").unwrap();
    } else {
        uwriteln!(uart, "Some tests failed").unwrap();
    }

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
