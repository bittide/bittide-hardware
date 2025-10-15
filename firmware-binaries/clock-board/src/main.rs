#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;
use ufmt::uwriteln;

use bittide_hal::clock_board_config_test::DeviceInstances;
use bittide_hal::clock_board_config_test::DomainDiffCounters;
use bittide_hal::manual_additions::si539x_spi::Config;
use bittide_hal::manual_additions::timer::Duration;
use bittide_hal::shared::devices::Timer;
use bittide_hal::shared::devices::Uart;

use bittide_macros::load_clock_config_csv;

#[cfg(not(test))]
use riscv_rt::entry;

enum FrequencyCheckError {
    CounterInactive,
    CounterOutOfRange,
    CouldNotRead,
    CounterIndexOutOfRange,
}

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };
const CONFIG_100: Config<3, 590, 5> =
    load_clock_config_csv!("../../bittide/data/clock_configs/Si5395J-100MHz-1ppb-Registers.csv");
const CONFIG_200: Config<3, 590, 5> =
    load_clock_config_csv!("../../bittide/data/clock_configs/Si5395J-200MHz-1ppb-Registers.csv");

const TOLERANCE: i32 = 1_000_000;

/// Check if the programmed clock runs at the expected frequency.
///
/// Runs the single domain difference counter for 1 second and verify that the
/// difference is within the expected range.
fn check_frequency(
    timer: &Timer,
    uart: &mut Uart,
    domain_diff_counters: &DomainDiffCounters,
    expected: i32,
) -> Result<(), FrequencyCheckError> {
    uwriteln!(uart, "Starting domain diff counters...").unwrap();
    domain_diff_counters.set_enable(0, true);
    match domain_diff_counters.counters_active(0) {
        None => {
            uwriteln!(uart, "Counter with index 0 could not be accessed").unwrap();
            return Err(FrequencyCheckError::CounterIndexOutOfRange);
        }
        Some(false) => {
            uwriteln!(uart, "Counter 0 is inactive").unwrap();
            return Err(FrequencyCheckError::CounterInactive);
        }
        Some(true) => (),
    }
    let start = domain_diff_counters.counters(0);
    timer.wait(Duration::from_secs(1));
    let end = domain_diff_counters.counters(0);
    domain_diff_counters.set_enable(0, false);

    let diff = match (start, end) {
        (Some(s), Some(e)) => e - s,
        _ => {
            uwriteln!(uart, "Could not read domain diff counter").unwrap();
            return Err(FrequencyCheckError::CouldNotRead);
        }
    };

    if (diff - expected).abs() > TOLERANCE {
        uwriteln!(
            uart,
            "Difference counter is {} and not within {} of {}",
            diff,
            TOLERANCE,
            expected
        )
        .unwrap();
        Err(FrequencyCheckError::CounterOutOfRange)
    } else {
        uwriteln!(
            uart,
            "Difference counter is {}, which is within {} of {}",
            diff,
            TOLERANCE,
            expected
        )
        .unwrap();
        Ok(())
    }
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let si539x_spi = INSTANCES.si539x_spi;
    let timer = INSTANCES.timer;
    let mut uart = INSTANCES.uart;
    let domain_diff_counters = INSTANCES.domain_diff_counters;

    // Write a clock configuration with a frequency of 200 MHz.
    uwriteln!(uart, "Writing configuration with f = 200 MHz...").unwrap();
    let start = timer.now();
    si539x_spi.write_configuration(&timer, &CONFIG_200, &mut uart);
    let end = timer.now();
    uwriteln!(uart, "Writing configuration took {}", end - start).unwrap();
    // Compare the programmed clock (200 MHz) against the system clock (125 MHz)
    // using a domain difference counter. After 1 second the domain difference
    // counter is expected to be around 75M.
    for i in 0..10_u8 {
        if unsafe { domain_diff_counters.counters_active_unchecked(0) } {
            uwriteln!(uart, "Counter 0 active after {} retries", i).unwrap();
            break;
        }
        uwriteln!(uart, "Retry {}: Counter 0 is inactive", i).unwrap();
        timer.wait(Duration::from_millis(100));
    }
    let result_a = check_frequency(&timer, &mut uart, &domain_diff_counters, 75_000_000);

    // Write a clock configuration with a frequency of 100 MHz.
    uwriteln!(uart, "Writing configuration with f = 100 MHz...").unwrap();
    let start = timer.now();
    si539x_spi.write_configuration(&timer, &CONFIG_100, &mut uart);
    let end = timer.now();
    uwriteln!(uart, "Writing configuration took {}", end - start).unwrap();
    // Compare the programmed clock (100 MHz) against the system clock (125 MHz)
    // using a domain difference counter. After 1 second the domain difference
    // counter is expected to be around -25M.
    for i in 0..10_u8 {
        if unsafe { domain_diff_counters.counters_active_unchecked(0) } {
            uwriteln!(uart, "Counter 0 active after {} retries", i).unwrap();
            break;
        }
        uwriteln!(uart, "Retry {}: Counter 0 is inactive", i).unwrap();
        timer.wait(Duration::from_millis(100));
    }
    let result_b = check_frequency(&timer, &mut uart, &domain_diff_counters, -25_000_000);

    match (result_a, result_b) {
        (Ok(()), Ok(())) => uwriteln!(uart, "Test passed").unwrap(),
        _ => uwriteln!(uart, "Test failed").unwrap(),
    };

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
