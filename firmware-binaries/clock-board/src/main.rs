#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;
use ufmt::derive::uDebug;
use ufmt::uwriteln;

use bittide_hal::manual_additions::si539x_spi::Config;
use bittide_hal::manual_additions::timer::Duration;
use bittide_hal::shared::devices::Timer;
use bittide_hal::shared::devices::Uart;
use bittide_hal::si539x_configuration::DeviceInstances;
use bittide_hal::si539x_configuration::DomainDiffCounters;

use bittide_macros::load_clock_config_csv;

#[cfg(not(test))]
use riscv_rt::entry;

#[derive(uDebug, Clone, Copy)]
enum FrequencyCheckError {
    CounterInactive,
    CounterInaccessible,
    UnexpectedFrequency,
}

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };
const CONFIG_100: Config<3, 590, 5> =
    load_clock_config_csv!("../../bittide/data/clock_configs/Si5395J-100MHz-10ppb-and-out1.csv");
const CONFIG_200: Config<3, 584, 5> =
    load_clock_config_csv!("../../bittide/data/clock_configs/Si5395J-200MHz-10ppb-and-out1.csv");

const SYS_FREQ: u32 = 125_000_000;
const TOLERANCE: i32 = 100_000;

/// Check if the programmed clock runs at the expected frequency.
///
/// Uses the domain difference counter to verify the programmed clock runs at
/// the expected frequency by comparing it to the system clock.
fn check_frequency(
    timer: &Timer,
    uart: &mut Uart,
    domain_diff_counters: &DomainDiffCounters,
    expected_freq: u32,
) -> Result<(), FrequencyCheckError> {
    uwriteln!(uart, "Enabling domain difference counter...").unwrap();
    domain_diff_counters.set_enable(0, true);
    timer.wait(Duration::from_micros(1));

    match domain_diff_counters.counters_active(0) {
        None => {
            uwriteln!(uart, "Counter with index 0 could not be accessed").unwrap();
            return Err(FrequencyCheckError::CounterInaccessible);
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
            uwriteln!(uart, "Counter with index 0 could not be accessed").unwrap();
            return Err(FrequencyCheckError::CounterInaccessible);
        }
    };

    let expected_diff = expected_freq as i32 - SYS_FREQ as i32;
    if (diff - expected_diff).abs() > TOLERANCE {
        uwriteln!(
            uart,
            "Unexpected domain difference counter: {} (expected {} ± {})",
            diff,
            expected_diff,
            TOLERANCE
        )
        .unwrap();
        let calculated_freq = SYS_FREQ as i32 + diff;
        uwriteln!(uart, "Calculated frequency is {} Hz", calculated_freq).unwrap();
        Err(FrequencyCheckError::UnexpectedFrequency)
    } else {
        uwriteln!(
            uart,
            "Difference counter is {}, which is within {} of {}",
            diff,
            TOLERANCE,
            expected_diff
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
    let _ = si539x_spi.write_configuration(&timer, &CONFIG_200, &mut uart);
    let end = timer.now();
    uwriteln!(uart, "Writing configuration took {}", end - start).unwrap();
    let result_a = check_frequency(&timer, &mut uart, &domain_diff_counters, 200e6 as u32);
    match result_a {
        Ok(()) => uwriteln!(uart, "200 MHz frequency check passed").unwrap(),
        Err(e) => uwriteln!(uart, "200 MHz frequency check failed: {:?}", e).unwrap(),
    }

    // Write a clock configuration with a frequency of 100 MHz.
    uwriteln!(uart, "Writing configuration with f = 100 MHz...").unwrap();
    let start = timer.now();
    let _ = si539x_spi.write_configuration(&timer, &CONFIG_100, &mut uart);
    let end = timer.now();
    uwriteln!(uart, "Writing configuration took {}", end - start).unwrap();
    let result_b = check_frequency(&timer, &mut uart, &domain_diff_counters, 100e6 as u32);
    match result_b {
        Ok(()) => uwriteln!(uart, "100 MHz frequency check passed").unwrap(),
        Err(e) => uwriteln!(uart, "100 MHz frequency check failed: {:?}", e).unwrap(),
    }

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
