#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;
use ufmt::uwriteln;

use bittide_hal::clock_board_config_test::DeviceInstances;
use bittide_hal::clock_board_config_test::DomainDiffCounters;
use bittide_hal::manual_additions::si539x_spi::{Config, Si539xError};
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

    // Read the current design ID
    let design_id = si539x_spi.read_design_id();
    match core::str::from_utf8(&design_id) {
        Ok(s) => uwriteln!(uart, "Design ID: {}", s).unwrap(),
        Err(_) => uwriteln!(
            uart,
            "Could not convert design ID to ASCII: {:?}",
            design_id
        )
        .unwrap(),
    }

    // Write a clock configuration with a frequency of 200 MHz.
    uwriteln!(uart, "Writing configuration with f = 200 MHz...").unwrap();
    let start = timer.now();
    si539x_spi.write_configuration(&timer, &CONFIG_200, &mut uart);
    let end = timer.now();
    uwriteln!(uart, "Writing configuration took {}", end - start).unwrap();

    // Verify that the configuration is now on the board.
    uwriteln!(uart, "Verifying configuration...").unwrap();
    match si539x_spi.verify_configuration(&CONFIG_200) {
        Ok(()) => uwriteln!(uart, "Verified configuration").unwrap(),
        Err(Si539xError::NotExpectedConfig { design_id }) => {
            match core::str::from_utf8(&design_id) {
                Ok(s) => {
                    uwriteln!(uart, "Unexpected configuration found with design ID: {}", s)
                        .unwrap();
                }
                Err(_) => {
                    uwriteln!(
                        uart,
                        "Unexpected configuration, and could not convert design ID to ascii: {:?}",
                        design_id
                    )
                    .unwrap();
                }
            }
        }
        Err(_) => (),
    }

    // Check if `spiDone` register is actually set
    uwriteln!(uart, "spiDone: {}", si539x_spi.spi_done()).unwrap();

    // Compare the programmed clock (200 MHz) against the system clock (125 MHz)
    // using a domain difference counter. After 1 second the domain difference
    // counter is expected to be around 75M.
    for i in 0..10_u8 {
        if unsafe { domain_diff_counters.counters_active_unchecked(0) } {
            uwriteln!(uart, "Counter 0 active after {} retries", i).unwrap();
            break;
        }
        if !unsafe { domain_diff_counters.enable_unchecked(0) } {
            uwriteln!(uart, "Counter 0 is disabled, enabling...").unwrap();
            unsafe {
                domain_diff_counters.set_enable_unchecked(0, true);
            }
        }
        uwriteln!(uart, "Retry {}: Counter 0 is inactive", i).unwrap();
        timer.wait(Duration::from_millis(100));
    }
    let result_a = check_frequency(&timer, &mut uart, &domain_diff_counters, 75_000_000);

    match result_a {
        Ok(()) => uwriteln!(uart, "Test passed").unwrap(),
        _ => uwriteln!(uart, "Test failed").unwrap(),
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
