#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;
use ufmt::derive::uDebug;
use ufmt::uwriteln;

use bittide_hal::manual_additions::si539x_spi::{Config, WriteError};
use bittide_hal::manual_additions::timer::Duration;
use bittide_hal::shared::devices::Timer;
use bittide_hal::si539x_configuration::DeviceInstances;
use bittide_hal::si539x_configuration::DomainDiffCounters;

use bittide_macros::load_clock_config_csv;

#[cfg(not(test))]
use riscv_rt::entry;

/// Errors that can occur during frequency verification.
#[derive(uDebug, Clone, Copy)]
enum FrequencyCheckError {
    /// The domain difference counter is not active.
    CounterInactive,
    /// The domain difference counter cannot be accessed.
    CounterInaccessible,
    /// The measured frequency is outside the acceptable tolerance.
    UnexpectedFrequency { expected: u32, measured: u32 },
}

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };
const CONFIG_200: Config<3, 584, 5> =
    load_clock_config_csv!("../../../bittide/data/clock_configs/Si5395J-200MHz-10ppb-and-out1.csv");

/// System clock frequency in Hz (125 MHz).
const SYS_FREQ: u32 = 125_000_000;

/// Acceptable frequency tolerance in Hz (±100 kHz).
const TOLERANCE: i32 = 100_000;

/// Check if the programmed clock runs at the expected frequency.
///
/// Uses the domain difference counter to verify the programmed clock runs at
/// the expected frequency by comparing it to the system clock.
fn check_frequency(
    timer: &Timer,
    domain_diff_counters: &DomainDiffCounters,
    expected_freq: u32,
) -> Result<(), FrequencyCheckError> {
    domain_diff_counters.set_enable(0, true);
    // Wait briefly for counter to become active
    timer.wait(Duration::from_micros(10));

    // Verify the counter is accessible and active. When inactive, it was either
    // not enabled correctly or one of the 2 clocks is not running.
    match domain_diff_counters.counters_active(0) {
        None => return Err(FrequencyCheckError::CounterInaccessible),
        Some(false) => return Err(FrequencyCheckError::CounterInactive),
        Some(true) => (),
    }

    // Measure counter values over a 1-second period
    let start = domain_diff_counters.counters(0);
    timer.wait(Duration::from_secs(1));
    let end = domain_diff_counters.counters(0);
    domain_diff_counters.set_enable(0, false);

    let diff = match (start, end) {
        (Some(s), Some(e)) => e - s,
        _ => return Err(FrequencyCheckError::CounterInaccessible),
    };

    // Calculate the expected value after 1 second of running the domain
    // difference counter.
    let expected_diff = expected_freq as i32 - SYS_FREQ as i32;

    // Check if measured difference is within acceptable tolerance
    if (diff - expected_diff).abs() > TOLERANCE {
        let calculated_freq = (SYS_FREQ as i32 + diff) as u32;
        Err(FrequencyCheckError::UnexpectedFrequency {
            expected: expected_freq,
            measured: calculated_freq,
        })
    } else {
        Ok(())
    }
}

/// Main entry point for the clock board firmware.
///
/// This function initializes the hardware, programs the Si5395J clock chip
/// with a 200 MHz configuration, and verifies the output frequency.
#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let si539x_spi = INSTANCES.si539x_spi;
    let timer = INSTANCES.timer;
    let mut uart = INSTANCES.uart;
    let domain_diff_counters = INSTANCES.domain_diff_counters;

    let mut all_passed = true;

    // ========================================================================
    // Test 1: Modify and read back DESIGN_ID registers
    // Check if we can write to the Si539x clock chip by writing to the
    // DESIGN_ID registers and reading them back.
    // ========================================================================
    {
        uwriteln!(uart, "Test 1: Modify and read back DESIGN_ID registers").unwrap();
        let old_design_id = si539x_spi.read_design_id();
        let mut reverse_design_id = old_design_id;
        reverse_design_id.reverse();
        si539x_spi.write_design_id(reverse_design_id);
        let read_back_design_id = si539x_spi.read_design_id();

        // Verify write succeeded by checking if read-back matches what we wrote
        if read_back_design_id != reverse_design_id {
            all_passed = false;
            uwriteln!(
                uart,
                "  FAIL: DESIGN_ID was incorrecly written or read back"
            )
            .unwrap();
            uwriteln!(uart, "  Tried to write: {:?}", reverse_design_id).unwrap();
            uwriteln!(uart, "  Read back:      {:?}", read_back_design_id).unwrap();
        } else {
            uwriteln!(uart, "  PASS").unwrap();
        }
    }

    // ========================================================================
    // Test 2: Program the Si5395J clock chip with the 200 MHz configuration
    // ========================================================================
    {
        uwriteln!(uart, "Test 2: Write clock configuration").unwrap();
        if let Err(WriteError::NotConfirmed { entry, read_data }) =
            si539x_spi.write_configuration(&timer, &CONFIG_200)
        {
            all_passed = false;
            uwriteln!(
                uart,
                "  FAIL: At 0x{:02X}{:02X} wrote 0x{:02X}, but read back 0x{:02X}",
                entry.page,
                entry.address,
                entry.data,
                read_data,
            )
            .unwrap();
        }

        // Verify the programmed frequency is correct (200 MHz)
        let result = check_frequency(&timer, &domain_diff_counters, 200e6 as u32);
        match result {
            Ok(()) => uwriteln!(uart, "  PASS").unwrap(),
            Err(FrequencyCheckError::CounterInactive) => {
                all_passed = false;
                uwriteln!(uart, "  FAIL: Domain difference counter is inactive").unwrap();
            }
            Err(FrequencyCheckError::CounterInaccessible) => {
                all_passed = false;
                uwriteln!(uart, "  FAIL: Domain difference counter is inaccessible").unwrap();
            }
            Err(FrequencyCheckError::UnexpectedFrequency { expected, measured }) => {
                all_passed = false;
                uwriteln!(
                    uart,
                    " FAIL: Unexpected frequency, got: {}, expected {} ± {}",
                    measured,
                    expected,
                    TOLERANCE
                )
                .unwrap();
            }
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
