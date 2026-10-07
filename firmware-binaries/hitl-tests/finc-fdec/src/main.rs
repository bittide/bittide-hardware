#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;
use ufmt::uDisplay;
use ufmt::uWrite;
use ufmt::uwrite;
use ufmt::uwriteln;

use bittide_hal::hals::finc_fdec_tests::devices::DomainDiffCounters;
use bittide_hal::hals::finc_fdec_tests::DeviceInstances;
use bittide_hal::manual_additions::si539x_spi::{Config, WriteError};
use bittide_hal::shared_devices::{Si539xSpi, Timer, Uart};

use bittide_macros::load_clock_config_csv;

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };
const CONFIG_125: Config<3, 590, 5> = load_clock_config_csv!(
    "../../../bittide/data/clock_configs/Si5395J-125MHz-10ppb-Registers.csv"
);

const MAX_CONFIG_RETRIES: usize = 10;

const THRESHOLD: i32 = 20_000;

#[derive(Copy, Clone)]
enum Direction {
    Finc,
    Fdec,
}

impl uDisplay for Direction {
    fn fmt<W>(&self, f: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: uWrite + ?Sized,
    {
        match self {
            Direction::Fdec => uwrite!(f, "Fdec"),
            Direction::Finc => uwrite!(f, "Finc"),
        }
    }
}

type TestResult = Result<(), Direction>;

fn read_counter(dc: &DomainDiffCounters) -> i32 {
    dc.counters(0).unwrap().into_inner()
}

fn counter_test<Pass, Fail, SpeedChangeFn>(
    dc: &DomainDiffCounters,
    mut apply_speed_change: SpeedChangeFn,
    (mut pass_test, pass): (Pass, i32),
    (mut fail_test, fail): (Fail, i32),
    direction: Direction,
) -> TestResult
where
    Pass: FnMut(&i32, &i32) -> bool,
    Fail: FnMut(&i32, &i32) -> bool,
    SpeedChangeFn: FnMut(),
{
    loop {
        apply_speed_change();
        let count = read_counter(dc);
        if pass_test(&count, &pass) {
            return Ok(());
        }
        if fail_test(&count, &fail) {
            return Err(direction);
        }
    }
}

// FINC/FDEC test patterns. Each loop iteration sends one SPI request.

fn do_sw_fdec(si539x_spi: &Si539xSpi, timer: &Timer, dc: &DomainDiffCounters) -> TestResult {
    counter_test(
        dc,
        || si539x_spi.fdec(timer, 1),
        (i32::lt, -THRESHOLD),
        (i32::gt, THRESHOLD),
        Direction::Fdec,
    )
}

fn do_sw_finc(si539x_spi: &Si539xSpi, timer: &Timer, dc: &DomainDiffCounters) -> TestResult {
    counter_test(
        dc,
        || si539x_spi.finc(timer, 1),
        (i32::gt, THRESHOLD),
        (i32::lt, -THRESHOLD),
        Direction::Finc,
    )
}

fn do_sw_fdec_inc(si539x_spi: &Si539xSpi, timer: &Timer, dc: &DomainDiffCounters) -> TestResult {
    counter_test(
        dc,
        || si539x_spi.fdec(timer, 1),
        (i32::lt, -THRESHOLD),
        (i32::gt, THRESHOLD),
        Direction::Fdec,
    )?;

    counter_test(
        dc,
        || si539x_spi.finc(timer, 1),
        (i32::gt, 0),
        (i32::lt, -(3 * THRESHOLD)),
        Direction::Finc,
    )
}

fn do_sw_finc_dec(si539x_spi: &Si539xSpi, timer: &Timer, dc: &DomainDiffCounters) -> TestResult {
    counter_test(
        dc,
        || si539x_spi.finc(timer, 1),
        (i32::gt, THRESHOLD),
        (i32::lt, -THRESHOLD),
        Direction::Finc,
    )?;

    counter_test(
        dc,
        || si539x_spi.fdec(timer, 1),
        (i32::lt, 0),
        (i32::gt, (3 * THRESHOLD)),
        Direction::Fdec,
    )
}

/// Run a single test with the full per-test sequence:
///
///  1. Program clock board
///  2. Enable domain diff counter
///  3. Execute test body (FINC/FDEC until threshold)
///  5. Disable domain diff counter
///  4. Print result
///
/// Returns whether the test passed.
fn run_test<Test>(
    name: &str,
    si539x_spi: &Si539xSpi,
    timer: &Timer,
    uart: &mut Uart,
    dc: &DomainDiffCounters,
    test_body: Test,
) -> bool
where
    Test: FnOnce(&DomainDiffCounters) -> TestResult,
{
    if let Err(WriteError::NotConfirmed { entry, read_data }) =
        si539x_spi.write_configuration_with_retry(timer, &CONFIG_125, MAX_CONFIG_RETRIES)
    {
        uwriteln!(
            uart,
            "FAIL: {} (clock board at 0x{:02X}{:02X}: wrote 0x{:02X}, read 0x{:02X})",
            name,
            entry.page,
            entry.address,
            entry.data,
            read_data,
        )
        .unwrap();
        return false;
    }

    dc.set_enable(0, true);
    loop {
        if dc.counters_active(0) == Some(true) {
            break;
        }
    }

    let test_result = test_body(dc);
    dc.set_enable(0, false);

    match test_result {
        Ok(()) => uwriteln!(uart, "PASS: {}", name).unwrap(),
        Err(dir) => uwriteln!(uart, "FAIL: {} on part {}", name, dir).unwrap(),
    }

    test_result.is_ok()
}

macro_rules! run_tests {
    (
        spi: $spi:ident,
        timer: $timer:ident,
        uart: $uart:ident,
        dc: $dc:ident,
        tests: [
            $([name: $name:literal, test: $test:expr]),+
            $(,)?
        ],
    ) => {
        $(
            run_test(
                $name,
                &$spi,
                &$timer,
                &mut $uart,
                &$dc,
                $test,
            )
        )&&+
    };
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let si539x_spi = INSTANCES.si539x_spi;
    let timer = INSTANCES.timer;
    let mut uart = INSTANCES.uart;
    let dc = INSTANCES.domain_diff_counters;

    let all_passed = run_tests!(
        spi: si539x_spi,
        timer: timer,
        uart: uart,
        dc: dc,
        tests: [
            [name: "Software FDec",    test: |dc| do_sw_fdec(&si539x_spi, &timer, dc)],
            [name: "Software FInc",    test: |dc| do_sw_finc(&si539x_spi, &timer, dc)],
            [name: "Software FDecInc", test: |dc| do_sw_fdec_inc(&si539x_spi, &timer, dc)],
            [name: "Software FIncDec", test: |dc| do_sw_finc_dec(&si539x_spi, &timer, dc)],
        ],
    );

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
