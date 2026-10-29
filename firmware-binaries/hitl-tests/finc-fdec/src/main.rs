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
use bittide_hal::manual_additions::clock_chip::{ClockChip, Config, WriteError};
use bittide_hal::manual_additions::timer::Duration;
use bittide_hal::shared_devices::{Si5345I2c, Timer, Uart};

use bittide_cpus::boot::start_fmc_clock;
use bittide_macros::load_clock_config_csv;

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };
const CONFIG_125: Config<3, 524, 5> =
    load_clock_config_csv!("../../../bittide/data/clock_configs/Si5345-125MHz-10ppb-Registers.csv");

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

// FINC/FDEC test patterns. Each loop iteration sends one I2C request.

fn do_sw_fdec(si5345_i2c: &Si5345I2c, timer: &Timer, dc: &DomainDiffCounters) -> TestResult {
    counter_test(
        dc,
        || si5345_i2c.fdec(timer, 1),
        (i32::lt, -THRESHOLD),
        (i32::gt, THRESHOLD),
        Direction::Fdec,
    )
}

fn do_sw_finc(si5345_i2c: &Si5345I2c, timer: &Timer, dc: &DomainDiffCounters) -> TestResult {
    counter_test(
        dc,
        || si5345_i2c.finc(timer, 1),
        (i32::gt, THRESHOLD),
        (i32::lt, -THRESHOLD),
        Direction::Finc,
    )
}

fn do_sw_fdec_inc(si5345_i2c: &Si5345I2c, timer: &Timer, dc: &DomainDiffCounters) -> TestResult {
    counter_test(
        dc,
        || si5345_i2c.fdec(timer, 1),
        (i32::lt, -THRESHOLD),
        (i32::gt, THRESHOLD),
        Direction::Fdec,
    )?;

    counter_test(
        dc,
        || si5345_i2c.finc(timer, 1),
        (i32::gt, 0),
        (i32::lt, -(3 * THRESHOLD)),
        Direction::Finc,
    )
}

fn do_sw_finc_dec(si5345_i2c: &Si5345I2c, timer: &Timer, dc: &DomainDiffCounters) -> TestResult {
    counter_test(
        dc,
        || si5345_i2c.finc(timer, 1),
        (i32::gt, THRESHOLD),
        (i32::lt, -THRESHOLD),
        Direction::Finc,
    )?;

    counter_test(
        dc,
        || si5345_i2c.fdec(timer, 1),
        (i32::lt, 0),
        (i32::gt, (3 * THRESHOLD)),
        Direction::Fdec,
    )
}

/// Print the debug registers of the clock chip interface.
fn print_i2c_status(si5345_i2c: &Si5345I2c, uart: &mut Uart) {
    let bus_state = si5345_i2c.bus_state().into_inner()[0];
    uwriteln!(
        uart,
        "  nack_count={} arbitration_lost_count={} scl={} sda={} busy={} step={}",
        si5345_i2c.nack_count().into_inner(),
        si5345_i2c.arbitration_lost_count().into_inner(),
        (bus_state >> 7) & 1,
        (bus_state >> 6) & 1,
        (bus_state >> 5) & 1,
        bus_state & 0xF,
    )
    .unwrap();
}

/// Try to read the `DEVICE_READY` register of the clock chip a few times, with a
/// timeout. Prints the result, or the state of the I2C interface on a timeout.
fn probe_clock_chip(si5345_i2c: &Si5345I2c, timer: &Timer, uart: &mut Uart) {
    uwriteln!(uart, "Probing clock chip").unwrap();
    print_i2c_status(si5345_i2c, uart);
    for attempt in 0..5 {
        si5345_i2c.start_read(0x00, 0xFE);
        let deadline = timer.now() + Duration::from_millis(100);
        while si5345_i2c.is_busy() && timer.now() < deadline {
            continue;
        }
        if si5345_i2c.is_busy() {
            uwriteln!(uart, "Probe {}: timeout", attempt).unwrap();
            print_i2c_status(si5345_i2c, uart);
            // Reset the driver so the next attempt starts from scratch
            si5345_i2c.set_reset(true);
            si5345_i2c.set_reset(false);
        } else {
            uwriteln!(
                uart,
                "Probe {}: DEVICE_READY=0x{:02X}",
                attempt,
                si5345_i2c.read_byte()
            )
            .unwrap();
            print_i2c_status(si5345_i2c, uart);
        }
    }
}

/// Run a single test with the full per-test sequence:
///
///  1. Program clock chip
///  2. Enable domain diff counter
///  3. Execute test body (FINC/FDEC until threshold)
///  5. Disable domain diff counter
///  4. Print result
///
/// Returns whether the test passed.
fn run_test<Test>(
    name: &str,
    si5345_i2c: &Si5345I2c,
    timer: &Timer,
    uart: &mut Uart,
    dc: &DomainDiffCounters,
    test_body: Test,
) -> bool
where
    Test: FnOnce(&DomainDiffCounters) -> TestResult,
{
    uwriteln!(uart, "{}: waiting for clock chip to be ready", name).unwrap();
    si5345_i2c.wait_for_ready();
    let design_id = si5345_i2c.read_design_id();
    uwriteln!(
        uart,
        "{}: clock chip ready, design ID {:?}",
        name,
        design_id
    )
    .unwrap();

    uwriteln!(uart, "{}: writing clock chip configuration", name).unwrap();
    if let Err(WriteError::NotConfirmed { entry, read_data }) =
        si5345_i2c.write_configuration_with_retry(timer, &CONFIG_125, MAX_CONFIG_RETRIES)
    {
        uwriteln!(
            uart,
            "FAIL: {} (clock chip at 0x{:02X}{:02X}: wrote 0x{:02X}, read 0x{:02X})",
            name,
            entry.page,
            entry.address,
            entry.data,
            read_data,
        )
        .unwrap();
        return false;
    }
    uwriteln!(uart, "{}: clock chip configured and locked", name).unwrap();

    uwriteln!(uart, "{}: waiting for domain diff counter", name).unwrap();
    dc.set_enable(0, true);
    loop {
        if dc.counters_active(0) == Some(true) {
            break;
        }
    }
    uwriteln!(uart, "{}: domain diff counter active, running test", name).unwrap();

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
        clock_chip: $clock_chip:ident,
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
                &$clock_chip,
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
    let si5345_i2c = INSTANCES.si5345_i2c;
    let timer = INSTANCES.timer;
    let mut uart = INSTANCES.uart;
    let dc = INSTANCES.domain_diff_counters;
    let main_i2c = INSTANCES.i2c_controller;

    // Each test below configures the clock chip again, from a known state
    let started = match start_fmc_clock(
        &main_i2c,
        &si5345_i2c,
        &timer,
        &mut uart,
        &CONFIG_125,
        MAX_CONFIG_RETRIES,
    ) {
        Ok(()) => {
            uwriteln!(uart, "PASS: clock chip started").unwrap();
            true
        }
        Err(e) => {
            uwriteln!(uart, "FAIL: clock chip not started ({:?})", e).unwrap();
            false
        }
    };

    let all_passed = started && {
        uwriteln!(uart, "Starting FINC/FDEC tests").unwrap();
        probe_clock_chip(&si5345_i2c, &timer, &mut uart);

        run_tests!(
            clock_chip: si5345_i2c,
        timer: timer,
        uart: uart,
        dc: dc,
        tests: [
            [name: "Software FDec",    test: |dc| do_sw_fdec(&si5345_i2c, &timer, dc)],
            [name: "Software FInc",    test: |dc| do_sw_finc(&si5345_i2c, &timer, dc)],
            [name: "Software FDecInc", test: |dc| do_sw_fdec_inc(&si5345_i2c, &timer, dc)],
            [name: "Software FIncDec", test: |dc| do_sw_finc_dec(&si5345_i2c, &timer, dc)],
        ],
        )
    };

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
