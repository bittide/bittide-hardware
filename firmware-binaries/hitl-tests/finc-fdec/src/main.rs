#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;
use ufmt::uwriteln;

use bittide_hal::hals::finc_fdec_tests::devices::DomainDiffCounters;
use bittide_hal::hals::finc_fdec_tests::DeviceInstances;
use bittide_hal::manual_additions::si539x_spi::{Config, WriteError};
use bittide_hal::shared_devices::{HardwareSpeedChange, Si539xSpi, Timer, Uart};
use bittide_hal::types::SpeedChange;

use bittide_macros::load_clock_config_csv;

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };
const CONFIG_125: Config<3, 590, 5> = load_clock_config_csv!(
    "../../../bittide/data/clock_configs/Si5395J-125MHz-10ppb-Registers.csv"
);

const THRESHOLD: i32 = 20_000;

fn wait_for_counter_active(dc: &DomainDiffCounters) {
    loop {
        if dc.counters_active(0) == Some(true) {
            break;
        }
    }
}

fn read_counter(dc: &DomainDiffCounters) -> i32 {
    dc.counters(0).unwrap().into_inner()
}

// FINC/FDEC test patterns, split into hardware and software variants.
// Hardware: set the SpeedChange register once; it continuously pulses the pins.
// Software: send one SPI request per loop iteration.
// All return true on success, false on failure.

fn do_fdec_hw(sc: &HardwareSpeedChange, dc: &DomainDiffCounters) -> bool {
    sc.set_speed_change(SpeedChange::SlowDown);
    let result = loop {
        let count = read_counter(dc);
        if count < -THRESHOLD {
            break true;
        }
        if count > THRESHOLD {
            break false;
        }
    };
    sc.set_speed_change(SpeedChange::NoChange);
    result
}

fn do_finc_hw(sc: &HardwareSpeedChange, dc: &DomainDiffCounters) -> bool {
    sc.set_speed_change(SpeedChange::SpeedUp);
    let result = loop {
        let count = read_counter(dc);
        if count > THRESHOLD {
            break true;
        }
        if count < -THRESHOLD {
            break false;
        }
    };
    sc.set_speed_change(SpeedChange::NoChange);
    result
}

fn do_fdec_finc_hw(sc: &HardwareSpeedChange, dc: &DomainDiffCounters) -> bool {
    sc.set_speed_change(SpeedChange::SlowDown);
    loop {
        let count = read_counter(dc);
        if count < -THRESHOLD {
            break;
        }
        if count > THRESHOLD {
            sc.set_speed_change(SpeedChange::NoChange);
            return false;
        }
    }
    sc.set_speed_change(SpeedChange::SpeedUp);
    let result = loop {
        let count = read_counter(dc);
        if count > 0 {
            break true;
        }
        if count < -(3 * THRESHOLD) {
            break false;
        }
    };
    sc.set_speed_change(SpeedChange::NoChange);
    result
}

fn do_finc_fdec_hw(sc: &HardwareSpeedChange, dc: &DomainDiffCounters) -> bool {
    sc.set_speed_change(SpeedChange::SpeedUp);
    loop {
        let count = read_counter(dc);
        if count > THRESHOLD {
            break;
        }
        if count < -THRESHOLD {
            sc.set_speed_change(SpeedChange::NoChange);
            return false;
        }
    }
    sc.set_speed_change(SpeedChange::SlowDown);
    let result = loop {
        let count = read_counter(dc);
        if count < 0 {
            break true;
        }
        if count > 3 * THRESHOLD {
            break false;
        }
    };
    sc.set_speed_change(SpeedChange::NoChange);
    result
}

fn do_fdec_sw(si539x_spi: &Si539xSpi, timer: &Timer, dc: &DomainDiffCounters) -> bool {
    loop {
        si539x_spi.fdec(timer, 1);
        let count = read_counter(dc);
        if count < -THRESHOLD {
            return true;
        }
        if count > THRESHOLD {
            return false;
        }
    }
}

fn do_finc_sw(si539x_spi: &Si539xSpi, timer: &Timer, dc: &DomainDiffCounters) -> bool {
    loop {
        si539x_spi.finc(timer, 1);
        let count = read_counter(dc);
        if count > THRESHOLD {
            return true;
        }
        if count < -THRESHOLD {
            return false;
        }
    }
}

fn do_fdec_finc_sw(si539x_spi: &Si539xSpi, timer: &Timer, dc: &DomainDiffCounters) -> bool {
    loop {
        si539x_spi.fdec(timer, 1);
        let count = read_counter(dc);
        if count < -THRESHOLD {
            break;
        }
        if count > THRESHOLD {
            return false;
        }
    }
    loop {
        si539x_spi.finc(timer, 1);
        let count = read_counter(dc);
        if count > 0 {
            return true;
        }
        if count < -(3 * THRESHOLD) {
            return false;
        }
    }
}

fn do_finc_fdec_sw(si539x_spi: &Si539xSpi, timer: &Timer, dc: &DomainDiffCounters) -> bool {
    loop {
        si539x_spi.finc(timer, 1);
        let count = read_counter(dc);
        if count > THRESHOLD {
            break;
        }
        if count < -THRESHOLD {
            return false;
        }
    }
    loop {
        si539x_spi.fdec(timer, 1);
        let count = read_counter(dc);
        if count < 0 {
            return true;
        }
        if count > 3 * THRESHOLD {
            return false;
        }
    }
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
fn run_test(
    name: &str,
    si539x_spi: &Si539xSpi,
    timer: &Timer,
    uart: &mut Uart,
    dc: &DomainDiffCounters,
    test_body: &dyn Fn(&DomainDiffCounters) -> bool,
) -> bool {
    if let Err(WriteError::NotConfirmed { entry, read_data }) =
        si539x_spi.write_configuration(timer, &CONFIG_125)
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
    wait_for_counter_active(dc);

    let passed = test_body(dc);
    dc.set_enable(0, false);

    if passed {
        uwriteln!(uart, "PASS: {}", name).unwrap();
    } else {
        uwriteln!(uart, "FAIL: {}", name).unwrap();
    }

    passed
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let si539x_spi = INSTANCES.si539x_spi;
    let timer = INSTANCES.timer;
    let mut uart = INSTANCES.uart;
    let dc = INSTANCES.domain_diff_counters;
    let sc = INSTANCES.hardware_speed_change;

    let mut all_passed = true;

    all_passed &= run_test(
        "FDec (hardware)",
        &si539x_spi,
        &timer,
        &mut uart,
        &dc,
        &|dc| do_fdec_hw(&sc, dc),
    );
    all_passed &= run_test(
        "FInc (hardware)",
        &si539x_spi,
        &timer,
        &mut uart,
        &dc,
        &|dc| do_finc_hw(&sc, dc),
    );
    all_passed &= run_test(
        "FDecInc (hardware)",
        &si539x_spi,
        &timer,
        &mut uart,
        &dc,
        &|dc| do_fdec_finc_hw(&sc, dc),
    );
    all_passed &= run_test(
        "FIncDec (hardware)",
        &si539x_spi,
        &timer,
        &mut uart,
        &dc,
        &|dc| do_finc_fdec_hw(&sc, dc),
    );
    all_passed &= run_test(
        "FDec (software)",
        &si539x_spi,
        &timer,
        &mut uart,
        &dc,
        &|dc| do_fdec_sw(&si539x_spi, &timer, dc),
    );
    all_passed &= run_test(
        "FInc (software)",
        &si539x_spi,
        &timer,
        &mut uart,
        &dc,
        &|dc| do_finc_sw(&si539x_spi, &timer, dc),
    );
    all_passed &= run_test(
        "FDecInc (software)",
        &si539x_spi,
        &timer,
        &mut uart,
        &dc,
        &|dc| do_fdec_finc_sw(&si539x_spi, &timer, dc),
    );
    all_passed &= run_test(
        "FIncDec (software)",
        &si539x_spi,
        &timer,
        &mut uart,
        &dc,
        &|dc| do_finc_fdec_sw(&si539x_spi, &timer, dc),
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
