// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use ufmt::uwriteln;

use bittide_hal::{
    hals::watchdog_test::DeviceInstances,
    manual_additions::timer::{Duration, Instant, WaitResult},
};

#[cfg(not(test))]
use riscv_rt::entry;

const DEVICES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals.
    let mut uart = DEVICES.uart;
    let timer = DEVICES.timer;

    // Align to start of a whole microsecond
    let wait_result = timer.wait_until(timer.now() + Duration::from_micros(10));
    let t0 = timer.now();
    DEVICES.idle_a.set_idle(());
    let t1: Instant = timer.now();
    let diff_a = t1 - t0;

    if wait_result == WaitResult::AlreadyPassed {
        uwriteln!(uart, "[ERROR] wait already passed").unwrap();
        loop {
            continue;
        }
    }

    // Align to start of a whole microsecond
    let wait_result = timer.wait_until(timer.now() + Duration::from_micros(10));
    let t0 = timer.now();
    DEVICES.idle_b.set_idle(());
    let t1: Instant = timer.now();
    let diff_b = t1 - t0;

    // Throw error if we missed our wait deadline
    if wait_result == WaitResult::AlreadyPassed {
        uwriteln!(uart, "[ERROR] wait already passed").unwrap();
        loop {
            continue;
        }
    }

    // We consider the 1 cycle timeout to be "instant" since our timing peripheral stores
    // time as microseconds, so subtracting these durations should compensate for the
    // overheads induced by talking to the timer peripheral and we can expect
    // subtracting them to yield the 50us delay induced by the second watchdog timeout.
    uwriteln!(
        uart,
        "Timeout took {} microseconds",
        (diff_b - diff_a).micros()
    )
    .unwrap();
    loop {
        continue;
    }
}

#[panic_handler]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    loop {
        continue;
    }
}
