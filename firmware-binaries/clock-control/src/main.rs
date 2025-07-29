#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;

use bittide_hal::manual_additions::timer::Instant;
use bittide_hal::manual_additions::timer::WaitResult;
use bittide_hal::shared::devices::Timer;
use bittide_hal::shared::devices::Uart;
use bittide_hal::shared::types::speed_change::SpeedChange;
use bittide_hal::switch_demo_cc::DeviceInstances;
use bittide_hal::{
    manual_additions::timer::Duration, shared::types::reframing_state::ReframingState,
};
use bittide_sys::sample_store::SampleStore;
use bittide_sys::stability_detector::StabilityDetector;
use ufmt::uwriteln;

use bittide_sys::callisto::{self, ControlConfig, ControlSt};
#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let cc = INSTANCES.clock_control;
    let dbgreg = INSTANCES.debug_register;
    let timer = INSTANCES.timer;
    let mut uart = INSTANCES.uart;
    let freeze = INSTANCES.freeze;
    let sample_memory = INSTANCES.sample_memory;
    let sync_out_generator = INSTANCES.sync_out_generator;

    uwriteln!(uart, "Starting sync out generator..").unwrap();
    sync_out_generator.set_active(true);

    uwriteln!(uart, "Starting clock control..").unwrap();
    uwriteln!(uart, "Current time: {}", timer.now()).unwrap();

    let config = ControlConfig {
        wait_time: 0,
        reframing_enabled: dbgreg.reframing_enabled(),
        k_p: 2e-9,
    };
    let mut state = ControlSt::new(
        0,
        SpeedChange::NoChange,
        0.0f32,
        dbgreg,
        ReframingState::Detect,
    );

    // Initialize stability detector
    let mut stability_detector = StabilityDetector::new(4, Duration::from_secs(2));

    // Store samples every _n_ updates. Currently set to 20 ms (50 Hz) times a
    // second (20 ms / 200 us = 100). Set to '1' for perfect storage -- not yet
    // possible due to limited memory size.
    let mut sample_store = SampleStore::new(sample_memory, 100);

    // Update clock control 5K updates per second
    let interval = Duration::from_micros(200);
    let mut next_update = timer.now() + interval;

    loop {
        // Do clock control on "frozen" counters
        freeze.set_freeze(true);
        callisto::callisto(&cc, freeze.eb_counters_volatile_iter(), &config, &mut state);
        cc.set_change_speed(state.b_k);

        // Detect stability
        let stability = stability_detector.update(&cc, timer.now());

        // Store debug information
        sample_store.store(&freeze, stability);

        // Wait for next update
        let timer_result = timer.wait_until(next_update);
        panic_on_missed_deadline(&mut uart, &timer, next_update, timer_result);
        next_update += interval;
    }
}

fn panic_on_missed_deadline(
    uart: &mut Uart,
    timer: &Timer,
    next_update: Instant,
    timer_result: WaitResult,
) {
    if let WaitResult::AlreadyPassed = timer_result {
        uwriteln!(
            uart,
            "Deadline missed! Timer did not update in time. Now: {}. Next update: {}",
            timer.now().micros(),
            next_update.micros(),
        )
        .unwrap();
        panic!("Deadline missed! See UART log.");
    };
}

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    loop {
        continue;
    }
}
