#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;

use bittide_hal::shared::types::speed_change::SpeedChange;
use bittide_hal::switch_demo_cc::DeviceInstances;
use bittide_hal::{
    manual_additions::timer::Duration, shared::types::reframing_state::ReframingState,
};
use bittide_sys::sample_store::SampleStore;
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

    let config = ControlConfig {
        target_count: 0,
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

    // Update clock control 50K updates per second
    let interval = Duration::from_micros(20);
    let mut next_update = timer.now() + interval;

    // Store samples every _n_ updates. Currently set to 50 times a second
    // (50K / 1000 = 50Hz). Set to '1' for perfect storage -- not yet possible
    // due to limited memory size.
    let mut sample_store = SampleStore::new(sample_memory, 1000);

    loop {
        freeze.set_freeze(true);
        callisto::callisto(&cc, freeze.eb_counters_volatile_iter(), &config, &mut state);
        cc.set_change_speed(state.b_k);
        sample_store.store(&freeze);
        timer.wait_until(next_update);
        next_update += interval;
    }
}

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    loop {
        continue;
    }
}
