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

    uwriteln!(uart, "Starting clock control..").unwrap();

    let config = ControlConfig {
        target_count: 0,
        wait_time: 0,
        reframing_enabled: dbgreg.reframing_enabled(),
    };
    let mut state = ControlSt::new(
        0,
        SpeedChange::NoChange,
        0.0f32,
        dbgreg,
        ReframingState::Detect,
    );

    // Update clock control 10K updates per second
    let interval = Duration::from_micros(100);
    let mut next_update = timer.now() + interval;

    loop {
        callisto::callisto(&cc, &config, &mut state);
        cc.set_change_speed(state.b_k);
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
