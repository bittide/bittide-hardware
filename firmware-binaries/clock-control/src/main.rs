#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;

use bittide_hal::shared::types::speed_change::SpeedChange;
use bittide_hal::switch_demo_cc::DeviceInstances as SwitchDemoCcDeviceInstances;
use bittide_hal::{
    manual_additions::timer::Duration, shared::types::reframing_state::ReframingState,
};

use bittide_sys::callisto::{self, ControlConfig, ControlSt};
#[cfg(not(test))]
use riscv_rt::entry;

// TODO: This binary is reused for both the switch demo and the topology instances. This
//       should be merged into a single instance, such that we can also just use one
//       binary (safely). It now _happens_ to work, because both instances are careful
//       to put the same devices in the same places.
const INSTANCES: SwitchDemoCcDeviceInstances = unsafe { SwitchDemoCcDeviceInstances::new() };

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let cc = INSTANCES.clock_control;
    let dbgreg = INSTANCES.debug_register;
    let timer = INSTANCES.timer;

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
