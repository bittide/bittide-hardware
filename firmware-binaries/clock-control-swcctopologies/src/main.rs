#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;

use bittide_hal::hals::sw_cc_topologies::DeviceInstances;
use bittide_hal::shared::types::reframing_state::ReframingState;
use bittide_hal::shared::types::speed_change::SpeedChange;
use bittide_sys::callisto::{self, ControlConfig, ControlSt};

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let cc = INSTANCES.clock_control;
    let dbgreg = INSTANCES.debug_register;

    let config = ControlConfig {
        wait_time: 0,
        reframing_enabled: dbgreg.reframing_enabled(),
        k_p: 2e-8,
    };

    let mut state = ControlSt::new(
        0,
        SpeedChange::NoChange,
        0.0f32,
        dbgreg,
        ReframingState::Detect,
    );

    loop {
        callisto::callisto(&cc, cc.data_counts_volatile_iter(), &config, &mut state);
        cc.set_change_speed(state.b_k);
    }
}

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    loop {
        continue;
    }
}
