#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;

use bittide_hal::shared::types::speed_change::SpeedChange;
use bittide_hal::{hals::sw_cc_topologies::DeviceInstances, manual_additions::timer::Duration};
use bittide_sys::{
    callisto::{self, ControlConfig, ControlSt},
    stability_detector::StabilityDetector,
};

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let cc = INSTANCES.clock_control;
    let timer = INSTANCES.timer;

    let config = ControlConfig {
        wait_time: 0,
        reframing_enabled: false,
        k_p: 2e-8,
    };

    let mut state = ControlSt::new(
        0,
        SpeedChange::NoChange,
        0.0f32,
    );

    // Initialize stability detector
    let mut stability_detector = StabilityDetector::new(25, Duration::from_secs(2));

    loop {
        callisto::callisto(&cc, cc.data_counts_volatile_iter(), &config, &mut state);
        cc.set_change_speed(state.b_k);
        stability_detector.update(&cc, timer.now());
    }
}

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    loop {
        continue;
    }
}
