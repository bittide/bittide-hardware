#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;

use bittide_sys::{
    callisto::{self, ControlConfig, ControlSt, ReframingState},
    clock_control::{ClockControl, SpeedChange},
};
#[cfg(not(test))]
use riscv_rt::entry;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    #[allow(clippy::zero_ptr)] // we might want to change the address!
    let mut cc = unsafe { ClockControl::from_base_addr(0xC000_0000 as *const u32) };

    let config = ControlConfig {
        target_count: 0,
        wait_time: 0,
        reframing_enabled: 0,
    };
    let mut state = ControlSt {
        z_k: 0,
        b_k: SpeedChange::NoChange,
        steady_state_target: 0.0f32,
        rf_state: ReframingState::Detect,
    };

    loop {
        let data_counts = cc.data_counts().map(|x| x as isize);
        let links_stable = cc.links_stable();

        callisto::callisto(&config, 0b0111_1111, links_stable, data_counts, &mut state);

        cc.change_speed(state.b_k);
    }
}

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    loop {
        continue;
    }
}
