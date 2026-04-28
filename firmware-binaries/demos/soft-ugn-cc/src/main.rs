#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;

use bittide_hal::hals::soft_ugn_demo_cc::devices::{ClockControl, Freeze};
use bittide_hal::soft_ugn_demo_cc::DeviceInstances;
use bittide_sys::clock_control_main::clock_control_main;

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    clock_control_main::<{ Freeze::EB_COUNTERS_LEN }, { ClockControl::DATA_COUNTS_LEN }, _, _, _>(
        INSTANCES.clock_control,
        INSTANCES.timer,
        INSTANCES.uart,
        INSTANCES.freeze,
        INSTANCES.sample_memory,
        INSTANCES.sync_out_generator,
        INSTANCES.domain_diff_counters,
    )
}

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    loop {
        continue;
    }
}
