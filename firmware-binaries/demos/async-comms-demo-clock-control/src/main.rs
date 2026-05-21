#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;

use bittide_hal::async_comms_demo_clock_control::DeviceInstances;

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    bittide_cpus::clock_control::run(
        INSTANCES.clock_control,
        INSTANCES.timer,
        &mut uart,
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
