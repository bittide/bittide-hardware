#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2026 QBayLogic
//
// SPDX-License-Identifier: Apache-2.0

// Clock-control CPU for the Manticore demo: the generic Callisto clock control,
// identical to the other demos (independent of the user core).

use core::panic::PanicInfo;

use bittide_hal::hals::manticore_demo_clock_control::DeviceInstances;

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
