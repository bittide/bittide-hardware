#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;

use bittide_cpus::clock_control::StableSampling;
use bittide_hal::soft_ugn_demo_clock_control::DeviceInstances;

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

/// Keep storing samples after all links have become stable, so we can observe
/// the system over long runs. A sample is stored every second (5000 updates of
/// 200 us each). Reserving 2000 samples covers a little over 33 minutes, leaving
/// room for ~15 seconds worth of samples before links become stable.
const STABLE_SAMPLING: StableSampling = StableSampling {
    store_samples_every: 5_000,
    reserved_samples: 2_000,
};

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    bittide_cpus::clock_control::run(
        INSTANCES.clock_control,
        INSTANCES.si539x_spi,
        INSTANCES.timer,
        &mut uart,
        INSTANCES.freeze,
        INSTANCES.sample_memory,
        INSTANCES.sync_out_generator,
        INSTANCES.domain_diff_counters,
        Some(STABLE_SAMPLING),
    )
}

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    loop {
        continue;
    }
}
