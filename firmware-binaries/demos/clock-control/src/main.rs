#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;
use itertools::izip;

use bittide_hal::manual_additions::timer::Duration;
use bittide_hal::manual_additions::timer::Instant;
use bittide_hal::manual_additions::timer::WaitResult;
use bittide_hal::shared_devices::Timer;
use bittide_hal::shared_devices::Uart;
use bittide_hal::switch_demo_cc::devices::DomainDiffCounters;
use bittide_hal::switch_demo_cc::DeviceInstances;
use bittide_sys::sample_store::SampleStore;
use bittide_sys::stability_detector::StabilityDetector;
use ufmt::uwriteln;

use bittide_sys::callisto::Callisto;
#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let cc = INSTANCES.clock_control;
    let timer = INSTANCES.timer;
    let mut uart = INSTANCES.uart;
    let freeze = INSTANCES.freeze;
    let sample_memory = INSTANCES.sample_memory;
    let sync_out_generator = INSTANCES.sync_out_generator;
    let domain_diff_counters = INSTANCES.domain_diff_counters;

    uwriteln!(uart, "Starting sync out generator..").unwrap();
    sync_out_generator.set_active(true);

    uwriteln!(uart, "Starting clock control..").unwrap();
    let mut callisto = Callisto::new(cc.config().callisto);

    // Initialize stability detector
    let mut stability_detector = StabilityDetector::new(4, Duration::from_secs(2));

    // Store samples every _n_ updates. Currently set to 20 ms (50 Hz) times a
    // second (20 ms / 200 us = 100). Set to '1' for perfect storage -- not yet
    // possible due to limited memory size.
    let mut sample_store = SampleStore::new(sample_memory, 100);

    // Update clock control 5K updates per second
    let interval = Duration::from_micros(200);
    let mut next_update = timer.now() + interval;
    let mut prev_all_stable = false;

    loop {
        // Store frozen elastic buffer counters
        freeze.set_freeze(());

        // Do clock control update
        cc.set_change_speed(
            callisto.update(
                &cc,
                izip!(
                    0..DomainDiffCounters::ENABLE_LEN,
                    freeze.eb_counters_volatile_iter()
                )
                .map(|(i, counter)| {
                    if domain_diff_counters.enable(i).unwrap_or(false) {
                        Some(counter)
                    } else {
                        None
                    }
                }),
            ),
        );

        // Detect stability
        let stability = stability_detector.update(&cc, timer.now());

        // Store debug information
        let has_sense_of_global_time = freeze.number_of_sync_pulses_seen() != 0;
        if has_sense_of_global_time {
            sample_store.store(&freeze, stability, callisto.accumulated_speed_requests);
        }

        // Emit stability information over UART
        let all_stable = stability.all_stable();
        if !prev_all_stable && all_stable {
            uwriteln!(uart, "All links stable").unwrap();
        } else if prev_all_stable && !all_stable {
            uwriteln!(uart, "Links no longer stable").unwrap();
            panic!("Links no longer stable");
        }
        prev_all_stable = all_stable;

        // Update active domain difference counters based on which links are
        // enabled.
        let link_mask_rev = cc.link_mask_rev();
        for i in 0..DomainDiffCounters::ENABLE_LEN {
            domain_diff_counters.set_enable(i, test_bit(link_mask_rev[0], i));
        }

        // Wait for next update
        let timer_result = timer.wait_until_stall(next_update);
        panic_on_missed_deadline(&mut uart, &timer, next_update, timer_result);
        next_update += interval;
    }
}

fn panic_on_missed_deadline(
    uart: &mut Uart,
    timer: &Timer,
    next_update: Instant,
    timer_result: WaitResult,
) {
    if let WaitResult::AlreadyPassed = timer_result {
        uwriteln!(
            uart,
            "Deadline missed! Timer did not update in time. Now: {}. Next update: {}",
            timer.now().micros(),
            next_update.micros(),
        )
        .unwrap();
        panic!("Deadline missed! See UART log.");
    };
}

/// Test whether the `i`-th bit in `bv` is set.
fn test_bit(bv: u8, i: usize) -> bool {
    (bv & (1 << i)) != 0
}

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    loop {
        continue;
    }
}
