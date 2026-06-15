// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::manual_additions::timer::{Duration, Instant, WaitResult};
use bittide_hal::shared_devices::{
    ClockControl, Freeze, SampleMemory, SyncOutGenerator, Timer, Uart,
};
use bittide_macros::unsigned;
use bittide_sys::callisto::Callisto;
use bittide_sys::sample_store::SampleStore;
use bittide_sys::stability_detector::StabilityDetector;
use itertools::izip;
use ufmt::uwriteln;

/// Per-design `DomainDiffCounters` device interface.
///
/// The HAL dedup pass keeps `DomainDiffCounters` per-design because the
/// Si539xConfiguration variant has a different register layout. This trait
/// abstracts over the two clock-control variants (which are otherwise
/// byte-identical).
pub trait DomainDiffCountersInterface {
    const ENABLE_LEN: usize;
    fn enable(&self, idx: usize) -> Option<bool>;
    fn set_enable(&self, idx: usize, val: bool) -> Option<()>;
}

impl DomainDiffCountersInterface
    for bittide_hal::hals::soft_ugn_demo_clock_control::devices::DomainDiffCounters
{
    const ENABLE_LEN: usize = Self::ENABLE_LEN;
    fn enable(&self, idx: usize) -> Option<bool> {
        Self::enable(self, idx)
    }
    fn set_enable(&self, idx: usize, val: bool) -> Option<()> {
        Self::set_enable(self, idx, val)
    }
}

impl DomainDiffCountersInterface
    for bittide_hal::hals::wire_demo_clock_control::devices::DomainDiffCounters
{
    const ENABLE_LEN: usize = Self::ENABLE_LEN;
    fn enable(&self, idx: usize) -> Option<bool> {
        Self::enable(self, idx)
    }
    fn set_enable(&self, idx: usize, val: bool) -> Option<()> {
        Self::set_enable(self, idx, val)
    }
}

impl DomainDiffCountersInterface
    for bittide_hal::hals::manticore_demo_clock_control::devices::DomainDiffCounters
{
    const ENABLE_LEN: usize = Self::ENABLE_LEN;
    fn enable(&self, idx: usize) -> Option<bool> {
        Self::enable(self, idx)
    }
    fn set_enable(&self, idx: usize, val: bool) -> Option<()> {
        Self::set_enable(self, idx, val)
    }
}

pub fn run<DDC: DomainDiffCountersInterface>(
    cc: ClockControl,
    timer: Timer,
    uart: &mut Uart,
    freeze: Freeze,
    sample_memory: SampleMemory,
    sync_out_generator: SyncOutGenerator,
    domain_diff_counters: DDC,
) -> ! {
    debug_assert_eq!(DDC::ENABLE_LEN, Freeze::EB_COUNTERS_LEN);
    debug_assert_eq!(DDC::ENABLE_LEN, ClockControl::DATA_COUNTS_LEN);

    uwriteln!(uart, "Starting sync out generator..").unwrap();
    sync_out_generator.set_active(true);

    uwriteln!(uart, "Starting clock control..").unwrap();
    let mut callisto = Callisto::new(cc.config().callisto);

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
        cc.set_change_speed(callisto.update(
            &cc,
            izip!(0..DDC::ENABLE_LEN, freeze.eb_counters_volatile_iter()).map(|(i, counter)| {
                if domain_diff_counters.enable(i).unwrap_or(false) {
                    Some(counter.into_inner())
                } else {
                    None
                }
            }),
        ));

        // Detect stability
        let stability = stability_detector.update(&cc, timer.now());

        // Store debug information. Stop capturing samples if we are stable to
        // reduce plot sizes.
        let has_sense_of_global_time = freeze.number_of_sync_pulses_seen() != unsigned!(0, n = 32);
        if !prev_all_stable && has_sense_of_global_time {
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
        for i in 0..DDC::ENABLE_LEN {
            domain_diff_counters.set_enable(i, test_bit(link_mask_rev[0], i));
        }

        // Wait for next update
        let timer_result = timer.wait_until_stall(next_update);
        panic_on_missed_deadline(uart, &timer, next_update, timer_result);
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
