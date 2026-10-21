// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::manual_additions::timer::{Duration, Instant, WaitResult};
use bittide_hal::shared_devices::{
    ClockControl, Freeze, SampleMemory, SyncOutGenerator, Timer, Uart,
};
use bittide_sys::callisto::Callisto;
use bittide_sys::sample_store::SampleStore;
use bittide_sys::stability_detector::StabilityDetector;
use clash_macros::unsigned;
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

/// Configures whether and how clock control samples are stored after all links
/// have become stable.
#[derive(Clone, Copy)]
pub struct StableSampling {
    /// Store a sample every _n_ clock control updates. Updates happen every
    /// 200 us, so '5_000' stores a sample every second.
    pub store_samples_every: usize,
    /// Number of samples reserved for the phase after all links have become
    /// stable. The phase before that can use the rest of the sample memory.
    pub reserved_samples: usize,
}

/// Run clock control. If 'stable_sampling' is 'None', samples are only stored
/// until all links are stable.
///
/// If 'speed_change_delay' is set, each speed change is applied that long after
/// it was computed, instead of immediately. The loop keeps doing its other work
/// in the meantime. This mimics frequency adjustments that take effect only once
/// a slow transaction (e.g., over SPI) has finished. The delay should be shorter
/// than the update interval minus the time the loop needs.
#[allow(clippy::too_many_arguments)]
pub fn run<DDC: DomainDiffCountersInterface>(
    cc: ClockControl,
    timer: Timer,
    uart: &mut Uart,
    freeze: Freeze,
    sample_memory: SampleMemory,
    sync_out_generator: SyncOutGenerator,
    domain_diff_counters: DDC,
    stable_sampling: Option<StableSampling>,
    speed_change_delay: Option<Duration>,
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
    if let Some(stable_sampling) = stable_sampling {
        sample_store.set_max_samples(
            SampleStore::CAPACITY.saturating_sub(stable_sampling.reserved_samples),
        );
    }

    // Update clock control 5K updates per second
    let interval = Duration::from_micros(200);
    let mut next_update = timer.now() + interval;
    let mut prev_all_stable = false;
    let mut reported_late_speed_change = false;

    loop {
        // Store frozen elastic buffer counters
        freeze.set_freeze(());

        // Do clock control update
        let speed_change = callisto.update(
            &cc,
            izip!(0..DDC::ENABLE_LEN, freeze.eb_counters_volatile_iter()).map(|(i, counter)| {
                if domain_diff_counters.enable(i).unwrap_or(false) {
                    Some(counter.into_inner())
                } else {
                    None
                }
            }),
        );
        let apply_speed_change_at = speed_change_delay.map(|delay| timer.now() + delay);
        if apply_speed_change_at.is_none() {
            cc.set_change_speed(speed_change);
        }

        // Detect stability
        let stability = stability_detector.update(&cc, timer.now());

        // Store debug information. Unless configured otherwise, stop capturing
        // samples if we are stable to reduce plot sizes.
        let has_sense_of_global_time = freeze.number_of_sync_pulses_seen() != unsigned!(0, n = 32);
        if (!prev_all_stable || stable_sampling.is_some()) && has_sense_of_global_time {
            sample_store.store(&freeze, stability, callisto.accumulated_speed_requests);
        }

        // Emit stability information over UART
        let all_stable = stability.all_stable();
        if !prev_all_stable && all_stable {
            uwriteln!(uart, "All links stable").unwrap();
            if let Some(stable_sampling) = stable_sampling {
                sample_store.set_max_samples(SampleStore::CAPACITY);
                sample_store.set_store_samples_every(stable_sampling.store_samples_every);
            }
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

        // Apply a delayed speed change. Report (once) if the loop's other work
        // took longer than the delay, so the delay was longer than intended.
        if let Some(apply_at) = apply_speed_change_at {
            if let WaitResult::AlreadyPassed = timer.wait_until_stall(apply_at) {
                if !reported_late_speed_change {
                    reported_late_speed_change = true;
                    uwriteln!(
                        uart,
                        "Delayed speed change applied late. Now: {}. Intended: {}",
                        timer.now().micros(),
                        apply_at.micros(),
                    )
                    .unwrap();
                }
            }
            cc.set_change_speed(speed_change);
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
