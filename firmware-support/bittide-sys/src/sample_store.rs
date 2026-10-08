// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use crate::stability_detector::Stability;
use bittide_hal::shared_devices::{freeze::Freeze, sample_memory::SampleMemory};
use clash_macros::bitvector;

/// Must match `wordsPerSample` in `Bittide.Instances.Hitl.Utils.Utils.dumpCcSamples`.
const WORDS_PER_SAMPLE: usize = 16;

/// Metrics on the rate at which clock control makes frequency adjustments. All
/// fields are cumulative since clock control started, except for `min_slack_micros`.
#[derive(Copy, Clone)]
pub struct UpdateMetrics {
    /// Number of speed changes requested by the clock control algorithm.
    pub requested_speed_changes: u32,
    /// Number of requested speed changes that were not applied, because the clock
    /// chip was still busy processing a previous one.
    pub skipped_speed_changes: u32,
    /// Smallest time left before the deadline of a clock control update, since the
    /// previously stored sample.
    pub min_slack_micros: u32,
}

impl UpdateMetrics {
    pub fn new() -> Self {
        Self {
            requested_speed_changes: 0,
            skipped_speed_changes: 0,
            min_slack_micros: u32::MAX,
        }
    }
}

impl Default for UpdateMetrics {
    fn default() -> Self {
        Self::new()
    }
}

/// State machinery for storing clock control samples in memory.
pub struct SampleStore {
    memory: SampleMemory,
    store_samples_every: usize,
    counter: usize,
}

impl SampleStore {
    pub fn new(memory: SampleMemory, store_samples_every: usize) -> Self {
        // First memory location is reserved for the number of samples stored.
        memory.set_data(0, bitvector!(0x0, n = 32));

        Self {
            memory,
            store_samples_every,
            counter: 0,
        }
    }

    /// *Actually* store the contents of 'Freeze' to memory. Note that the public
    /// function 'store' does 'store_samples_every' boundary checking.
    fn do_store(
        &mut self,
        freeze: &Freeze,
        bump_counter: bool,
        stability: Stability,
        net_speed_change: i32,
        metrics: UpdateMetrics,
    ) {
        let n_samples_stored: usize =
            u32::from_ne_bytes(self.memory.data(0).unwrap().into_inner()) as usize;
        let start_index = n_samples_stored * WORDS_PER_SAMPLE + 1;

        // Store local clock counter
        let local_clock: u64 = freeze.local_clock_counter().into_inner();
        let local_clock_msbs = (local_clock >> 32) as u32;
        let local_clock_lsbs = (local_clock & 0xFFFFFFFF) as u32;
        self.memory.set_data(
            start_index,
            bitvector!(local_clock_lsbs.to_le_bytes(), n = 32),
        );
        self.memory.set_data(
            start_index + 1,
            bitvector!(local_clock_msbs.to_le_bytes(), n = 32),
        );

        // Store number of sync pulses seen
        let number_of_sync_pulses_seen = freeze.number_of_sync_pulses_seen();
        self.memory
            .set_data(start_index + 2, number_of_sync_pulses_seen.into());

        // Store cycles since last sync pulse
        let cycles_since_sync_pulse = freeze.cycles_since_sync_pulse();
        self.memory
            .set_data(start_index + 3, cycles_since_sync_pulse.into());

        // Store stability information
        self.memory.set_data(
            start_index + 4,
            bitvector!(
                (stability.stable as u32 | ((stability.settled as u32) << 8)).to_le_bytes(),
                n = 32
            ),
        );

        // Store net speed change
        self.memory.set_data(
            start_index + 5,
            bitvector!((net_speed_change as u32).to_le_bytes(), n = 32),
        );

        // Store the EB counters
        let n_eb_counters = Freeze::EB_COUNTERS_LEN;
        for (i, eb_counter) in freeze.eb_counters_volatile_iter().enumerate() {
            self.memory.set_data(start_index + 6 + i, eb_counter.into());
        }

        // Store update rate metrics
        let metrics_index = start_index + 6 + n_eb_counters;
        for (i, value) in [
            metrics.requested_speed_changes,
            metrics.skipped_speed_changes,
            metrics.min_slack_micros,
        ]
        .into_iter()
        .enumerate()
        {
            self.memory
                .set_data(metrics_index + i, bitvector!(value.to_le_bytes(), n = 32));
        }

        // Bump number of samples stored, but only if we're running "for real"
        // and the data actually fits in memory.
        if bump_counter && self.memory.data(start_index + WORDS_PER_SAMPLE).is_some() {
            self.memory.set_data(
                0,
                bitvector!(((n_samples_stored + 1) as u32).to_le_bytes(), n = 32),
            );
        }
    }

    /// Store the contents of 'Freeze' to memory. Whether or not a store actually
    /// happens depends on whether we're at a 'store_sample_every' boundary. Returns
    /// true if a sample was stored, false if this was a dry run.
    pub fn store(
        &mut self,
        freeze: &Freeze,
        stability: Stability,
        net_speed_change: i32,
        metrics: UpdateMetrics,
    ) -> bool {
        self.counter += 1;

        let bump_counter = self.counter >= self.store_samples_every;

        // Always go through the motions of loading/storing to get a reliable
        // execution time.
        self.do_store(freeze, bump_counter, stability, net_speed_change, metrics);

        if bump_counter {
            self.counter = 0;
        }

        bump_counter
    }
}
