// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::freeze::{Freeze, SampleMemory};

use crate::stability_detector::Stability;

const WORDS_PER_SAMPLE: usize = 13;

/// State machinery for storing clock control samples in memory.
pub struct SampleStore {
    memory: SampleMemory,
    store_samples_every: usize,
    counter: usize,
}

impl SampleStore {
    pub fn new(memory: SampleMemory, store_samples_every: usize) -> Self {
        // First memory location is reserved for the number of samples stored.
        memory.set_data(0, 0);

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
        freeze: &bittide_hal::freeze::Freeze,
        bump_counter: bool,
        stability: Stability,
        net_speed_change: i32,
    ) {
        let n_samples_stored = self.memory.data(0).unwrap() as usize;
        let start_index = n_samples_stored * WORDS_PER_SAMPLE + 1;

        // Store local clock counter
        let local_clock: u64 = freeze.local_clock_counter();
        let local_clock_msbs = (local_clock >> 32) as u32;
        let local_clock_lsbs = (local_clock & 0xFFFFFFFF) as u32;
        self.memory.set_data(start_index, local_clock_lsbs);
        self.memory.set_data(start_index + 1, local_clock_msbs);

        // Store number of sync pulses seen
        let number_of_sync_pulses_seen = freeze.number_of_sync_pulses_seen();
        self.memory
            .set_data(start_index + 2, number_of_sync_pulses_seen);

        // Store cycles since last sync pulse
        let cycles_since_sync_pulse = freeze.cycles_since_sync_pulse();
        self.memory
            .set_data(start_index + 3, cycles_since_sync_pulse);

        // Store stability information
        self.memory.set_data(
            start_index + 4,
            stability.stable as u32 | ((stability.settled as u32) << 8),
        );

        // Store net speed change
        self.memory
            .set_data(start_index + 5, net_speed_change as u32);

        // Store the EB counters
        for (i, eb_counter) in freeze.eb_counters_volatile_iter().enumerate() {
            self.memory.set_data(start_index + 6 + i, eb_counter as u32);
        }

        // Bump number of samples stored, but only if we're running "for real"
        // and the data actually fits in memory.
        if bump_counter && self.memory.data(start_index + WORDS_PER_SAMPLE).is_some() {
            self.memory.set_data(0, (n_samples_stored + 1) as u32);
        }
    }

    /// Store the contents of 'Freeze' to memory. Whether or not a store actually
    /// happens depends on whether we're at a 'store_sample_every' boundary. Returns
    /// true if a sample was stored, false if this was a dry run.
    pub fn store(&mut self, freeze: &Freeze, stability: Stability, net_speed_change: i32) -> bool {
        self.counter += 1;

        let bump_counter = self.counter >= self.store_samples_every;

        // Always go through the motions of loading/storing to get a reliable
        // execution time.
        self.do_store(freeze, bump_counter, stability, net_speed_change);

        if bump_counter {
            self.counter = 0;
        }

        bump_counter
    }
}
