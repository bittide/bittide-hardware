// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::{
    manual_additions::timer::{Duration, Instant},
    shared::devices::clock_control::ClockControl,
};
use itertools::izip;
use ufmt::derive::uDebug;

pub struct StabilityDetector {
    /// Reference data counts -- sampled at some point in time and used to
    /// compare against the current data counts to determine stability.
    data_counts: [i32; ClockControl::DATA_COUNTS_LEN],
    /// Start time of the current stability window for each link.
    starts: [Option<Instant>; ClockControl::DATA_COUNTS_LEN],
    /// Margin of error for the data counts to be considered stable.
    margin: u32,
    // Time frame size for stability checks.
    frame_size: Duration,
    // Stabilities of the links at the last update. Used to reset stored data
    // counts when moving from an unstable to a stable state.
    prev_stabilities: [bool; ClockControl::DATA_COUNTS_LEN],
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, uDebug)]
pub struct Stability {
    /// A link is stable if the data counts have not changed "significantly" for
    /// a certain amount of time. See `frame_size` and `margin`.
    pub stable: u8,
    /// A link is settled if the data counts are within the `margin` of
    /// the current elastic buffer's midpoint.
    pub settled: u8,
}

/// Picks the current data counts and wait for `frame_size` to pass without
/// the data counts changing more than `margin`.
impl StabilityDetector {
    pub fn new(margin: u32, frame_size: Duration) -> Self {
        Self {
            data_counts: [0; ClockControl::DATA_COUNTS_LEN],
            starts: [None; ClockControl::DATA_COUNTS_LEN],
            margin,
            frame_size,
            prev_stabilities: [false; ClockControl::DATA_COUNTS_LEN],
        }
    }

    pub fn update(&mut self, cc: &ClockControl, now: Instant) -> Stability {
        let mut stables: u32 = 0;
        let mut settleds: u32 = 0;

        for (data_count_stored, maybe_start, data_count, min_seen, max_seen, prev_stable) in izip!(
            self.data_counts.iter_mut(),
            self.starts.iter_mut(),
            cc.data_counts_volatile_iter(),
            cc.min_data_counts_seen_volatile_iter(),
            cc.max_data_counts_seen_volatile_iter(),
            self.prev_stabilities.iter_mut()
        ) {
            let diff0 = data_count_stored.abs_diff(min_seen);
            let diff1 = data_count_stored.abs_diff(max_seen);
            let height_violated = diff0 > self.margin || diff1 > self.margin;

            let (stable, settled) = if height_violated {
                // Reset everything, as we violated the window height
                *maybe_start = Some(now);
                *data_count_stored = data_count;
                cc.set_clear_data_counts_seen(true);
                (false, false)
            } else {
                let start = *maybe_start.get_or_insert(now);
                let stable = (now - start) > self.frame_size;
                let settled = data_count_stored.unsigned_abs() < self.margin;
                (stable, stable && settled)
            };

            stables <<= 1;
            stables |= stable as u32;

            settleds <<= 1;
            settleds |= settled as u32;

            // If a link has become stable, we store the current data count and
            // clear the data counts seen. This is to prevent a link from being
            // considered unstable again immediately after it has become stable,
            // if it happened to stabilize very close to its margins and, just
            // by "luck", the next sample pushes it over the edge.
            if !*prev_stable && stable {
                *data_count_stored = data_count;
                cc.set_clear_data_counts_seen(true);
            }
            *prev_stable = stable;
        }

        // XXX: These values are currently hardcoded to 8 bits, which would
        //      break if we ever have more than 8 links. We'll cross that bridge
        //      when we get there.
        cc.set_links_settled(settleds as u8);
        cc.set_links_stable(stables as u8);

        Stability {
            stable: stables as u8,
            settled: settleds as u8,
        }
    }
}
