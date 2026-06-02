// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::manual_additions::{
    clock_control::ClockControlInterface,
    timer::{Duration, Instant},
};
use itertools::izip;
use ufmt::derive::uDebug;

fn test_bit(bv: u8, i: usize) -> bool {
    (bv & (1 << i)) != 0
}

pub struct StabilityDetector<const N: usize> {
    data_counts: [i32; N],
    starts: [Option<Instant>; N],
    margin: u32,
    frame_size: Duration,
    prev_stabilities: [bool; N],
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

impl Stability {
    pub fn all_stable(&self, n_links: usize) -> bool {
        self.stable == (1 << n_links) - 1
    }
}

/// Picks the current data counts and wait for `frame_size` to pass without
/// the data counts changing more than `margin`.
impl<const N: usize> StabilityDetector<N> {
    pub fn new(margin: u32, frame_size: Duration) -> Self {
        Self {
            data_counts: [0; N],
            starts: [None; N],
            margin,
            frame_size,
            prev_stabilities: [false; N],
        }
    }

    /// Whether all links are stable
    pub fn all_stable(&self) -> bool {
        self.prev_stabilities.iter().all(|&stable| stable)
    }

    pub fn update(&mut self, cc: &impl ClockControlInterface, now: Instant) -> Stability {
        let mut stables: u32 = 0;
        let mut settleds: u32 = 0;
        let link_mask_rev = cc.link_mask_rev();

        for (i, (data_count_stored, maybe_start, data_count, min_seen, max_seen, prev_stable)) in
            izip!(
                self.data_counts.iter_mut(),
                self.starts.iter_mut(),
                cc.data_counts_volatile_iter(),
                cc.min_data_counts_seen_volatile_iter(),
                cc.max_data_counts_seen_volatile_iter(),
                self.prev_stabilities.iter_mut()
            )
            .enumerate()
        {
            stables <<= 1;
            settleds <<= 1;
            let active = test_bit(link_mask_rev[0], i);
            if active {
                let diff0 = data_count_stored.abs_diff(min_seen.into_inner());
                let diff1 = data_count_stored.abs_diff(max_seen.into_inner());
                let height_violated = diff0 > self.margin || diff1 > self.margin;

                let (stable, settled) = if height_violated {
                    *maybe_start = Some(now);
                    *data_count_stored = data_count.into_inner();
                    cc.set_clear_data_counts_seen(true);
                    (false, false)
                } else {
                    let start = *maybe_start.get_or_insert(now);
                    let stable = (now - start) > self.frame_size;
                    let settled = data_count_stored.unsigned_abs() < self.margin;
                    (stable, stable && settled)
                };

                stables |= stable as u32;
                settleds |= settled as u32;

                if !*prev_stable && stable {
                    *data_count_stored = data_count.into_inner();
                    cc.set_clear_data_counts_seen(true);
                }
                *prev_stable = stable;
            }
        }

        cc.set_links_settled([settleds as u8]);
        cc.set_links_stable([stables as u8]);

        Stability {
            stable: stables as u8,
            settled: settleds as u8,
        }
    }
}
