// SPDX-FileCopyrightText: 2023 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::shared_devices::clock_control::ClockControl;
use bittide_hal::types::callisto_config::CallistoConfig;
use bittide_hal::types::maybe::Maybe;
use bittide_hal::types::speed_change::SpeedChange;

/// Rust sibling of
/// `Bittide.ClockControl.Callisto.Types.Stability`.
#[repr(transparent)]
#[derive(PartialEq, PartialOrd)]
pub struct Stability(usize);

impl Stability {
    /// Indicates stability of the signal over time.
    pub fn stable(&self) -> bool {
        (self.0 & 0b01) == 0b01
    }
    /// Indicates whether the signal is stable and close to
    /// `targetDataCount`.
    pub fn settled(&self) -> bool {
        (self.0 & 0b10) == 0b10
    }
}

pub enum ReframingState {
    Detect,
    Wait {
        target_correction: f32,
        cur_wait_time: u32,
    },
    Done,
}

pub struct Callisto {
    /// Accumulated speed change requests, where
    /// * `speedup ~ 1`
    /// * `slowdown ~ -1`
    pub accumulated_speed_requests: i32,
    /// Steady-state value (determined when stability is detected for
    /// the first time).
    steady_state_target: f32,
    reframe_state: ReframingState,
    config: CallistoConfig,
}

impl Callisto {
    pub fn new(config: CallistoConfig) -> Self {
        Self {
            accumulated_speed_requests: 0,
            steady_state_target: 0.0,
            reframe_state: ReframingState::Detect,
            config,
        }
    }

    /// Clock correction strategy based on:
    /// [https://github.com/bittide/Callisto.jl](https://github.com/bittide/Callisto.jl)
    pub fn update<I>(self: &mut Callisto, cc: &ClockControl, eb_counters_iter: I) -> SpeedChange
    where
        I: Iterator<Item = Option<i32>>,
    {
        // `fStep` should match the step size of the clock boards. For all our HITL
        // tests this is set by `HwCcTopologies.commonStepSizeSelect`.
        const FSTEP: f32 = 10e-9; // 10 PPB

        // Sum the data counts for all active links
        let measured_sum: i32 = eb_counters_iter.flatten().sum();

        let c_des = self.config.gain * (measured_sum as f32) + self.steady_state_target;
        let c_est = FSTEP * self.accumulated_speed_requests as f32;

        let speed_request = if c_des < c_est {
            SpeedChange::SlowDown
        } else if c_des > c_est {
            SpeedChange::SpeedUp
        } else {
            SpeedChange::NoChange
        };

        self.accumulated_speed_requests += speed_change_to_sign(speed_request);

        if let Maybe::Just(wait_time) = self.config.wait_time {
            self.update_reframe_state(
                wait_time as usize,
                Into::<u32>::into(cc.links_stable()) != 0,
                c_des,
            );
        }

        speed_request
    }

    fn update_reframe_state(&mut self, wait_time: usize, stable: bool, target: f32) {
        match self.reframe_state {
            ReframingState::Detect if stable => {
                self.reframe_state = ReframingState::Wait {
                    target_correction: target,
                    cur_wait_time: wait_time as u32,
                };
            }
            ReframingState::Wait {
                ref mut cur_wait_time,
                ..
            } if *cur_wait_time > 0 => *cur_wait_time -= 1,
            ReframingState::Wait {
                target_correction, ..
            } => {
                self.reframe_state = ReframingState::Done;
                self.steady_state_target = target_correction;
            }
            _ => (),
        }
    }
}

fn speed_change_to_sign(speed_change: SpeedChange) -> i32 {
    match speed_change {
        SpeedChange::NoChange => 0,
        SpeedChange::SlowDown => -1,
        SpeedChange::SpeedUp => 1,
    }
}
