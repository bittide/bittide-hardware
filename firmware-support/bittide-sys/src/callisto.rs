// SPDX-FileCopyrightText: 2023 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::derive::uDebug;

use crate::clock_control::SpeedChange;

/// Rust sibling of
/// `Bittide.ClockControl.StabilityChecker.StabilityIndication`.
#[repr(transparent)]
#[derive(PartialEq, PartialOrd)]
pub struct StabilityIndication(usize);

impl StabilityIndication {
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

/// Rust version of
/// `Bittide.ClockControl.Callisto.Types.ControlConfig`.
#[derive(uDebug, Clone)]
pub struct ControlConfig {
    /// Enable reframing. Reframing allows a system to resettle buffers around
    /// their midpoints, without dropping any frames. For more information, see
    /// [arXiv:2303.11467](https://arxiv.org/abs/2303.11467).
    pub reframing_enabled: usize,
    /// Number of cycles to wait until reframing takes place after
    /// stability has been detected.
    pub wait_time: usize,
    /// Target data count. See `Bittide.ClockControl.targetDataCount`.
    pub target_count: isize,
}

/// Rust version of
/// `Bittide.ClockControl.Callisto.Types.ReframingState`.
#[derive(Clone)]
pub enum ReframingState {
    /// The controller remains in this state until stability has been
    /// detected.
    Detect,
    /// Reframing has taken place. There is nothing more to do.
    Done,
    /// The controller remains in this state for the predefined
    /// number of cycles with the assumption that the elastic buffers
    /// of all other nodes are sufficiently stable after that time.
    Wait {
        /// Stored correction value to be applied at reframing time.
        target_correction: f32,
        /// Number of cycles to wait until reframing takes place.
        cur_wait_time: u32,
    },
}

/// Rust version of `Bittide.ClockControl.Callisto.Types.ControlSt`.
#[derive(Clone)]
pub struct ControlSt {
    /// Accumulated speed change requests, where
    /// * `speedup ~ 1`
    /// * `slowdown ~ -1`
    pub z_k: i32,
    /// Previously submitted speed change request. Used to determine
    /// the estimated clock frequency.
    pub b_k: SpeedChange,
    /// Steady-state value (determined when stability is detected for
    /// the first time).
    pub steady_state_target: f32,
    /// finite state machine for reframing detection
    pub rf_state: ReframingState,
}

impl ControlSt {
    fn rf_state_update(&mut self, wait_time: usize, enabled: bool, stable: bool, target: f32) {
        if enabled {
            match self.rf_state {
                ReframingState::Detect if stable => {
                    self.rf_state = ReframingState::Wait {
                        target_correction: target,
                        cur_wait_time: wait_time as u32,
                    }
                }
                ReframingState::Wait {
                    ref mut cur_wait_time,
                    ..
                } if *cur_wait_time > 0 => *cur_wait_time -= 1,
                ReframingState::Wait {
                    target_correction, ..
                } => {
                    self.rf_state = ReframingState::Done;
                    self.steady_state_target = target_correction;
                }
                _ => (),
            }
        }
    }
}

/// Clock correction strategy based on:
/// [https://github.com/bittide/Callisto.jl](https://github.com/bittide/Callisto.jl)
pub fn callisto(
    config: &ControlConfig,
    availability_mask: u32,
    links_stable: u32,
    data_counts: impl Iterator<Item = isize>,
    state: &mut ControlSt,
) {
    // see clock control algorithm simulation here:
    //
    // https://github.com/bittide/Callisto.jl/blob/e47139fca128995e2e64b2be935ad588f6d4f9fb/demo/pulsecontrol.jl#L24
    //
    // `k_p` (proportional gain) is copied from the Julia implementation. `fStep` should
    // match the step size of the clock boards. For all our HITL tests this is set by
    // `HwCcTopologies.commonStepSizeSelect`.
    const K_P: f32 = 2e-9;
    const FSTEP: f32 = 10e-9;

    let n_buffers = availability_mask.count_ones();
    let measured_sum = data_counts.sum::<isize>() as i32;
    let r_k = (measured_sum - n_buffers as i32 * config.target_count as i32) as f32;
    let c_des = K_P * r_k + state.steady_state_target;

    state.z_k += state.b_k.sign();

    let c_est = FSTEP * state.z_k as f32;

    state.b_k = if c_des < c_est {
        SpeedChange::SlowDown
    } else if c_des > c_est {
        SpeedChange::SpeedUp
    } else {
        SpeedChange::NoChange
    };

    state.rf_state_update(
        config.wait_time,
        config.reframing_enabled != 0,
        links_stable.count_ones() == n_buffers,
        c_des,
    );
}
