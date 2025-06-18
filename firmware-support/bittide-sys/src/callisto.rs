// SPDX-FileCopyrightText: 2023 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::derive::uDebug;

use bittide_hal::shared::devices::clock_control::{ClockControl, ReframingState};
use bittide_hal::shared::devices::debug_register::DebugRegister;
use bittide_hal::shared::types::speed_change::SpeedChange;

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
    pub reframing_enabled: bool,
    /// Number of cycles to wait until reframing takes place after
    /// stability has been detected.
    pub wait_time: usize,
    /// Target data count. See `Bittide.ClockControl.targetDataCount`.
    pub target_count: isize,
}

/// Rust version of `Bittide.ClockControl.Callisto.Types.ControlSt`.
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
    /// Debug register
    pub debug_register: DebugRegister,
}

impl ControlSt {
    pub fn new(
        z_k: i32,
        b_k: SpeedChange,
        steady_state_target: f32,
        debug_register: DebugRegister,
        init_rf_state: ReframingState,
    ) -> Self {
        let new = ControlSt {
            z_k,
            b_k,
            steady_state_target,
            debug_register,
        };
        new.debug_register.set_reframing_state(init_rf_state);
        new
    }

    fn rf_state_update(&mut self, wait_time: usize, stable: bool, target: f32) {
        match self.debug_register.reframing_state() {
            ReframingState::Detect if stable => {
                self.debug_register
                    .set_reframing_state(ReframingState::Wait {
                        target_correction: target,
                        cur_wait_time: wait_time as u32,
                    });
            }
            ReframingState::Wait {
                ref mut cur_wait_time,
                ..
            } if *cur_wait_time > 0 => *cur_wait_time -= 1,
            ReframingState::Wait {
                target_correction, ..
            } => {
                self.debug_register
                    .set_reframing_state(ReframingState::Done);
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

/// Clock correction strategy based on:
/// [https://github.com/bittide/Callisto.jl](https://github.com/bittide/Callisto.jl)
pub fn callisto(cc: &ClockControl, config: &ControlConfig, state: &mut ControlSt) {
    // see clock control algorithm simulation here:
    //
    // https://github.com/bittide/Callisto.jl/blob/e47139fca128995e2e64b2be935ad588f6d4f9fb/demo/pulsecontrol.jl#L24
    //
    // `k_p` (proportional gain) is copied from the Julia implementation. `fStep` should
    // match the step size of the clock boards. For all our HITL tests this is set by
    // `HwCcTopologies.commonStepSizeSelect`.
    const K_P: f32 = 2e-8;
    const FSTEP: f32 = 10e-9;

    let n_buffers = cc.up_links();

    let measured_sum: i32 = cc.data_counts_volatile_iter().sum();

    let r_k = (measured_sum - n_buffers as i32 * config.target_count as i32) as f32;
    let c_des = K_P * r_k + state.steady_state_target;

    state.z_k += speed_change_to_sign(state.b_k);

    let c_est = FSTEP * state.z_k as f32;

    state.b_k = if c_des < c_est {
        SpeedChange::SlowDown
    } else if c_des > c_est {
        SpeedChange::SpeedUp
    } else {
        SpeedChange::NoChange
    };

    if config.reframing_enabled {
        state.rf_state_update(
            config.wait_time,
            cc.links_stable() ^ cc.link_mask() == 0,
            c_des,
        );
    }
}
