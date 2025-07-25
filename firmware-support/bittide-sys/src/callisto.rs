// SPDX-FileCopyrightText: 2023 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::shared::devices::clock_control::{ClockControl, ReframingState};
use bittide_hal::shared::devices::debug_register::DebugRegister;
use bittide_hal::shared::types::speed_change::SpeedChange;

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

/// Rust version of
/// `Bittide.ClockControl.Callisto.Types.ControlConfig`.
#[derive(Clone)]
pub struct ControlConfig {
    /// Enable reframing. Reframing allows a system to resettle buffers around
    /// their midpoints, without dropping any frames. For more information, see
    /// [arXiv:2303.11467](https://arxiv.org/abs/2303.11467).
    pub reframing_enabled: bool,
    /// Number of cycles to wait until reframing takes place after
    /// stability has been detected.
    pub wait_time: usize,
    // Gain. See https://github.com/bittide/Callisto.jl/blob/e47139fca128995e2e64b2be935ad588f6d4f9fb/demo/pulsecontrol.jl#L24.
    pub k_p: f32,
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

/// Test whether the `i`-th bit in `bv` is set.
fn test_bit(bv: u8, i: usize) -> bool {
    (bv & (1 << i)) != 0
}

/// Clock correction strategy based on:
/// [https://github.com/bittide/Callisto.jl](https://github.com/bittide/Callisto.jl)
pub fn callisto<I>(
    cc: &ClockControl,
    eb_counters_iter: I,
    config: &ControlConfig,
    state: &mut ControlSt,
) where
    I: Iterator<Item = i32>,
{
    // `fStep` should match the step size of the clock boards. For all our HITL
    // tests this is set by `HwCcTopologies.commonStepSizeSelect`.
    const FSTEP: f32 = 10e-9;

    let link_mask_rev = cc.link_mask_rev();

    // Sum the data counts for all active links
    let measured_sum: i32 = eb_counters_iter
        .enumerate()
        .map(|(i, v)| if test_bit(link_mask_rev, i) { v } else { 0 })
        .sum();

    let r_k = measured_sum as f32;
    let c_des = config.k_p * r_k + state.steady_state_target;
    let c_est = FSTEP * state.z_k as f32;

    let b_k = if c_des < c_est {
        SpeedChange::SlowDown
    } else if c_des > c_est {
        SpeedChange::SpeedUp
    } else {
        SpeedChange::NoChange
    };

    state.z_k += speed_change_to_sign(b_k);
    state.b_k = b_k;

    if config.reframing_enabled {
        state.rf_state_update(
            config.wait_time,
            cc.links_stable() ^ cc.link_mask() == 0,
            c_des,
        );
    }
}
