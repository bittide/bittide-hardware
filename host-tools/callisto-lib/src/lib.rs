// SPDX-FileCopyrightText: 2023 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use bittide_sys::{callisto, clock_control};
use std::mem::{align_of, size_of};

/// Rust sibling of `Bittide.ClockControl.SpeedChange`.
#[repr(u32)]
pub enum SpeedChange {
    /// Keeps the clock as it is.
    NoChange = 0,
    /// Decreases clock speed.
    SlowDown = 1,
    /// Increases clock speed.
    SpeedUp = 2,
}

// We use a static assertion to verify that the memory layout appears
// to be as expected for ensuring a consistent encoding at the Haskell
// side.
const _: () = assert!(4 == size_of::<SpeedChange>());
const _: () = assert!(4 == align_of::<SpeedChange>());

/// Rust sibling of
/// `Bittide.ClockControl.Callisto.Types.ReframingState`.
#[repr(C)]
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

const _: () = assert!(12 == size_of::<ReframingState>());
const _: () = assert!(4 == align_of::<ReframingState>());

const _: () = assert!(
    0u32 == {
        let v = ReframingState::Detect;
        unsafe { *(&v as *const _ as *const u32) }
    }
);

const _: () = assert!(
    1u32 == {
        let v = ReframingState::Done;
        unsafe { *(&v as *const _ as *const u32) }
    }
);

#[allow(dead_code)]
const SOME_RF_STATE_WAIT: ReframingState = ReframingState::Wait {
    target_correction: 3.321,
    cur_wait_time: 12345,
};

#[allow(dead_code)]
const CASTED_RF_STATE_WAIT: *const (u32, f32, u32) =
    &SOME_RF_STATE_WAIT as *const _ as *const (u32, f32, u32);

const _: () = assert!(
    2u32 == {
        let (x, _, _) = unsafe { *CASTED_RF_STATE_WAIT };
        x
    }
);

const _: () = assert!(
    3.321f32 == {
        let (_, x, _) = unsafe { *CASTED_RF_STATE_WAIT };
        x
    }
);

const _: () = assert!(
    12345u32 == {
        let (_, _, x) = unsafe { *CASTED_RF_STATE_WAIT };
        x
    }
);

/// Rust sibling of `Bittide.ClockControl.Callisto.Types.ControlSt`.
#[repr(C)]
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

const _: () = assert!(24 == size_of::<ControlSt>());
const _: () = assert!(4 == align_of::<ControlSt>());

#[allow(dead_code)]
const SOME_CONTROL_STATE: ControlSt = ControlSt {
    z_k: 54321,
    b_k: SpeedChange::SlowDown,
    steady_state_target: 3.185,
    rf_state: SOME_RF_STATE_WAIT,
};

#[allow(dead_code)]
const CASTED_CONTROL_STATE: *const (i32, u32, f32, u32, f32, u32) =
    &SOME_CONTROL_STATE as *const _ as *const (i32, u32, f32, u32, f32, u32);

const _: () = assert!(
    54321i32 == {
        let (x, _, _, _, _, _) = unsafe { *CASTED_CONTROL_STATE };
        x
    }
);

const _: () = assert!(
    1u32 == {
        let (_, x, _, _, _, _) = unsafe { *CASTED_CONTROL_STATE };
        x
    }
);

const _: () = assert!(
    3.185f32 == {
        let (_, _, x, _, _, _) = unsafe { *CASTED_CONTROL_STATE };
        x
    }
);

const _: () = assert!(
    2u32 == {
        let (_, _, _, x, _, _) = unsafe { *CASTED_CONTROL_STATE };
        x
    }
);

const _: () = assert!(
    3.321f32 == {
        let (_, _, _, _, x, _) = unsafe { *CASTED_CONTROL_STATE };
        x
    }
);

const _: () = assert!(
    12345u32 == {
        let (_, _, _, _, _, x) = unsafe { *CASTED_CONTROL_STATE };
        x
    }
);

/// Rust sibling of
/// `Bittide.ClockControl.Callisto.Types.ControlConfig`.
#[repr(C)]
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

const _: () = assert!(3 * size_of::<isize>() == size_of::<ControlConfig>());
const _: () = assert!(align_of::<isize>() == align_of::<ControlConfig>());

#[allow(dead_code)]
const SOME_CONTROL_CONFIG: ControlConfig = ControlConfig {
    reframing_enabled: 1,
    wait_time: 88,
    target_count: -23,
};

#[allow(dead_code)]
const CASTED_CONTROL_CONFIG: *const (usize, usize, isize) =
    &SOME_CONTROL_CONFIG as *const _ as *const (usize, usize, isize);

const _: () = assert!(
    1usize == {
        let (x, _, _) = unsafe { *CASTED_CONTROL_CONFIG };
        x
    }
);

const _: () = assert!(
    88usize == {
        let (_, x, _) = unsafe { *CASTED_CONTROL_CONFIG };
        x
    }
);

const _: () = assert!(
    -23isize == {
        let (_, _, x) = unsafe { *CASTED_CONTROL_CONFIG };
        x
    }
);

fn speed_change_from_ffi(val: &SpeedChange) -> clock_control::SpeedChange {
    match val {
        SpeedChange::SpeedUp => clock_control::SpeedChange::SpeedUp,
        SpeedChange::SlowDown => clock_control::SpeedChange::SlowDown,
        SpeedChange::NoChange => clock_control::SpeedChange::NoChange,
    }
}

fn speed_change_to_ffi(val: &clock_control::SpeedChange) -> SpeedChange {
    match val {
        clock_control::SpeedChange::SpeedUp => SpeedChange::SpeedUp,
        clock_control::SpeedChange::SlowDown => SpeedChange::SlowDown,
        clock_control::SpeedChange::NoChange => SpeedChange::NoChange,
    }
}

fn reframing_state_from_ffi(val: &ReframingState) -> callisto::ReframingState {
    match val {
        ReframingState::Detect => callisto::ReframingState::Detect,
        ReframingState::Done => callisto::ReframingState::Done,
        ReframingState::Wait {
            target_correction,
            cur_wait_time,
        } => callisto::ReframingState::Wait {
            target_correction: *target_correction,
            cur_wait_time: *cur_wait_time,
        },
    }
}

fn reframing_state_to_ffi(val: &callisto::ReframingState) -> ReframingState {
    match val {
        callisto::ReframingState::Detect => ReframingState::Detect,
        callisto::ReframingState::Done => ReframingState::Done,
        callisto::ReframingState::Wait {
            target_correction,
            cur_wait_time,
        } => ReframingState::Wait {
            target_correction: *target_correction,
            cur_wait_time: *cur_wait_time,
        },
    }
}

fn control_state_from_ffi(st: &ControlSt) -> callisto::ControlSt {
    callisto::ControlSt {
        z_k: st.z_k,
        b_k: speed_change_from_ffi(&st.b_k),
        steady_state_target: st.steady_state_target,
        rf_state: reframing_state_from_ffi(&st.rf_state),
    }
}

fn control_state_to_ffi(st: &callisto::ControlSt, rs: &mut ControlSt) {
    rs.z_k = st.z_k;
    rs.b_k = speed_change_to_ffi(&st.b_k);
    rs.steady_state_target = st.steady_state_target;
    rs.rf_state = reframing_state_to_ffi(&st.rf_state);
}

fn control_config_from_ffi(cfg: &ControlConfig) -> callisto::ControlConfig {
    callisto::ControlConfig {
        reframing_enabled: cfg.reframing_enabled,
        wait_time: cfg.wait_time,
        target_count: cfg.target_count,
    }
}

unsafe fn vsi_from_ptr<'a>(ptr: *const ()) -> &'a [callisto::StabilityIndication] {
    let usize_ptr = ptr as *const usize;
    let data_ptr = usize_ptr.offset(1) as *const callisto::StabilityIndication;
    return std::slice::from_raw_parts(data_ptr, *usize_ptr);
}

unsafe fn data_counts_from_ptr<'a>(ptr: *const ()) -> &'a [isize] {
    let usize_ptr = ptr as *const usize;
    let data_ptr = usize_ptr.offset(1) as *const isize;
    return std::slice::from_raw_parts(data_ptr, *usize_ptr);
}

/// Runs the callisto algorithm
///
/// # Safety
///
/// - `config_ptr` needs to point to a valid memory address holding a
///   `ControlConfig`
/// - `stability_checks_ptr` needs to point to a valid memory block holding
///   a `VecS n StabilityIndication`
/// - `data_counts_ptr` needs to point to a valid memory address holding
///   a `VecS n (DataCountS m)
/// - `control_state_ptr` needs to point to a valid memory address holding
///   a `ControlSt`
#[no_mangle]
pub unsafe extern "C" fn __c_callisto_rust(
    config_ptr: *const ControlConfig,
    availability_mask: u32,
    stability_checks_ptr: *const (),
    data_counts_ptr: *const (),
    control_state_ptr: *mut ControlSt,
) {
    let mut state = control_state_from_ffi(&*control_state_ptr);

    let vsi = vsi_from_ptr(stability_checks_ptr);
    let data_counts = data_counts_from_ptr(data_counts_ptr);

    let links_stable = {
        let mut mask = 0u32;
        for (i, indication) in vsi.iter().enumerate() {
            if indication.stable() {
                mask |= 1 << i;
            }
        }
        mask & availability_mask
    };

    callisto::callisto(
        &control_config_from_ffi(&*(config_ptr as *const ControlConfig)),
        availability_mask,
        links_stable,
        data_counts.iter().copied(),
        &mut state,
    );

    control_state_to_ffi(&state, &mut *control_state_ptr)
}
