// SPDX-FileCopyrightText: 2023 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![feature(slice_ptr_get)]
use hs_bindgen::{traits::ReprRust, traits::ReprC, *};
use std::mem;
use std::fmt;

#[macro_use]
extern crate static_assertions;

/// Rust sibling of `Bittide.ClockControl.SpeedChange`.
#[repr(u32)]
pub enum SpeedChange
  { /// Increases clock speed.
    SpeedUp  = 0
  , /// Decreases clock speed.
    SlowDown = 1
  , /// Keeps the clock as it is.
    NoChange = 2
  }

// We use `static_assert` to verify that the memory layout appears to
// be as expected for ensuring a consistent encoding at the Haskell
// side.
const_assert!(
  4 ==
    mem::size_of::<SpeedChange>()
);

const_assert!(
  4 ==
    mem::align_of::<SpeedChange>()
);

/// Produces the same output as the corresponding Haskell `Show`
/// instance.
impl fmt::Display for SpeedChange {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self
    { SpeedChange::SpeedUp  => write!(f, "SpeedUp")
    , SpeedChange::SlowDown => write!(f, "SlowDown")
    , SpeedChange::NoChange => write!(f, "NoChange")
    }
  }
}

/// Read `SpeedChange` values from their correpsonding FFI `u32`
/// representation.
impl ReprRust<u32> for SpeedChange
{
  fn from(v: u32) -> Self
    {
      match v
        { 0 => SpeedChange::SpeedUp
        , 1 => SpeedChange::SlowDown
        , 2 => SpeedChange::NoChange
        , _ => panic!("out of range")
        }
    }
}

/// Turn `SpeedChange` values into their correpsonding FFI `u32`
/// representation.
impl ReprC<SpeedChange> for u32
{
  fn from(v: SpeedChange) -> Self
    {
      match v
        { SpeedChange::SpeedUp  => 0
        , SpeedChange::SlowDown => 1
        , SpeedChange::NoChange => 2
        }
    }
}

/// Rust sibling of
/// `Bittide.ClockControl.Callisto.Types.ReframingState`.
#[allow(dead_code)]
#[repr(C)]
pub enum ReframingState
  { /// The controller remains in this state until stability has been
    /// detected.
    Detect
  , /// Reframing has taken place. There is nothing more to do.
    Done
  , /// The controller remains in this state for the predefined
    /// number of cycles with the assumption that the elastic buffers
    /// of all other nodes are sufficiently stable after that time.
    Wait
      { /// Stored correction value to be applied at reframing time.
        target_correction: f32
      , /// Number of cycles to wait until reframing takes place.
        cur_wait_time:     u32
      }
  }

const_assert!(
  12 ==
    mem::size_of::<ReframingState>()
);

const_assert!(
  4 ==
    mem::align_of::<ReframingState>()
);

const_assert!(
  0u32 ==
    { let v = ReframingState::Detect
    ; unsafe {*(&v as *const _ as *const u32)}
    }
);

const_assert!(
  1u32 ==
    { let v = ReframingState::Done
    ; unsafe {*(&v as *const _ as *const u32)}
    }
);

#[allow(dead_code)]
const SOME_RF_STATE_WAIT: ReframingState =
  ReframingState::Wait
    { target_correction: 3.321
    , cur_wait_time: 12345
    };

#[allow(dead_code)]
const CASTED_RF_STATE_WAIT: *const (u32, f32, u32) =
  &SOME_RF_STATE_WAIT as *const _ as *const (u32, f32, u32);

const_assert!(
  2u32 ==
    { let (x,_,_) = unsafe {*CASTED_RF_STATE_WAIT}; x }
);

const_assert!(
  3.321f32 ==
    { let (_,x,_) = unsafe {*CASTED_RF_STATE_WAIT}; x }
);

const_assert!(
  12345u32 ==
    { let (_,_,x) = unsafe {*CASTED_RF_STATE_WAIT}; x }
);

/// Produces the same output as the corresponding Haskell `Show`
/// instance.
impl fmt::Display for ReframingState {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self
    { ReframingState::Detect => write!(f, "Detect")
    , ReframingState::Done => write!(f, "Done")
    , ReframingState::Wait{target_correction, cur_wait_time} => write!
      ( f
      , "Wait {{targetCorrection = {}, curWaitTime = {}}}"
      , target_correction
      , cur_wait_time
      )
    }
  }
}

/// Allows to cast a "`void`" pointer into a mutable `ReframingState`
/// reference (unsafe).
impl ReprRust<*const ()> for &mut ReframingState {
  fn from(ptr: *const ()) -> Self {
    return unsafe { mem::transmute(ptr) };
  }
}

/// Rust sibling of `Bittide.ClockControl.Callisto.Types.ControlSt`.
#[repr(C)]
pub struct ControlSt
  { /// Accumulated speed change requests, where
    /// * `speedup ~ 1`
    /// * `slowdown ~ -1`
    pub z_k: i32
  , /// Previously submitted speed change request. Used to determine
    /// the estimated clock frequency.
    pub b_k: SpeedChange
  , /// Steady-state value (determined when stability is detected for
    /// the first time).
    pub steady_state_target: f32
  , /// finite state machine for reframing detection
    pub rf_state: ReframingState
  }

const_assert!(
  24 ==
    mem::size_of::<ControlSt>()
);

const_assert!(
  4 ==
    mem::align_of::<ControlSt>()
);

#[allow(dead_code)]
const SOME_CONTROL_STATE: ControlSt =
  ControlSt
    { z_k: 54321
    , b_k: SpeedChange::SlowDown
    , steady_state_target: 3.185
    , rf_state: SOME_RF_STATE_WAIT
    };

#[allow(dead_code)]
const CASTED_CONTROL_STATE: *const (i32, u32, f32, u32, f32, u32) =
  &SOME_CONTROL_STATE as *const _ as *const (i32, u32, f32, u32, f32, u32);

const_assert!(
  54321i32 ==
    { let (x,_,_,_,_,_) = unsafe {*CASTED_CONTROL_STATE}; x }
);

const_assert!(
  1u32 ==
    { let (_,x,_,_,_,_) = unsafe {*CASTED_CONTROL_STATE}; x }
);

const_assert!(
  3.185f32 ==
    { let (_,_,x,_,_,_) = unsafe {*CASTED_CONTROL_STATE}; x }
);

const_assert!(
  2u32 ==
    { let (_,_,_,x,_,_) = unsafe {*CASTED_CONTROL_STATE}; x }
);

const_assert!(
  3.321f32 ==
    { let (_,_,_,_,x,_) = unsafe {*CASTED_CONTROL_STATE}; x }
);

const_assert!(
  12345u32 ==
    { let (_,_,_,_,_,x) = unsafe {*CASTED_CONTROL_STATE}; x }
);

/// Produces the same output as the corresponding Haskell `Show`
/// instance.
impl fmt::Display for ControlSt {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!
      ( f
      , "ControlSt {{\
         _z_k = {}, \
         _b_k = {}, \
         _steadyStateTarget = {}, \
         rfState = {}\
         }}"
      , self.z_k
      , self.b_k
      , self.steady_state_target
      , self.rf_state
      )
  }
}

/// Allows to cast a "`void`" pointer into a mutable `ControlSt`
/// reference (unsafe).
impl ReprRust<*const ()> for &mut ControlSt {
  fn from(ptr: *const ()) -> Self {
    return unsafe { mem::transmute(ptr) };
  }
}

/// Allows to casts a "`void`" pointer into a non-mutable `ControlSt`
/// reference (unsafe).
impl ReprRust<*const ()> for &ControlSt {
  fn from(ptr: *const ()) -> Self {
    return unsafe { mem::transmute(ptr) };
  }
}

/// Rust sibling of
/// `Bittide.ClockControl.StabilityChecker.StabilityIndication`.
#[derive(PartialEq, PartialOrd)]
pub struct StabilityIndication(usize);

impl StabilityIndication {
  /// Indicates stability of the signal over time.
  pub fn stable(&self) -> bool {
    return self.0 & 1 == 1;
  }
  /// Indicates whether the signal is stable and close to
  /// `targetDataCount`.
  pub fn settled(&self) -> bool {
    return self.0 & 2 == 2;
  }
}

/// Produces the same output as the corresponding Haskell `Show`
/// instance.
impl fmt::Display for StabilityIndication {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!
      ( f
      , "StabilityIndication {{\
         stable = {}, \
         settled = {}\
         }}"
      , if self.stable()  { "True" } else { "False" }
      , if self.settled() { "True" } else { "False" }
      )
  }
}

/// Type wrapper for `Vec<StabilityIndication>`.
pub struct VSI(Vec<StabilityIndication>);

/// Allows to casts a "`void`" pointer into a non-mutable `ControlSt`
/// reference (unsafe).
impl ReprRust<*const ()> for &VSI {
  fn from(ptr: *const ()) -> Self {
    return unsafe { mem::transmute(ptr) };
  }
}

/// Produces the same output as the corresponding Haskell `Show`
/// instance.
impl fmt::Display for VSI {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if let Err(e) = write!(f, "[ ") {
      return Err(e);
    }
    for i in 0..self.0.len()-1 {
      if let Err(e) = write!(f, "{}, ", self.0[i]) {
        return Err(e);
      }
    }
    if let Err(e) = write!(f, "{}", self.0[self.0.len()-1]) {
      return Err(e);
    }
    return write!(f, " ]");
  }
}

/// Type wrapper for `Vec<isize>` (cf. `Bittide.ClockControl.DataCount`)
pub struct DataCounts(Vec<isize>);

/// Allows to casts a "`void`" pointer into a non-mutable `ControlSt`
/// reference (unsafe).
impl ReprRust<*const ()> for &DataCounts {
  fn from(ptr: *const ()) -> Self {
    return unsafe { mem::transmute(ptr) };
  }
}

/// Produces the same output as the corresponding Haskell `Show`
/// instance.
impl fmt::Display for DataCounts {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if let Err(e) = write!(f, "[ ") {
      return Err(e);
    }
    for i in 0..self.0.len()-1 {
      if let Err(e) = write!(f, "{}, ", self.0[i]) {
        return Err(e);
      }
    }
    if let Err(e) = write!(f, "{}", self.0[self.0.len()-1]) {
      return Err(e);
    }
    return write!(f, " ]");
  }
}

/// Rust sibling of
/// `Bittide.ClockControl.Callisto.Types.ControlConfig`.
#[repr(C)]
pub struct ControlConfig
  { /// Enable reframing. Reframing allows a system to resettle buffers around
    /// their midpoints, without dropping any frames. For more information, see
    /// [arXiv:2303.11467](https://arxiv.org/abs/2303.11467).
    pub reframing_enabled: usize
  , /// Number of cycles to wait until reframing takes place after
    /// stability has been detected.
    pub wait_time: usize
  , /// Target data count. See `Bittide.ClockControl.targetDataCount`.
    pub target_count: isize
  }

const_assert!(
  3 * mem::size_of::<isize>() ==
    mem::size_of::<ControlConfig>()
);

const_assert!(
  mem::align_of::<isize>() ==
    mem::align_of::<ControlConfig>()
);

#[allow(dead_code)]
const SOME_CONTROL_CONFIG: ControlConfig =
  ControlConfig
    { reframing_enabled: 1
    , wait_time: 88
    , target_count: -23
    };

#[allow(dead_code)]
const CASTED_CONTROL_CONFIG: *const (usize, usize, isize) =
  &SOME_CONTROL_CONFIG as *const _ as *const (usize, usize, isize);

const_assert!(
  1usize ==
    { let (x,_,_) = unsafe {*CASTED_CONTROL_CONFIG}; x }
);

const_assert!(
  88usize ==
    { let (_,x,_) = unsafe {*CASTED_CONTROL_CONFIG}; x }
);

const_assert!(
  -23isize ==
    { let (_,_,x) = unsafe {*CASTED_CONTROL_CONFIG}; x }
);

/// Produces the same output as the corresponding Haskell `Show`
/// instance.
impl fmt::Display for ControlConfig {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!
      ( f
      , "ControlConfig {{\
         reframingEnabled = {}, \
         waitTime = {}, \
         targetCount = {}\
         }}"
      , self.reframing_enabled
      , self.wait_time
      , self.target_count
      )
  }
}

/// Allows to casts a "`void`" pointer into a non-mutable `ControlSt`
/// reference (unsafe).
impl ReprRust<*const ()> for &ControlConfig {
  fn from(ptr: *const ()) -> Self {
    return unsafe { mem::transmute(ptr) };
  }
}

/// Clock correction strategy based on:
/// [https://github.com/bittide/Callisto.jl](https://github.com/bittide/Callisto.jl)
#[hs_bindgen(
    callisto_rust ::
      Ptr () -> // config
      CUInt  -> // availability_mask
      Ptr () -> // stability_checks
      Ptr () -> // data_counts
      Ptr () -> // control_state
      IO ()
)]
pub fn callisto
  ( config:            &ControlConfig
  , availability_mask: u32
  , stability_checks:  &VSI
  , data_counts:       &DataCounts
  , state:             &mut ControlSt
  ) -> () {
  const K_P: f32 = 2e-4;
  const FSTEP: f32 = 5e-4;

  let n_buffers = availability_mask.count_ones() as i32;
  let measured_sum = data_counts.0.iter().sum::<isize>() as i32;
  let r_k = (measured_sum - n_buffers * config.target_count as i32) as f32;
  let c_des = K_P * r_k + state.steady_state_target;

  state.z_k += state.b_k.sign();

  let c_est = FSTEP * state.z_k as f32;

  state.b_k =
    if      c_des < c_est { SpeedChange::SlowDown }
    else if c_des > c_est { SpeedChange::SpeedUp  }
    else                  { SpeedChange::NoChange };

  state.rf_state_update
    ( config.wait_time
    , config.reframing_enabled != 0
    , stability_checks.0.iter().all(|x| x.stable())
    , c_des
    );
}

impl SpeedChange {
  fn sign(&self) -> i32 {
    match &self
    { SpeedChange::SpeedUp  => 1
    , SpeedChange::NoChange => 0
    , SpeedChange::SlowDown => -1
    }
  }
}

impl ControlSt {
  fn rf_state_update
    ( &mut self
    , wait_time: usize
    , enabled: bool
    , stable: bool
    , target: f32
    ) -> () {
    if enabled {
      match self.rf_state
      { ReframingState::Detect if stable =>
          self.rf_state = ReframingState::Wait
            { target_correction: target
            , cur_wait_time: wait_time as u32
            }
      , ReframingState::Wait{ref mut cur_wait_time, ..}
          if *cur_wait_time > 0 =>
            *cur_wait_time -= 1
      , ReframingState::Wait{target_correction, ..} => {
          self.rf_state = ReframingState::Done;
          self.steady_state_target = target_correction;
        }
      , _ => ()
      }
    }
  }
}
