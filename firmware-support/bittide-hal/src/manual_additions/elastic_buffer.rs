// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use crate::shared_devices::elastic_buffer::ElasticBuffer;

impl ElasticBuffer {
    /// Minimum occupancy value for the elastic buffer (signed 8-bit).
    pub const MIN_OCCUPANCY: i8 = i8::MIN;

    /// Maximum occupancy value for the elastic buffer (signed 8-bit).
    pub const MAX_OCCUPANCY: i8 = i8::MAX;

    /// Execute an adjustment on the elastic buffer using the two-register interface.
    ///
    /// This is a convenience function that:
    /// 1. Submits the adjustment with `adjustment_async`
    /// 2. Waits for completion with `adjustment_wait`
    ///
    /// Negative values drain (remove frames), positive values fill (add frames).
    pub fn set_adjustment(&self, adjustment: i32) {
        self.set_adjustment_async(adjustment);
        self.set_adjustment_wait(());
    }

    /// Set buffer occupancy to a target value.
    ///
    /// This function adjusts the buffer occupancy to the specified target by issuing
    /// the appropriate Fill or Drain commands. It reads the current occupancy and
    /// calculates the difference to reach the target.
    ///
    /// There is no control over which frames are duplicated or dropped, so this function
    /// should only be used during system initialization.
    ///
    /// Returns the number of frames added or dropped
    pub fn set_occupancy(&self, target: i8) -> i8 {
        let current = self.data_count();
        let delta = target - current;
        self.set_adjustment(delta as i32);
        delta
    }

    /// Set buffer occupancy to a target value asynchronously.
    ///
    /// Like `set_occupancy`, but submits the adjustment without waiting for completion.
    /// Use `set_adjustment_wait` to wait for the adjustment to complete.
    ///
    /// There is no control over which frames are duplicated or dropped, so this function
    /// should only be used during system initialization.
    ///
    /// Returns the number of frames added or dropped
    pub fn set_occupancy_async(&self, target: i8) -> i8 {
        let current = self.data_count();
        let delta = target - current;
        self.set_adjustment_async(delta as i32);
        delta
    }

    /// Reset the auto-centering state machine (safe wrapper).
    ///
    /// This function ensures safety by:
    /// 1. Disabling the auto-centering state machine
    /// 2. Waiting for it to become idle
    /// 3. Resetting it
    ///
    /// This function always succeeds and is safe to call at any time.
    pub fn reset_auto_center(&self) {
        self.set_auto_center_enable(false);
        self.wait_auto_center_idle();
        self.set_auto_center_reset_unchecked(());
        self.wait_auto_center_idle();
    }

    /// Wait for the auto-centering state machine to become idle.
    ///
    /// This is useful after disabling auto-center before performing a reset.
    pub fn wait_auto_center_idle(&self) {
        while !self.auto_center_is_idle() {
            core::hint::spin_loop();
        }
    }
}
