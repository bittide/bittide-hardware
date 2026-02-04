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

    /// Increase buffer occupancy by N frames. there is no control over which
    /// frames are duplicated. Only usable during system initialization.
    pub fn increase_occupancy(&self, n: u32) {
        self.set_adjustment(n as i32);
    }

    /// Decrease buffer occupancy by N frames. there is no control over which
    /// frames are dropped. Only usable during system initialization.
    pub fn decrease_occupancy(&self, n: u32) {
        self.set_adjustment(-(n as i32));
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
    /// Returns the buffer occupancy before adjustment.
    pub fn set_occupancy(&self, target: i8) -> i8 {
        let current = self.data_count();
        let delta = target - current;

        if delta > 0 {
            self.increase_occupancy(delta as u32);
        } else if delta < 0 {
            self.decrease_occupancy((-delta) as u32);
        }
        // If delta == 0, do nothing - already at target
        delta
    }

    /// Clear the underflow flag.
    ///
    /// Writes to the underflow register to clear the sticky flag.
    pub fn clear_underflow(&self) {
        self.set_underflow(false);
    }

    /// Clear the overflow flag.
    ///
    /// Writes to the overflow register to clear the sticky flag.
    pub fn clear_overflow(&self) {
        self.set_overflow(false);
    }

    /// Clear both underflow and overflow flags.
    ///
    /// Convenience function to clear both sticky flags at once.
    pub fn clear_flags(&self) {
        self.clear_underflow();
        self.clear_overflow();
    }
}
