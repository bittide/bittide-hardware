// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use crate::shared_devices::elastic_buffer::ElasticBuffer;
use crate::types::EbCommand;

impl ElasticBuffer {
    /// Minimum occupancy value for the elastic buffer (signed 8-bit).
    pub const MIN_OCCUPANCY: i8 = i8::MIN;

    /// Maximum occupancy value for the elastic buffer (signed 8-bit).
    pub const MAX_OCCUPANCY: i8 = i8::MAX;

    /// Execute a command on the elastic buffer using the three-register interface.
    ///
    /// This is a convenience function that:
    /// 1. Prepares the command in `command_prepare`
    /// 2. Triggers execution with `command_go`
    /// 3. Waits for completion with `command_wait`
    pub fn set_command(&self, command: EbCommand) {
        self.set_command_prepare(command);
        self.set_command_go(());
        self.set_command_wait(());
    }

    /// Increase buffer occupancy by N frames. there is no control over which
    /// frames are duplicated. Only usable during system initialization.
    pub fn increase_occupancy(&self, n: u32) {
        self.set_command(EbCommand::Fill { n });
    }

    /// Decrease buffer occupancy by N frames. there is no control over which
    /// frames are dropped. Only usable during system initialization.
    pub fn decrease_occupancy(&self, n: u32) {
        self.set_command(EbCommand::Drain { n });
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
