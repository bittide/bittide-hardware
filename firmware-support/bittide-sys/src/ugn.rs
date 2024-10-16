// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::derive::uDebug;

#[repr(C)]
#[derive(uDebug, PartialEq, Eq, Copy, Clone)]
pub struct Ugn {
    pub local_counter: u64,
    pub remote_counter: u64,
}

impl Ugn {
    // Calculate the oracle number
    pub fn calculate_ugn(&self) -> u64 {
        self.local_counter - self.remote_counter
    }

    // Check if both the local and remote counter have been captured.
    pub fn is_link_up(&self) -> bool {
        self.local_counter != 0 && self.remote_counter != 0
    }
}
