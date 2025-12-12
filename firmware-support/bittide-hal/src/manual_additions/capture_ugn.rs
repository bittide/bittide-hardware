// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::shared_devices::CaptureUgn;

impl CaptureUgn {
    pub fn ugn(&self) -> Option<u64> {
        if self.has_captured() {
            Some(self.ugn_unchecked())
        } else {
            None
        }
    }

    pub fn ugn_unchecked(&self) -> u64 {
        self.local_counter() - u64::from_ne_bytes(self.remote_counter())
    }
}
