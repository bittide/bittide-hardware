// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use crate::shared::devices::dna::Dna;
use crate::shared::types::Maybe;

impl Dna {
    /// Fetch the DNA value from the device, retrying until successful.
    pub fn dna(&self) -> u128 {
        loop {
            match self.maybe_dna() {
                Maybe::Just(value) => return value,
                Maybe::Nothing => continue,
            }
        }
    }
}
