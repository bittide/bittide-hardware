// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

/// The `DnaValue` type is a unsigned 96-bit integer. We represent it as a 12-byte array
/// because Rust does not have a built-in 96-bit integer type.
pub type DnaValue = [u8; 12];

/// Convert a `DnaValue` to a `u128` integer.
pub fn dna_to_u128(dna: DnaValue) -> u128 {
    let mut u128_array = [0u8; 16];
    u128_array[..12].copy_from_slice(&dna);
    u128::from_le_bytes(u128_array)
}
