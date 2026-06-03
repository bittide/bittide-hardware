// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

pub mod addressable_buffer;
pub mod aligned;
pub mod capture_ugn;
pub mod dna;
pub mod elastic_buffer;
pub mod ring_buffer;
pub mod si539x_spi;
pub mod timer;
pub mod uart;

use crate::types::Maybe;

pub trait ConvertOptional<Target> {
    fn conv_optional(self) -> Target;
}

impl<T> ConvertOptional<Option<T>> for Maybe<T> {
    #[inline]
    fn conv_optional(self) -> Option<T> {
        match self {
            Maybe::Just(val) => Some(val),
            Maybe::Nothing => None,
        }
    }
}

impl<T> ConvertOptional<Maybe<T>> for Option<T> {
    #[inline]
    fn conv_optional(self) -> Maybe<T> {
        match self {
            Some(val) => Maybe::Just(val),
            None => Maybe::Nothing,
        }
    }
}
