// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]

// Since this module gets generated, running rustfmt on a
// non-built source tree will error.
// This makes it so that rustfmt won't attempt to check
// files that aren't yet generated.
// (and if we want to style check generated files anyway?..)
#[rustfmt::skip]
pub mod hals;

pub use hals::*;

#[rustfmt::skip]
pub mod shared_devices;

#[rustfmt::skip]
pub mod types;


pub mod manual_additions;
