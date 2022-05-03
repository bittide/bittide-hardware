// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]

#[cfg(feature = "debug-printing")]
pub mod character_device;
#[cfg(feature = "debug-printing")]
pub mod panic_handler;
