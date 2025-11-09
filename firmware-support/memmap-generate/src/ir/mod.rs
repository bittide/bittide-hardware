// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Intermediate Representation for memory maps
//!
//! For further processing, the input format is transformed to an internmediate
//! representation.
//!
//! This intermediate format is based on flat storages and uses handles to
//! refer to any data.
//!
//! One advantage of a handle based approach is that new data can be
//! associated to any part of the memory map without having to modify
//! existing data. This can be done by associating existing handles with
//! new data in a separate type.
//!
//! The flat storage is based on [mod@crate::storage].

pub mod deduplicate;
pub mod input_to_ir;
pub mod monomorph;
pub mod types;
