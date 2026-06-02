// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]
#![feature(sync_unsafe_cell)]
#![allow(incomplete_features)]
#![feature(generic_const_exprs)]

pub mod axi;
pub mod callisto;
pub mod clock_control_main;
pub mod link_startup;
pub mod mac;
pub mod net_state;
pub mod sample_store;
pub mod smoltcp;
pub mod stability_detector;
pub mod uart;
