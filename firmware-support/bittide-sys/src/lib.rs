// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]
#![feature(sync_unsafe_cell)]

pub mod axi;
pub mod callisto;
pub mod clock_control;
pub mod debug_register;
pub mod dna_port_e2;
pub mod gather_unit;
pub mod mac;
pub mod program_stream;
pub mod scatter_unit;
pub mod smoltcp;
pub mod switch_demo_pe;
pub mod time;
pub mod uart;
pub mod ugn;
