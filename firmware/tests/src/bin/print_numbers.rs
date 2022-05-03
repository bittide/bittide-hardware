// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]
#![no_main]

use core::fmt::Write;
use riscv_rt::entry;

use contranomy_sys::{character_device, println};

#[entry]
fn main() -> ! {
    unsafe {
        character_device::initialise(0x9000_0000 as *mut u8);
    }

    println!("This test serves as a regression test.");
    println!("The regression this tests for is that a lone `format_args!()`");
    println!("to display a numeric value does in fact cause the value to print.");
    println!();

    for i in 0..10 {
        println!("{i}");
    }

    loop {
        continue;
    }
}
