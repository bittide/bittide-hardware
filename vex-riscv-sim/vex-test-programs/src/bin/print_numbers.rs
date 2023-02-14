// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]
#![cfg_attr(not(test), no_main)]

use core::fmt::Write;

#[cfg(not(test))]
use riscv_rt::entry;

use bittide_sys::println;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    unsafe {
        bittide_sys::character_device::initialise(0x2000_0000 as *mut u8);
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
