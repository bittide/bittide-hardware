// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]
#![cfg_attr(not(test), no_main)]

use core::fmt::Write;
use riscv_rt::entry;

use bittide_sys::println;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    unsafe {
        let init = bittide_sys::Initialiser::new().unwrap();
        init.initialise_character_device("character-device")
            .unwrap();
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
