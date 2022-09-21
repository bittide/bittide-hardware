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

    println!("hello, world.");

    loop {
        continue;
    }
}
