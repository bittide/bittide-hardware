#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::fmt::Write;
use riscv_rt::entry;

use bittide_sys::println;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    unsafe {
        let init = bittide_sys::Initialiser::new().unwrap();
        init.initialise_character_device("character-device")
            .unwrap();
    };

    let names = ["Rust", "RISC-V", "Haskell"];
    loop {
        for name in names {
            println!("Hello from {name}!");
        }
        println!("This can also do {:?} {:#x}", "debug prints", 42);
    }
}
