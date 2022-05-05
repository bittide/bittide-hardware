#![no_std]
#![no_main]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::fmt::Write;
use riscv_rt::entry;

use contranomy_sys::println;

#[entry]
fn main() -> ! {
    unsafe {
        contranomy_sys::initialise().unwrap();
    }

    let names = ["Rust", "RISC-V", "Haskell"];
    loop {
        for name in names {
            println!("Hello from {name}!");
        }
        println!("This can also do {:?} {:#x}", "debug prints", 42);
    }
}
