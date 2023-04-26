#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#[cfg(not(test))]
use riscv_rt::entry;

#[cfg(not(test))]
extern crate panic_halt;

use core::fmt::Write;
use heapless::String;

const ADDR: *mut u8 = 0x0000_1000 as *mut u8;

fn print(s: &str) {
    for b in s.bytes() {
        unsafe {
            ADDR.write_volatile(b);
        }
    }
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let i = 14495943;
    let mut s = String::<16>::new();
    let _ = writeln!(s, "Hey! {i}");
    print(&s);

    loop {
        continue;
    }
}
