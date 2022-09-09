#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#[cfg(not(test))]
use riscv_rt::entry;

#[cfg(not(test))]
extern crate panic_halt;

const ADDR: *mut u8 = 0x2000_0000 as *mut u8;

fn print(s: &str) {
    for b in s.bytes() {
        unsafe {
            ADDR.write_volatile(b);
        }
    }
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    print("0....|....1....|....2....|....3....|....4....|....5....|....6\n");
    print("0....|....1....|....2....|....3....|....4....|....5....|....6\n");
    print("0....|....1....|....2....|....3....|....4....|....5....|....6....|....1....|....2....|....3....|....4....|....5....|....6....|....1....|....2....|....3....|....4....|....5....|....6\n");

    loop {
        continue;
    }
}
