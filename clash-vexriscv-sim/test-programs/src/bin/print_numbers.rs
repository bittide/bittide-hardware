// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]
#![cfg_attr(not(test), no_main)]

use core::fmt::Write;

#[cfg(not(test))]
use riscv_rt::entry;

#[cfg(not(test))]
extern crate panic_halt;

const ADDR: *mut u8 = 0x0000_1000 as *mut u8;

fn print(s: &str) {
    for b in s.bytes() {
        unsafe {
            ADDR.write_volatile(b);
        }
    }
}

struct PrintAddr;

impl core::fmt::Write for PrintAddr {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        print(s);
        Ok(())
    }
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut addr = PrintAddr;

    print("This test serves as a regression test.\n");
    print("The regression this tests for is that a lone `format_args!()`\n");
    print("to display a numeric value does in fact cause the value to print.\n");
    print("\n");

    for i in 0..10 {
        let _ = writeln!(addr, "{i}");
    }

    loop {
        continue;
    }
}
