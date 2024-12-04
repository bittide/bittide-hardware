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

    // The compiler is smart enough to constant fold the following floating
    // point operations, so we put the values around a black box.
    // That makes it so that the compiler is "blind" to the values and
    // generates the actual instructions
    let b = core::hint::black_box;

    print("This test makes sure the FPU works as expected\n");

    #[allow(clippy::unnecessary_cast)]
    let _ = writeln!(addr, "79823i32 as f32 = {}", 79823i32 as f32);

    let _ = writeln!(addr, "1.3 + 5.3 = {}", b(1.3f32) + b(5.3f32));
    let _ = writeln!(addr, "5.3 - 1.3 = {}", b(5.3f32) - b(1.3f32));

    let _ = writeln!(addr, "24.65 * 43.2 = {}", b(24.65f32) * b(43.2f32));

    let _ = writeln!(addr, "12.6 / 4.2 = {}", b(12.6f32) / b(4.2f32));

    loop {
        continue;
    }
}
