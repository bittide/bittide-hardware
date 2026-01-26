#![no_std]
#![no_main]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use riscv_rt::entry;

// Declare the external C main function
extern "C" {
    fn c_main() -> !;
}

// Entry point - riscv-rt handles all the startup
#[entry]
fn main() -> ! {
    unsafe { c_main() }
}

// Panic handler required for no_std
#[panic_handler]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    loop {
        continue;
    }
}
