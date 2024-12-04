// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]
#![cfg_attr(not(test), no_main)]
#![allow(non_snake_case)]

#[cfg(not(test))]
extern crate panic_halt;

#[cfg(not(test))]
use riscv_rt::entry;

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
    print("Executing `ebreak` instruction...\n");

    unsafe {
        riscv::asm::ebreak();
    }

    print("This should never be reached\n");

    loop {
        continue;
    }
}

#[export_name = "ExceptionHandler"]
fn exception_handler(_trap_frame: &riscv_rt::TrapFrame) -> ! {
    riscv::interrupt::free(|| {
        print("... caught an exception. Looping forever now.\n");
    });
    loop {
        continue;
    }
}
