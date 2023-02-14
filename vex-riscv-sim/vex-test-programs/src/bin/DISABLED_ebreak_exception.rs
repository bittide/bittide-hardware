// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]
#![cfg_attr(not(test), no_main)]
#![allow(non_snake_case)]

use core::fmt::Write;

#[cfg(not(test))]
use riscv_rt::entry;

use bittide_sys::println;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    unsafe {
        bittide_sys::character_device::initialise(0x2000_0000 as *mut u8);
    }

    println!("Executing `ebreak` instruction...");

    /*
    unsafe {
        riscv::asm::ebreak();
    }

    println!("This should never be reached");

    */
    loop {
        continue;
    }
}

#[export_name = "ExceptionHandler"]
fn exception_handler(_trap_frame: &riscv_rt::TrapFrame) -> ! {
    riscv::interrupt::free(|| {
        println!("... caught an exception. Looping forever now.");
    });
    loop {
        continue;
    }
}
