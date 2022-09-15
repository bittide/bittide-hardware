#![no_std]
#![no_main]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use core::fmt::Write;
use riscv_rt::entry;

use contranomy_sys::println;

const FRAME_SIZE: usize = 8;

#[entry]
fn main() -> ! {
    unsafe {
        contranomy_sys::initialise().unwrap();
    }
    let _components = unsafe { bittide_sys::initialise::<FRAME_SIZE>().unwrap() };

    loop {
        println!("PONG!!");
    }
}
