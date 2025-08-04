// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use ufmt::uwriteln;

use bittide_hal::shared_devices::{Dna, Uart};
#[cfg(not(test))]
use riscv_rt::entry;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals.
    let mut uart = unsafe { Uart::new(0x4000_0000 as *mut u8) };
    let dna = unsafe { Dna::new(0x6000_0000 as *mut u8) };
    uwriteln!(uart, "{}", dna.dna()).unwrap();
    loop {
        continue;
    }
}

#[panic_handler]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    loop {
        continue;
    }
}
