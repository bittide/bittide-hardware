// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use ufmt::uwriteln;

use bittide_hal::shared::devices::uart::Uart;
use bittide_sys::dna_port_e2::{dna_to_u128, DnaValue};
#[cfg(not(test))]
use riscv_rt::entry;

const DNA_ADDR: *const DnaValue = 0xC000_0000 as *const DnaValue;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals.
    let mut uart = unsafe { Uart::new(0x8000_0000 as *mut u8) };
    let dna = dna_to_u128(unsafe { *DNA_ADDR });
    uwriteln!(uart, "{}", dna).unwrap();
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
