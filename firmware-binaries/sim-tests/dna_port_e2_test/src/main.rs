// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use ufmt::uwriteln;

use bittide_hal::hals::dna_port_e2_test::DeviceInstances;
#[cfg(not(test))]
use riscv_rt::entry;

const DEVICES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals.
    let mut uart = DEVICES.uart;
    let dna = DEVICES.dna;
    let mut dna_arr = [0; 16];
    dna_arr[0..12].copy_from_slice(&dna.dna());
    let dna_val = u128::from_le_bytes(dna_arr);
    uwriteln!(uart, "{}", dna_val).unwrap();
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
