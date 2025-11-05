#![no_std]
#![cfg_attr(not(test), no_main)]
// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::uwriteln;

use bittide_hal::hals::delay_wishbone_c as hal;

#[cfg(not(test))]
use riscv_rt::entry;

#[cfg_attr(not(test), entry)]
#[allow(clippy::empty_loop)]
fn main() -> ! {
    let mut instances = unsafe { hal::DeviceInstances::new() };
    let uart = &mut instances.uart;
    let whoami = instances.who_am_i.identifier().to_le_bytes();
    match core::str::from_utf8(&whoami) {
        Ok(whoami) => {
            let mut valid = true;
            for c in whoami.chars() {
                if !c.is_ascii_graphic() {
                    uwriteln!(
                        uart,
                        "Encountered non-printable ASCII char code: {}",
                        c as u8
                    )
                    .unwrap();
                    valid = false;
                }
            }
            if valid {
                uwriteln!(uart, "WhoAmID: {}", whoami).unwrap();
            } else {
                uwriteln!(uart, "WhoAmID was invalid.").unwrap();
            }
        }
        Err(_) => uwriteln!(uart, "Read nonsense WhoAmID. Raw: {:?}", whoami).unwrap(),
    }
    uwriteln!(uart, "\x04").unwrap();
    loop {}
}

#[panic_handler]
#[allow(clippy::empty_loop)]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
