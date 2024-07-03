#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::uwriteln;

mod wrappers;

#[cfg(not(test))]
use riscv_rt::entry;

use wrappers::*;

impl ufmt::uWrite for UART {
    fn write_str(&mut self, s: &str) -> Result<(), Self::Error> {
        for b in s.bytes() {
            self.set_data(b);
        }
        Ok(())
    }

    type Error = ();
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let DeviceInstances {
        UART: mut uart,
        StatusReg: status,
        ..
    } = unsafe { DeviceInstances::new() };

    let names = ["Rust", "RISC-V", "Haskell"];
    for name in names {
        uwriteln!(uart, "Hello from {}!", name).unwrap();
    }
    uwriteln!(uart, "This can also do {} {:#x}", "debug prints", 42).unwrap();
    uwriteln!(uart, "Going in echo mode!").unwrap();

    status.set_status(TestStatus::Success);

    loop {
        let c = uart.data();
        uart.set_data(c);
    }
}

#[panic_handler]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    let DeviceInstances {
        StatusReg: status, ..
    } = unsafe { DeviceInstances::new() };
    status.set_status(TestStatus::Fail);
    loop {
        continue;
    }
}
