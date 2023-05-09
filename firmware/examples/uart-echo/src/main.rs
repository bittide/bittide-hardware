#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::fmt::Write;
pub mod uart;
use uart::Uart;

#[cfg(not(test))]
use riscv_rt::entry;

use bittide_sys::{elf_loading::validation::ElfConfig, print, println};
// use println;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart: Uart = unsafe {Uart::new(0x8000_0000 as *mut u8)};

    let names = ["Rust", "RISC-V", "Haskell"];
    for name in names {
        writeln!(uart, "Hello from {name}!");
    }
    writeln!(uart, "This can also do {:?} {:#x}", "debug prints", 42);
    writeln!(uart, "Going in echo mode!");
    loop {
        let c = uart.receive();
        uart.send(c);
    }
}
