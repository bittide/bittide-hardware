#![no_std]
#![no_main]

extern crate panic_halt;

use core::fmt::Write;
use riscv_rt::entry;

pub mod character_device;

use character_device::DEBUG_IO;

#[entry]
fn main() -> ! {
    let names = ["Rust", "RISC-V", "Haskell"];
    loop {
        for name in names {
            writeln!(DEBUG_IO, "Hello from {name}!").unwrap();
        }
        writeln!(DEBUG_IO, "This can also do {:?} {:#x}", "debug prints", 42).unwrap();
    }
}
