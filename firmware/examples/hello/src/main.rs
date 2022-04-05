#![no_std]
#![no_main]

use core::fmt::Write;
use riscv_rt::entry;

use contranomy_sys::{character_device, println};

#[entry]
fn main() -> ! {
    unsafe {
        character_device::initialise(0x90000000);
    }

    let names = ["Rust", "RISC-V", "Haskell"];
    loop {
        for name in names {
            println!("Hello from {name}!");
        }
        println!("This can also do {:?} {:#x}", "debug prints", 42);
    }
}
