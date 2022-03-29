#![no_std]
#![no_main]

extern crate panic_halt;

use riscv_rt::entry;

pub mod character_device;

use character_device::write_str;

#[entry]
fn main() -> ! {
    loop {
        write_str("Hello from Rust!!\n");
    }
}
