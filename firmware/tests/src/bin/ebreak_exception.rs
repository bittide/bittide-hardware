#![no_std]
#![no_main]

use core::fmt::Write;
use riscv_rt::entry;

use contranomy_sys::{character_device, println};

#[entry]
fn main() -> ! {
    unsafe {
        character_device::initialise(0x9000_0000 as *mut u8);
    }

    println!("Executing `ebreak` instruction...");
    unsafe {
        riscv::asm::ebreak();
    }

    println!("This should never be reached");

    loop {
        continue;
    }
}

#[export_name = "ExceptionHandler"]
fn exception_handler(_trap_frame: &riscv_rt::TrapFrame) -> ! {
    riscv::interrupt::free(|_| {
        println!("... caught an exception. Looping forever now.");
    });
    loop {
        continue;
    }
}
