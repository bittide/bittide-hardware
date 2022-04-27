//  Copyright  :  (C) 2022, Google LLC
//  License    :  Apache-2.0
//  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

#![no_std]
#![no_main]

use core::fmt::Write;
use riscv_rt::entry;

use contranomy_sys::{character_device, println};

#[entry]
fn main() -> ! {
    unsafe {
        character_device::initialise(0x90000000 as *mut u8);
    }

    println!("hello, world.");

    loop {
        continue;
    }
}
