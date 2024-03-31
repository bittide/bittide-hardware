#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022-2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;

#[cfg(not(test))]
use riscv_rt::entry;

use ufmt::uwrite;

const STATUS_REG_ADDR: *mut u32 = 0xE000_0000 as *mut u32;

fn test_success() {
    unsafe {
        STATUS_REG_ADDR.write_volatile(1);
    }
}

fn test_failure() {
    unsafe {
        STATUS_REG_ADDR.write_volatile(2);
    }
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    #[allow(clippy::eq_op)]
    {
        assert_eq!(14 + 51, 65);
    }

    for i in 0..5 {
        let mut s = heapless::String::<32>::new();
        _ = uwrite!(s, "{}", i);

        let num = s.parse::<i32>().unwrap();
        assert_eq!(i, num);
    }

    test_success();

    loop {
        continue;
    }
}

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    test_failure();
    loop {
        continue;
    }
}
