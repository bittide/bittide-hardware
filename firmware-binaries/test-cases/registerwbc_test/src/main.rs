#![no_std]
#![cfg_attr(not(test), no_main)]
#![allow(const_item_mutation)]
#![allow(clippy::empty_loop)]
// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::uwriteln;

use bittide_hal::hals::register_wb_c as hal;

use core::fmt::Write;
#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: hal::DeviceInstances = unsafe { hal::DeviceInstances::new() };

fn test_result(result: &str) -> ! {
    let uart = &mut INSTANCES.uart;
    uwriteln!(uart, "RESULT: {}", result).unwrap();
    loop {}
}

fn test_ok() -> ! {
    test_result("OK")
}

fn test_fail(msg: &str) -> ! {
    let mut full_msg = heapless::String::<64>::new();
    let _ = write!(full_msg, "FAIL: {}", msg);
    test_result(&full_msg)
}

fn expect<T: core::fmt::Debug + PartialEq>(msg: &str, expected: T, actual: T) {
    if expected != actual {
        let mut err = heapless::String::<64>::new();
        let _ = write!(err, "{}: expected {:?}, got {:?}", msg, expected, actual);
        test_fail(&err);
    }
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let many_types = &mut INSTANCES.many_types;

    expect("s0", -8, many_types.s0());
    expect("s1", 8, many_types.s1());
    expect("s2", 16, many_types.s2());
    expect("s3", 3721049880298531338, many_types.s3());

    expect("u0", 8, many_types.u0());
    expect("u1", 16, many_types.u1());
    expect("u2", 3721049880298531338, many_types.u2());

    expect("bv0", 8, many_types.bv0());
    expect("bv1", 16, many_types.bv1());
    expect("bv2", 3721049880298531338, many_types.bv2());

    // Passing floats/doubles to 'expect' yields linker errors..
    expect("f0", true, many_types.f0() == -8.0);
    expect("f1", true, many_types.f1() == 8.0);
    expect("d0", true, many_types.d0() == -8.0);
    expect("d1", true, many_types.d1() == 8.0);

    test_ok();
}

#[panic_handler]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    test_result("PANIC");
}
