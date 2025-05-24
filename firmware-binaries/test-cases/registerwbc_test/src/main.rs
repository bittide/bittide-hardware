#![no_std]
#![cfg_attr(not(test), no_main)]
#![allow(const_item_mutation)]
#![allow(clippy::empty_loop)]
// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use ufmt::{uwrite, uwriteln};

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

fn expect<T: ufmt::uDebug + PartialEq>(msg: &str, expected: T, actual: T) {
    if expected != actual {
        let mut err = heapless::String::<64>::new();
        let _ = uwrite!(err, "{}: expected {:?}, got {:?}", msg, expected, actual);
        test_fail(&err);
    }
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let many_types = &mut INSTANCES.many_types;

    // Test initial values:
    expect("init.s0", -8, many_types.s0());
    expect("init.s1", 8, many_types.s1());
    expect("init.s2", 16, many_types.s2());
    expect("init.s3", 3721049880298531338, many_types.s3());

    expect("init.u0", 8, many_types.u0());
    expect("init.u1", 16, many_types.u1());
    expect("init.u2", 3721049880298531338, many_types.u2());
    expect("init.u3", 0xBADC_0FEE, many_types.u3());

    expect("init.bv0", 8, many_types.bv0());
    expect("init.bv1", 16, many_types.bv1());
    expect("init.bv2", 3721049880298531338, many_types.bv2());

    // Passing floats/doubles to 'expect' yields linker errors..
    expect("init.f0", true, many_types.f0() == -8.0);
    expect("init.f1", true, many_types.f1() == 8.0);
    expect("init.d0", true, many_types.d0() == -8.0);
    expect("init.d1", true, many_types.d1() == 8.0);

    expect("init.b0", true, many_types.b0());

    // Test writing values:
    many_types.set_s0(-16);
    many_types.set_s1(16);
    many_types.set_s2(32);
    many_types.set_s3(7442099760597062676);
    many_types.set_u0(16);
    many_types.set_u1(32);
    many_types.set_u2(7442099760597062676);
    many_types.set_u3(24);
    many_types.set_bv0(16);
    many_types.set_bv1(32);
    many_types.set_bv2(7442099760597062676);
    many_types.set_f0(-16.0);
    many_types.set_f1(16.0);
    many_types.set_d0(-16.0);
    many_types.set_d1(16.0);
    many_types.set_b0(false);

    // Test read back values:
    expect("rt.s0", -16, many_types.s0());
    expect("rt.s1", 16, many_types.s1());
    expect("rt.s2", 32, many_types.s2());
    expect("rt.s3", 7442099760597062676, many_types.s3());
    expect("rt.u0", 16, many_types.u0());
    expect("rt.u1", 32, many_types.u1());
    expect("rt.u2", 7442099760597062676, many_types.u2());
    expect("rt.u3", 24, many_types.u3());
    expect("rt.bv0", 16, many_types.bv0());
    expect("rt.bv1", 32, many_types.bv1());
    expect("rt.bv2", 7442099760597062676, many_types.bv2());
    expect("rt.f0", true, many_types.f0() == -16.0);
    expect("rt.f1", true, many_types.f1() == 16.0);
    expect("rt.d0", true, many_types.d0() == -16.0);
    expect("rt.d1", true, many_types.d1() == 16.0);
    expect("rt.b0", false, many_types.b0());

    test_ok();
}

#[panic_handler]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    test_result("PANIC");
}
