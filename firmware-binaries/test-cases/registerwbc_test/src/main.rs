#![no_std]
#![cfg_attr(not(test), no_main)]
#![allow(const_item_mutation)]
#![allow(clippy::empty_loop)]
#![allow(clippy::approx_constant)]
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
    expect("init.s4", -12, many_types.s4());

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

    expect("init.v0[0]", Some(0x8), many_types.v0(0));
    expect("init.v0[1]", Some(0x16), many_types.v0(1));
    expect("init.v0[2]", Some(0x24), many_types.v0(2));
    expect("init.v0[3]", Some(0x32), many_types.v0(3));
    expect("init.v0[4]", Some(0x40), many_types.v0(4));
    expect("init.v0[5]", Some(0x4E), many_types.v0(5));
    expect("init.v0[6]", Some(0x5C), many_types.v0(6));
    expect("init.v0[7]", Some(0x6A), many_types.v0(7));
    expect(
        "init.v0_iter.count",
        8,
        many_types.v0_volatile_iter().count(),
    );

    // also check with iterator interface
    {
        let v0_ref = [0x8, 0x16, 0x24, 0x32, 0x40, 0x4E, 0x5C, 0x6A];
        for (i, (got, expected)) in many_types
            .v0_volatile_iter()
            .zip(v0_ref.into_iter())
            .enumerate()
        {
            let mut msg = heapless::String::<64>::new();
            _ = uwrite!(msg, "init.v0[{:?}]", i);
            expect(&msg, expected, got);
        }
    }

    expect("init.v1[0]", Some(0x8), many_types.v1(0));
    expect("init.v1[1]", Some(0x16), many_types.v1(1));
    expect("init.v1[2]", Some(3721049880298531338), many_types.v1(2));

    expect("init.v2[0]", Some([0x8, 0x16]), many_types.v2(0));
    expect("init.v2[1]", Some([0x24, 0x32]), many_types.v2(1));

    // XXX: No uDebug trait for enums, so we can't use expect() here.
    expect("init.sum0", true, hal::Abc::C == many_types.sum0());
    expect("init.sum1", true, hal::Xyz::S == many_types.sum1());

    expect("init.sop0.f", true, many_types.sop0().f == 3.14);
    expect("init.sop0.u", true, many_types.sop0().u == 6.28);

    expect("init.p0.0", 0xBADC, many_types.p0().0);
    expect("init.p0.1", 0x0F, many_types.p0().1);
    expect("init.p0.2", 0xEE, many_types.p0().2);
    expect("init.p1.0", 0xBADC, many_types.p1().0);
    expect("init.p1.1", 0x0F, many_types.p1().1);
    expect("init.p1.2", 0xBEAD, many_types.p1().2);
    expect("init.p2.0", 0xBADC, many_types.p2().0);
    expect("init.p2.1", 0x0F, many_types.p2().1);
    expect("init.p3.0.0", 0xBADC, many_types.p3().0 .0);
    expect("init.p3.0.1", 0x0F, many_types.p3().0 .1);
    expect("init.p3.1", 0xEE, many_types.p3().1);

    expect("init.e0", true, hal::Either::Left(8) == many_types.e0());
    expect(
        "init.me0",
        true,
        hal::Maybe::Just(hal::Either::Left(8)) == many_types.me0(),
    );
    expect(
        "init.me1",
        true,
        hal::Maybe::Just(hal::Either::Left(8)) == many_types.me1(),
    );

    expect("init.t0.0", 12, many_types.t0().0);
    expect("init.t0.1", 584, many_types.t0().1);

    // Test writing values:
    many_types.set_s0(-16);
    many_types.set_s1(16);
    many_types.set_s2(32);
    many_types.set_s3(7442099760597062676);
    many_types.set_s4(-13);
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
    many_types.set_v0(0, 16).unwrap();
    many_types.set_v0(1, 32).unwrap();
    many_types.set_v0(2, 64).unwrap();
    many_types.set_v0(3, 128).unwrap();
    many_types.set_v0(4, 3).unwrap();
    many_types.set_v0(5, 9).unwrap();
    many_types.set_v0(6, 27).unwrap();
    many_types.set_v0(7, 81).unwrap();
    many_types.set_v1(0, 1600).unwrap();
    many_types.set_v1(1, 3200).unwrap();
    many_types.set_v1(2, 7442099760597062676).unwrap();
    many_types.set_v2(0, [0xAB, 0xCD]).unwrap();
    many_types.set_v2(1, [0x12, 0x34]).unwrap();
    many_types.set_sum0(hal::Abc::A);
    many_types.set_sum1(hal::Xyz::Z);
    many_types.set_sop0(hal::F { f: 1.0, u: 8.0 });
    many_types.set_x2(hal::X2(8, hal::X3(16, 32, 64)));
    many_types.set_e0(hal::Either::Left(0x12));
    many_types.set_me0(hal::Maybe::Just(hal::Either::Right(0x12)));
    many_types.set_me1(hal::Maybe::Just(hal::Either::Right(0x12)));
    many_types.set_t0(hal::Pair(24, -948));

    // Test read back values:
    expect("rt.s0", -16, many_types.s0());
    expect("rt.s1", 16, many_types.s1());
    expect("rt.s2", 32, many_types.s2());
    expect("rt.s3", 7442099760597062676, many_types.s3());
    expect("rt.s4", -13, many_types.s4());
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
    expect("rt.v0[0]", Some(16), many_types.v0(0));
    expect("rt.v0[1]", Some(32), many_types.v0(1));
    expect("rt.v0[2]", Some(64), many_types.v0(2));
    expect("rt.v0[3]", Some(128), many_types.v0(3));
    expect("rt.v0[4]", Some(3), many_types.v0(4));
    expect("rt.v0[5]", Some(9), many_types.v0(5));
    expect("rt.v0[6]", Some(27), many_types.v0(6));
    expect("rt.v0[7]", Some(81), many_types.v0(7));
    expect("rt.v1[0]", Some(1600), many_types.v1(0));
    expect("rt.v1[1]", Some(3200), many_types.v1(1));
    expect("rt.v1[2]", Some(7442099760597062676), many_types.v1(2));
    expect("rt.v2[0]", Some([0xAB, 0xCD]), many_types.v2(0));
    expect("rt.v2[1]", Some([0x12, 0x34]), many_types.v2(1));
    expect("rt.sum0", true, hal::Abc::A == many_types.sum0());
    expect("rt.sum1", true, hal::Xyz::Z == many_types.sum1());
    expect("rt.sop0.f", true, many_types.sop0().f == 1.0);
    expect("rt.sop0.g", true, many_types.sop0().u == 8.0);
    expect("rt.v2[0]", Some([0xAB, 0xCD]), many_types.v2(0));
    expect("rt.v2[1]", Some([0x12, 0x34]), many_types.v2(1));
    expect("rt.sum0", true, hal::Abc::A == many_types.sum0());
    expect("rt.sum1", true, hal::Xyz::Z == many_types.sum1());
    expect("rt.x2.0", 8, many_types.x2().0);
    expect("rt.x2.1.0", 16, many_types.x2().1 .0);
    expect("rt.x2.1.1", 32, many_types.x2().1 .1);
    expect("rt.e0", true, hal::Either::Left(0x12) == many_types.e0());
    expect(
        "rt.me0",
        true,
        hal::Maybe::Just(hal::Either::Right(0x12)) == many_types.me0(),
    );
    expect(
        "rt.me1",
        true,
        hal::Maybe::Just(hal::Either::Right(0x12)) == many_types.me1(),
    );

    expect("rt.t0.0", 24, many_types.t0().0);
    expect("rt.t0.1", -948, many_types.t0().1);

    many_types.set_e0(hal::Either::Right(0x12));
    expect("rt.e0", true, hal::Either::Right(0x12) == many_types.e0());

    many_types.set_x2(hal::X2(8, hal::X3(16, 32, 64)));
    expect("rt.x2.1.2", 64, many_types.x2().1 .2);

    test_ok();
}

#[panic_handler]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    test_result("PANIC");
}
