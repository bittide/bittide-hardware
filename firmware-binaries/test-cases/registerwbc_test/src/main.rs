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
use bittide_hal::index;

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

    macro_rules! read_write {
        ($name:literal, $read_fn:ident, $init_expected:expr, $write_fn:ident, $ret_expected:expr) => {
            let name = concat!("init.", $name);
            expect(name, $init_expected, many_types.$read_fn());
            many_types.$write_fn($ret_expected);
            let name = concat!("rt.", $name);
            expect(name, $ret_expected, many_types.$read_fn());
        };

        (array => $name:literal, $read_fn:ident, [$($init_expected:expr),*], $write_fn:ident, [$($ret_expected:expr),*]) => {
            let mut i = 0;
            $(
                let mut msg = heapless::String::<64>::new();
                _ = uwrite!(msg, "init.");
                _ = uwrite!(msg, $name);
                _ = uwrite!(msg, "[{:?}]", i);
                expect(&msg, Some($init_expected), many_types.$read_fn(i));

                many_types.$write_fn(i, $ret_expected);
                msg.clear();
                _ = uwrite!(msg, "rt.");
                _ = uwrite!(msg, $name);
                _ = uwrite!(msg, "[{:?}]", i);
                expect(&msg, Some($ret_expected), many_types.$read_fn(i));
                i += 1;
            )*
            // read one extra to check for None
            let mut msg = heapless::String::<64>::new();
            _ = uwrite!(msg, "init.");
            _ = uwrite!(msg, $name);
            _ = uwrite!(msg, "[{:?}]", i);
            expect(&msg, None, many_types.$read_fn(i));

        };
    }

    // Test initial values:
    read_write!("s0", s0, -8, set_s0, -16);
    read_write!("s1", s1, 8, set_s1, 16);
    read_write!("s2", s2, 16, set_s2, 32);
    read_write!("s3", s3, 3721049880298531338, set_s3, 7442099760597062676);
    read_write!("s4", s4, -12, set_s4, -13);

    read_write!("u0", u0, 8, set_u0, 16);
    read_write!("u1", u1, 16, set_u1, 32);
    read_write!("u2", u2, 3721049880298531338, set_u2, 7442099760597062676);
    read_write!("u3", u3, 0xBADC_0FEE, set_u3, 24);

    read_write!("bv0", bv0, 8, set_bv0, 16);
    read_write!("bv1", bv1, 16, set_bv1, 32);
    read_write!(
        "bv2",
        bv2,
        3721049880298531338,
        set_bv2,
        7442099760597062676
    );

    // floats/doubles don't implement uDebug, so go through bool instead :(
    expect("init.f0", true, many_types.f0() == -8.0);
    expect("init.f1", true, many_types.f1() == 8.0);
    expect("init.d0", true, many_types.d0() == -8.0);
    expect("init.d1", true, many_types.d1() == 8.0);
    many_types.set_f0(-16.0);
    many_types.set_f1(16.0);
    many_types.set_d0(-16.0);
    many_types.set_d1(16.0);
    expect("rt.f0", true, many_types.f0() == -16.0);
    expect("rt.f1", true, many_types.f1() == 16.0);
    expect("rt.d0", true, many_types.d0() == -16.0);
    expect("rt.d1", true, many_types.d1() == 16.0);

    read_write!("b0", b0, true, set_b0, false);

    read_write!(array => "v0", v0, [0x8, 0x16, 0x24, 0x32, 0x40, 0x4E, 0x5C, 0x6A], set_v0, [16, 32, 64, 128, 3, 9, 27, 81]);

    // also check with iterator interface
    {
        let v0_ref = [16, 32, 64, 128, 3, 9, 27, 81];
        for (i, (got, expected)) in many_types
            .v0_volatile_iter()
            .zip(v0_ref.into_iter())
            .enumerate()
        {
            let mut msg = heapless::String::<64>::new();
            _ = uwrite!(msg, "rt.v0[{:?}]", i);
            expect(&msg, expected, got);
        }
    }

    read_write!(array => "v1", v1, [0x8, 0x16, 3721049880298531338], set_v1, [1600, 3200, 7442099760597062676]);
    read_write!(array => "v2", v2, [[0x8, 0x16], [0x24, 0x32]], set_v2, [[0xAB, 0xCD], [0x12, 0x34]]);

    read_write!("sum0", sum0, hal::Abc::C, set_sum0, hal::Abc::A);
    read_write!("sum1", sum1, hal::Xyz::S, set_sum1, hal::Xyz::Z);

    // floats again...
    expect("init.sop0.f", true, many_types.sop0().f == 3.14);
    expect("init.sop0.u", true, many_types.sop0().u == 6.28);
    many_types.set_sop0(hal::F { f: 1.0, u: 8.0 });
    expect("rt.sop0.f", true, many_types.sop0().f == 1.0);
    expect("rt.sop0.g", true, many_types.sop0().u == 8.0);

    read_write!(
        "e0",
        e0,
        hal::Either::Left(8),
        set_e0,
        hal::Either::Right(0x12)
    );

    read_write!(
        "oi",
        oi,
        hal::Maybe::Just(hal::Inner {
            inner_a: 0x16,
            inner_b: 0x24,
        }),
        set_oi,
        hal::Maybe::Just(hal::Inner {
            inner_a: 2,
            inner_b: 4,
        })
    );

    read_write!(
        "x2",
        x2,
        hal::X2(8, hal::X3(16, 32, 64)),
        set_x2,
        hal::X2(16, hal::X3(32, 64, 128))
    );

    read_write!(
        "me0",
        me0,
        hal::Maybe::Just(hal::Either::Left(8)),
        set_me0,
        hal::Maybe::Just(hal::Either::Right(0x12))
    );
    read_write!(
        "me1",
        me1,
        hal::Maybe::Just(hal::Either::Left(8)),
        set_me1,
        hal::Maybe::Just(hal::Either::Right(0x12))
    );

    read_write!(
        "p0",
        p0,
        hal::P0(0xBADC, 0x0F, 0xEE),
        set_p0,
        hal::P0(0x1234, 0x56, 0xFE)
    );
    read_write!(
        "p1",
        p1,
        hal::P1(0xBADC, 0x0F, 0xBEAD),
        set_p1,
        hal::P1(0x1234, 0x56, 0xFACE)
    );
    read_write!(
        "p2",
        p2,
        hal::P2(0xBADC, 0x0F, index!(6, n = 10)),
        set_p2,
        hal::P2(0x1234, 0x56, index!(9, n = 10))
    );
    read_write!(
        "p3",
        p3,
        hal::P3(hal::P2(0xBADC, 0x0F, index!(6, n = 10),), 0xEE),
        set_p3,
        hal::P3(hal::P2(0x1234, 0x56, index!(9, n = 10)), 0xBA)
    );

    read_write!(
        "t0",
        t0,
        hal::Tuple2(12, 584),
        set_t0,
        hal::Tuple2(24, -948)
    );

    read_write!("i20", i20, index!(17, n = 20), set_i20, index!(3, n = 20));
    read_write!(
        "mi12",
        mi12,
        hal::Maybe::Just(index!(5, n = 12)),
        set_mi12,
        hal::Maybe::Nothing
    );

    test_ok();
}

#[panic_handler]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    test_result("PANIC");
}
