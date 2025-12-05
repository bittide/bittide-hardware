#![no_std]
#![cfg_attr(not(test), no_main)]
#![allow(const_item_mutation)]
#![allow(clippy::empty_loop)]
#![allow(clippy::approx_constant)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use ufmt::{uWrite, uwrite, uwriteln};

use bittide_hal::hals::register_wb as hal;
use bittide_hal::types;

use core::fmt::Write;
#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: hal::DeviceInstances = unsafe { hal::DeviceInstances::new() };

trait WriterAny: uWrite<Error = ()> + Write {}

impl<T: uWrite<Error = ()> + Write> WriterAny for T {}

fn test_result<F>(result: F) -> !
where
    F: FnOnce(&mut dyn WriterAny),
{
    let uart = &mut INSTANCES.uart;
    uwrite!(uart, "RESULT: ").unwrap();
    result(uart);
    loop {}
}

fn test_ok() -> ! {
    test_result(|w| uwriteln!(w, "OK").unwrap())
}

fn test_fail(msg: impl FnOnce(&mut dyn WriterAny)) -> ! {
    test_result(|w| {
        uwrite!(w, "FAIL: ").unwrap();
        msg(w);
    });
}

fn expect<T: ufmt::uDebug + PartialEq>(msg: &str, expected: T, actual: T) {
    if expected != actual {
        test_fail(|w| {
            uwriteln!(w, "{}: expected {:?}, got {:?}", msg, expected, actual).unwrap();
        });
    }
}

fn bv16(v: u16) -> [u8; 2] {
    v.to_le_bytes()
}
fn bv64(v: u64) -> [u8; 8] {
    v.to_le_bytes()
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
                let mut msg = heapless::String::<150>::new();
                uwrite!(msg, "init.").unwrap();
                uwrite!(msg, $name).unwrap();
                uwrite!(msg, "[{:?}]", i).unwrap();
                expect(&msg, Some($init_expected), many_types.$read_fn(i));

                many_types.$write_fn(i, $ret_expected);
                msg.clear();
                uwrite!(msg, "rt.").unwrap();
                uwrite!(msg, $name).unwrap();
                uwrite!(msg, "[{:?}]", i).unwrap();
                expect(&msg, Some($ret_expected), many_types.$read_fn(i));
                i += 1;
            )*
            // read one extra to check for None
            let mut msg = heapless::String::<150>::new();
            uwrite!(msg, "init.").unwrap();
            uwrite!(msg, $name).unwrap();
            uwrite!(msg, "[{:?}]", i).unwrap();
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

    read_write!("bv0", bv0, [8], set_bv0, [16]);
    read_write!("bv1", bv1, bv16(16), set_bv1, bv16(32));
    read_write!(
        "bv2",
        bv2,
        bv64(3721049880298531338),
        set_bv2,
        bv64(7442099760597062676)
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

    read_write!(
        array => "v0",
        v0,
        [[0x8], [0x16], [0x24], [0x32], [0x40], [0x4E], [0x5C], [0x6A]],
        set_v0,
        [[16], [32], [64], [128], [3], [9], [27], [81]]
    );

    // also check with iterator interface
    {
        let v0_ref = [16, 32, 64, 128, 3, 9, 27, 81];
        for (i, (got, expected)) in many_types
            .v0_volatile_iter()
            .zip(v0_ref.into_iter())
            .enumerate()
        {
            let mut msg = heapless::String::<64>::new();
            uwrite!(msg, "rt.v0[{:?}]", i).unwrap();
            expect(&msg, [expected], got);
        }
    }

    read_write!(array => "v1", v1, [bv64(0x8), bv64(0x16), bv64(3721049880298531338u64)], set_v1, [bv64(1600), bv64(3200), bv64(7442099760597062676)]);
    read_write!(array => "v2", v2, [[[0x8], [0x16]], [[0x24], [0x32]]], set_v2, [[[0xAB], [0xCD]], [[0x12], [0x34]]]);

    expect("init.unitW", false, many_types.unit_w());
    many_types.set_unit(());
    expect("rt.unitW", true, many_types.unit_w());

    many_types.zs();
    many_types.set_zs(types::MyZeroSizedType);

    read_write!("sum0", sum0, types::Abc::C, set_sum0, types::Abc::A);
    read_write!("sum1", sum1, types::Xyz::S, set_sum1, types::Xyz::Z);

    // floats again...
    expect("init.sop0.f", true, many_types.sop0().f == 3.14);
    expect("init.sop0.u", true, many_types.sop0().u == 6.28);
    many_types.set_sop0(types::F { f: 1.0, u: 8.0 });
    expect("rt.sop0.f", true, many_types.sop0().f == 1.0);
    expect("rt.sop0.g", true, many_types.sop0().u == 8.0);

    read_write!(
        "e0",
        e0,
        types::Either::Left([8]),
        set_e0,
        types::Either::Right(bv16(0x12))
    );

    read_write!(
        "oi",
        oi,
        types::Maybe::Just(types::Inner {
            inner_a: [0x16],
            inner_b: bv16(0x24),
        }),
        set_oi,
        types::Maybe::Just(types::Inner {
            inner_a: [2],
            inner_b: bv16(4),
        })
    );

    read_write!(
        "x2",
        x2,
        types::X2([8], types::X3(bv16(16), [32], [64])),
        set_x2,
        types::X2([16], types::X3(bv16(32), [64], [128]))
    );

    read_write!(
        "me0",
        me0,
        types::Maybe::Just(types::Either::Left([8])),
        set_me0,
        types::Maybe::Just(types::Either::Right(bv16(0x12)))
    );
    read_write!(
        "me1",
        me1,
        types::Maybe::Just(types::Either::Left(bv16(8))),
        set_me1,
        types::Maybe::Just(types::Either::Right([0x12]))
    );

    read_write!(
        "p0",
        p0,
        types::P0(0xBADC, 0x0F, 0xEE),
        set_p0,
        types::P0(0x1234, 0x56, 0xFE)
    );
    read_write!(
        "p1",
        p1,
        types::P1(0xBADC, 0x0F, 0xBEAD),
        set_p1,
        types::P1(0x1234, 0x56, 0xFACE)
    );
    read_write!(
        "p2",
        p2,
        types::P2(0xBADC, 0x0F, 6),
        set_p2,
        types::P2(0x1234, 0x56, 9)
    );
    read_write!(
        "p3",
        p3,
        types::P3(types::P2(0xBADC, 0x0F, 6,), 0xEE),
        set_p3,
        types::P3(types::P2(0x1234, 0x56, 9), 0xBA)
    );

    read_write!("t0", t0, ([12], 584), set_t0, ([24], -948));

    read_write!("i20", i20, 17, set_i20, 3);
    read_write!(
        "mi12",
        mi12,
        types::Maybe::Just(5),
        set_mi12,
        types::Maybe::Nothing
    );

    read_write!(
        "maybe_b96",
        maybe_b96,
        types::Maybe::Just([
            0xF1, 0xEE, 0xDB, 0xEA, 0x5D, 0x55, 0x55, 0x55, 0xB5, 0xBA, 0xBA, 0x4A
        ]),
        set_maybe_b96,
        types::Maybe::Nothing
    );

    read_write!(
        "maybe_u96",
        maybe_u96,
        types::Maybe::Just(0x4ABABAB55555555DEADBEEF1),
        set_maybe_u96,
        types::Maybe::Nothing
    );

    // XXX: Writing a negative value here breaks because of lacking atomic operations.
    //      See https://github.com/bittide/bittide-hardware/issues/832.
    read_write!(
        "maybe_s96",
        maybe_s96,
        types::Maybe::Just(0x4ABABAB55555555DEADBEEF1),
        set_maybe_s96,
        types::Maybe::Nothing
    );

    read_write!(
        "eitherAbc",
        either_abc,
        types::Either::Left([0]),
        set_either_abc,
        types::Either::Left([0b11])
    );

    read_write!(
        "eitherAbc",
        either_abc,
        types::Either::Left([0b11]),
        set_either_abc,
        types::Either::Right(types::Abc::B)
    );

    read_write!(
        array => "only_referenced_in_vec",
        only_referenced_in_vec,
        [types::OnlyReferencedInVec([2], bv16(3)), types::OnlyReferencedInVec([4], bv16(5))],
        set_only_referenced_in_vec,
        [types::OnlyReferencedInVec([6], bv16(7)), types::OnlyReferencedInVec([8], bv16(9))]
    );

    test_ok();
}

#[panic_handler]
fn panic_handler(info: &core::panic::PanicInfo) -> ! {
    test_fail(|w| {
        let mut any_write_errors = false;
        if let Some(location) = info.location() {
            any_write_errors |=
                uwrite!(w, "PANIC at {}:{}: ", location.file(), location.line()).is_err();
        } else {
            any_write_errors |= uwrite!(w, "PANIC: ").is_err();
        }

        any_write_errors |= writeln!(w, "{}", info.message()).is_err();

        if any_write_errors {
            _ = writeln!(w, "recursive panic");
        }
    });
}
