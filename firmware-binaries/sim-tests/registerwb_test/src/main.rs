// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]
#![allow(const_item_mutation)]
#![allow(clippy::approx_constant)]

use ufmt::{uWrite, uwrite, uwriteln};

use bittide_hal::{hals::register_wb as hal, types};
use bittide_macros::{bitvector, index, signed, unsigned};

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
    loop {
        continue;
    }
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

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let many_types = &mut INSTANCES.many_types;

    macro_rules! read_write {
        (
            $name:literal,
            $read_fn:ident,
            $init_expected:expr,
            $write_fn:ident,
            $ret_expected:expr$(,)?
        ) => {
            let name = concat!("init.", $name);
            expect(name, $init_expected, many_types.$read_fn());
            many_types.$write_fn($ret_expected);
            let name = concat!("rt.", $name);
            expect(name, $ret_expected, many_types.$read_fn());
        };

        (
            array => $name:literal,
            $read_fn:ident,
            [$($init_expected:expr),*$(,)?],
            $write_fn:ident,
            [$($ret_expected:expr),*$(,)?]$(,)?
        ) => {
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
    read_write!("s0", s0, signed!(-8, n = 8), set_s0, signed!(-16, n = 8));
    read_write!("s1", s1, signed!(8, n = 8), set_s1, signed!(16, n = 8));
    read_write!("s2", s2, signed!(16, n = 16), set_s2, signed!(32, n = 16));
    read_write!(
        "s3",
        s3,
        signed!(3721049880298531338, n = 64),
        set_s3,
        signed!(7442099760597062676, n = 64),
    );
    read_write!("s4", s4, signed!(-12, n = 8), set_s4, signed!(-13, n = 8));

    read_write!("u0", u0, unsigned!(8, n = 8), set_u0, unsigned!(16, n = 8));
    read_write!(
        "u1",
        u1,
        unsigned!(16, n = 16),
        set_u1,
        unsigned!(32, n = 16),
    );
    read_write!(
        "u2",
        u2,
        unsigned!(3721049880298531338, n = 64),
        set_u2,
        unsigned!(7442099760597062676, n = 64),
    );
    read_write!(
        "u3",
        u3,
        unsigned!(0xBADC_0FEE, n = 32),
        set_u3,
        unsigned!(24, n = 32),
    );

    read_write!(
        "bv0",
        bv0,
        bitvector!(0x08, n = 8),
        set_bv0,
        bitvector!(0x10, n = 8),
    );
    read_write!(
        "bv1",
        bv1,
        bitvector!(0x10, n = 16),
        set_bv1,
        bitvector!(0x20, n = 16),
    );
    read_write!(
        "bv2",
        bv2,
        bitvector!(0x33A3D326B2B42A0A, n = 64),
        set_bv2,
        bitvector!(0x6747A64D65685414, n = 64),
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
        [
            bitvector!(0x08, n = 8),
            bitvector!(0x16, n = 8),
            bitvector!(0x24, n = 8),
            bitvector!(0x32, n = 8),
            bitvector!(0x40, n = 8),
            bitvector!(0x4E, n = 8),
            bitvector!(0x5C, n = 8),
            bitvector!(0x6A, n = 8),
        ],
        set_v0,
        [
            bitvector!(0x10, n = 8),
            bitvector!(0x20, n = 8),
            bitvector!(0x40, n = 8),
            bitvector!(0x80, n = 8),
            bitvector!(0x03, n = 8),
            bitvector!(0x09, n = 8),
            bitvector!(0x1B, n = 8),
            bitvector!(0x51, n = 8),
        ],
    );

    // also check with iterator interface
    {
        let v0_ref = [16, 32, 64, 128, 3, 9, 27, 81].map(|n| bitvector!([n], n = 8));
        for (i, (got, expected)) in many_types
            .v0_volatile_iter()
            .zip(v0_ref.into_iter())
            .enumerate()
        {
            let mut msg = heapless::String::<64>::new();
            uwrite!(msg, "rt.v0[{:?}]", i).unwrap();
            expect(&msg, expected, got);
        }
    }

    read_write!(
        array => "v1",
        v1,
        [bitvector!(0x8, n = 64), bitvector!(0x16, n = 64), bitvector!(0x33A3D326B2B42A0A, n = 64)],
        set_v1,
        [bitvector!(0x640, n = 64), bitvector!(0xC80, n = 64), bitvector!(0x6747A64D65685414, n = 64)],
    );
    read_write!(
        array => "v2",
        v2,
        [
            [bitvector!(0x08, n = 8), bitvector!(0x16, n = 8)],
            [bitvector!(0x24, n = 8), bitvector!(0x32, n = 8)],
        ],
        set_v2,
        [
            [bitvector!(0xAB, n = 8), bitvector!(0xCD, n = 8)],
            [bitvector!(0x12, n = 8), bitvector!(0x34, n = 8)],
        ],
    );

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
        types::Either::Left(bitvector!(0x08, n = 8)),
        set_e0,
        types::Either::Right(bitvector!(0x12, n = 16)),
    );

    read_write!(
        "oi",
        oi,
        types::Maybe::Just(types::Inner {
            inner_a: bitvector!(0x16, n = 8),
            inner_b: bitvector!(0x24, n = 16),
        }),
        set_oi,
        types::Maybe::Just(types::Inner {
            inner_a: bitvector!(0x02, n = 8),
            inner_b: bitvector!(0x04, n = 16),
        }),
    );

    read_write!(
        "x2",
        x2,
        types::X2(
            bitvector!(0x08, n = 8),
            types::X3(
                bitvector!(0x10, n = 16),
                bitvector!(0x20, n = 8),
                bitvector!(0x40, n = 8)
            ),
        ),
        set_x2,
        types::X2(
            bitvector!(0x10, n = 8),
            types::X3(
                bitvector!(0x20, n = 16),
                bitvector!(0x40, n = 8),
                bitvector!(0x80, n = 8)
            ),
        ),
    );

    read_write!(
        "me0",
        me0,
        types::Maybe::Just(types::Either::Left(bitvector!(0x08, n = 8))),
        set_me0,
        types::Maybe::Just(types::Either::Right(bitvector!(0x12, n = 16))),
    );
    read_write!(
        "me1",
        me1,
        types::Maybe::Just(types::Either::Left(bitvector!(0x08, n = 16))),
        set_me1,
        types::Maybe::Just(types::Either::Right(bitvector!(0x12, n = 8))),
    );

    read_write!(
        "p0",
        p0,
        types::P0(
            unsigned!(0xBADC, n = 16),
            unsigned!(0x0F, n = 8),
            unsigned!(0xEE, n = 8)
        ),
        set_p0,
        types::P0(
            unsigned!(0x1234, n = 16),
            unsigned!(0x56, n = 8),
            unsigned!(0xFE, n = 8)
        ),
    );
    read_write!(
        "p1",
        p1,
        types::P1(
            unsigned!(0xBADC, n = 16),
            unsigned!(0x0F, n = 8),
            unsigned!(0xBEAD, n = 16)
        ),
        set_p1,
        types::P1(
            unsigned!(0x1234, n = 16),
            unsigned!(0x56, n = 8),
            unsigned!(0xFACE, n = 16)
        ),
    );
    read_write!(
        "p2",
        p2,
        types::P2(
            unsigned!(0xBADC, n = 16),
            unsigned!(0x0F, n = 8),
            index!(6, n = 10)
        ),
        set_p2,
        types::P2(
            unsigned!(0x1234, n = 16),
            unsigned!(0x56, n = 8),
            index!(9, n = 10)
        ),
    );
    read_write!(
        "p3",
        p3,
        types::P3(
            types::P2(
                unsigned!(0xBADC, n = 16),
                unsigned!(0x0F, n = 8),
                index!(6, n = 10)
            ),
            unsigned!(0xEE, n = 8),
        ),
        set_p3,
        types::P3(
            types::P2(
                unsigned!(0x1234, n = 16),
                unsigned!(0x56, n = 8),
                index!(9, n = 10)
            ),
            unsigned!(0xBA, n = 8),
        ),
    );

    read_write!(
        "t0",
        t0,
        (bitvector!(0x0C, n = 8), signed!(584, n = 16)),
        set_t0,
        (bitvector!(0x18, n = 8), signed!(-948, n = 16)),
    );

    read_write!("i20", i20, index!(17, n = 20), set_i20, index!(3, n = 20));
    read_write!(
        "mi12",
        mi12,
        types::Maybe::Just(index!(5, n = 12)),
        set_mi12,
        types::Maybe::Nothing,
    );

    read_write!(
        "maybe_b96",
        maybe_b96,
        types::Maybe::Just(bitvector!(0x4A_BA_BA_B5_55_55_55_5D_EA_DB_EE_F1, n = 96)),
        set_maybe_b96,
        types::Maybe::Nothing,
    );

    read_write!(
        "maybe_u96",
        maybe_u96,
        types::Maybe::Just(unsigned!(0x4A_BA_BA_B5_55_55_55_5D_EA_DB_EE_F1, n = 128)),
        set_maybe_u96,
        types::Maybe::Nothing,
    );

    // XXX: Writing a negative value here breaks because of lacking atomic operations.
    //      See https://github.com/bittide/bittide-hardware/issues/832.
    read_write!(
        "maybe_s96",
        maybe_s96,
        types::Maybe::Just(signed!(0x4A_BA_BA_B5_55_55_55_5D_EA_DB_EE_F1, n = 128)),
        set_maybe_s96,
        types::Maybe::Nothing,
    );

    read_write!(
        "eitherAbc",
        either_abc,
        types::Either::Left(bitvector!(0x0, n = 2)),
        set_either_abc,
        types::Either::Left(bitvector!(0x3, n = 2)),
    );

    read_write!(
        "eitherAbc",
        either_abc,
        types::Either::Left(bitvector!(0x3, n = 2)),
        set_either_abc,
        types::Either::Right(types::Abc::B),
    );

    read_write!(
        array => "only_referenced_in_vec",
        only_referenced_in_vec,
        [
            types::OnlyReferencedInVec(bitvector!(0x02, n = 8), bitvector!(0x00_03, n = 13)),
            types::OnlyReferencedInVec(bitvector!(0x04, n = 8), bitvector!(0x00_05, n = 13)),
        ],
        set_only_referenced_in_vec,
        [
            types::OnlyReferencedInVec(bitvector!(0x06, n = 8), bitvector!(0x00_07, n = 13)),
            types::OnlyReferencedInVec(bitvector!(0x08, n = 8), bitvector!(0x00_09, n = 13)),
        ],
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
