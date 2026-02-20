#![no_std]
#![cfg_attr(not(test), no_main)]
#![allow(const_item_mutation)]
#![allow(clippy::empty_loop)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::{uWrite, uwrite, uwriteln};

use bittide_hal::hals::addressable_bytes_wb as hal;
use bittide_hal::manual_additions::AlignedArray;

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

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let uart = &mut INSTANCES.uart;
    let buffer = &mut INSTANCES.addressable_buffer;

    uwriteln!(uart, "Starting addressable_bytes_wb_test").unwrap();

    // Test data: 8 words with distinct patterns
    let test_data: [[u8; 4]; 8] = [
        [0x00, 0x10, 0x20, 0x30],
        [0x01, 0x11, 0x21, 0x31],
        [0x02, 0x12, 0x22, 0x32],
        [0x03, 0x13, 0x23, 0x33],
        [0x04, 0x14, 0x24, 0x34],
        [0x05, 0x15, 0x25, 0x35],
        [0x06, 0x16, 0x26, 0x36],
        [0x07, 0x17, 0x27, 0x37],
    ];

    // Test 1: Word-based access for whole memory
    uwriteln!(uart, "Test 1: Word-based access").unwrap();
    for (i, &word) in test_data.iter().enumerate() {
        buffer.set_buffer(i, word);
    }

    for (i, &expected) in test_data.iter().enumerate() {
        expect("word access", Some(expected), buffer.buffer(i));
    }

    buffer.clear();
    for i in 0..8 {
        expect("Test 1 clear", Some([0u8; 4]), buffer.buffer(i));
    }
    uwriteln!(uart, "  PASS").unwrap();

    // Create aligned test data
    let test_data = AlignedArray::<32>::from_bytes([
        0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7, 0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6,
        0xB7, 0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5,
        0xD6, 0xD7,
    ]);

    // Test 2: Aligned byte slices
    uwriteln!(uart, "Test 2: Byte slice - various sizes").unwrap();

    for size in 1..=8 {
        uwriteln!(uart, "  Testing size: {}", size).unwrap();
        // Write slice of test_data
        buffer.write_slice(&test_data.as_slice()[0..size], 0);

        // Read back and verify
        let mut verify = AlignedArray::<32>::new();
        buffer.read_slice(&mut verify.as_slice_mut()[0..size], 0);

        for (i, &expected) in test_data.as_slice().iter().enumerate().take(size) {
            expect("byte slice", expected, verify.as_bytes()[i]);
        }

        buffer.clear();
        for i in 0..8 {
            expect("Test 2 clear", Some([0u8; 4]), buffer.buffer(i));
        }
    }

    // Test3: Unaligned byte slices
    for alignment in 1..=3 {
        for size in 1..=4 {
            for offset in 1..=3 {
                // Safe guard against out-of-bounds writes
                if alignment + size + offset > 32 {
                    continue;
                }

                buffer.write_slice(&test_data.as_slice()[alignment..alignment + size], offset);

                for i in 0..size {
                    let expected = test_data.as_slice()[alignment + i];
                    let actual = unsafe { buffer.as_slice() }[offset + i];
                    expect("unaligned byte slice", expected, actual);
                }

                buffer.clear();
                for i in 0..8 {
                    expect("Test 3 clear", Some([0u8; 4]), buffer.buffer(i));
                }
            }
        }
    }
    uwriteln!(uart, "  PASS").unwrap();

    test_ok()
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
