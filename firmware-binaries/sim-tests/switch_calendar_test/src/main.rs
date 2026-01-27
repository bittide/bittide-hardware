// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]
#![allow(const_item_mutation)]
#![allow(clippy::empty_loop)]
#![allow(clippy::approx_constant)]

// Non-aliased imports
use bittide_hal::{
    hals::switch_c as hal,
    manual_additions::calendar::{CalendarEntryType, CalendarType},
    types::ValidEntry_12,
};
use ufmt::{uDebug, uwrite, uwriteln};

#[cfg(not(test))]
use riscv_rt::entry;

// Aliased imports
use hal::devices::Calendar;

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
    let mut full_msg = heapless::String::<1024>::new();
    let _ = uwrite!(full_msg, "FAIL: {}", msg);
    test_result(&full_msg)
}

fn expect<T: uDebug + PartialEq>(msg: &str, expected: T, actual: T) {
    if expected != actual {
        let mut err = heapless::String::<1024>::new();
        let _ = uwrite!(err, "{}: expected {:?}, got {:?}", msg, expected, actual);
        test_fail(&err);

        // Uncomment below to print progress messages
        // } else {
        //     uwriteln!(INSTANCES.uart, "PASS: {} | {:?}", msg, expected).unwrap();
    }
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let active_entry0: [u8; 16] = core::array::from_fn(|i| i as u8);
    let mut active_entry1: [u8; 16] = active_entry0;
    active_entry1.reverse();

    let cal_active = [
        ValidEntry_12 {
            ve_entry: active_entry0,
            ve_repeat: 8,
        },
        ValidEntry_12 {
            ve_entry: active_entry1,
            ve_repeat: 16,
        },
    ];

    let cal_shadow: [CalendarEntryType<Calendar>; 16] = core::array::from_fn(|i| ValidEntry_12 {
        ve_entry: core::array::from_fn(|_j| i as u8),
        ve_repeat: i as u16,
    });

    let calendar = &mut INSTANCES.switch_calendar;
    // Test End of metacycle register and metacycle count
    calendar.wait_for_end_of_metacycle();
    let bootmetacycle = calendar.metacycle_count();
    calendar.wait_for_end_of_metacycle();
    let next_metacycle = calendar.metacycle_count();
    expect("(1) Metacycle increment", 1, next_metacycle - bootmetacycle);
    //

    // Test reading shadow depth register
    let calendar_depth = calendar.shadow_depth();
    expect(
        "(2) Shadow depth register",
        cal_shadow.len(),
        calendar_depth,
    );

    // Test swapping active calendar with shadow calendar
    calendar.swap_calendar();
    calendar.wait_for_end_of_metacycle();
    let calendar_depth = calendar.shadow_depth();
    expect(
        "(3) Shadow depth register post ",
        cal_active.len(),
        calendar_depth,
    );
    calendar.swap_calendar();
    calendar.wait_for_end_of_metacycle();
    //

    // Test reading shadow calendar entries
    let expected = cal_shadow;
    for (i, entry) in calendar.read_shadow_calendar().enumerate() {
        let mut msg = heapless::String::<64>::new();
        _ = uwrite!(
            msg,
            "(4) Reading shadow calendar entry {}/{}",
            i,
            calendar.shadow_depth_index()
        );
        expect(&msg, expected[i], entry);
    }
    calendar.swap_calendar();
    calendar.wait_for_end_of_metacycle();

    let expected = cal_active;
    for (i, entry) in calendar.read_shadow_calendar().enumerate() {
        let mut msg = heapless::String::<64>::new();
        _ = uwrite!(
            msg,
            "(5) Reading shadow calendar entry post swap {}/{}",
            i,
            calendar.shadow_depth_index(),
        );
        expect(&msg, expected[i], entry);
    }
    //

    // Test writing the shadow calendar
    let expected = cal_active;
    for (i, actual) in calendar.read_shadow_calendar().enumerate() {
        let mut msg = heapless::String::<64>::new();
        _ = uwrite!(
            msg,
            "(6) Reading shadow calendar pre write: {}/{}",
            i,
            calendar.shadow_depth_index(),
        );
        expect(&msg, expected[i], actual);
    }
    calendar.write_shadow_calendar(&cal_shadow);

    expect(
        "(7) Shadow calendar depth updated",
        cal_shadow.len(),
        calendar.shadow_depth(),
    );
    let expected = cal_shadow;
    for (i, actual) in calendar.read_shadow_calendar().enumerate() {
        let mut msg = heapless::String::<64>::new();
        _ = uwrite!(
            msg,
            "(8) Reading shadow calendar post write: {}/{}",
            i,
            calendar.shadow_depth_index(),
        );
        expect(&msg, expected[i], actual);
    }
    //

    test_ok();
}

#[panic_handler]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    test_result("PANIC");
}
