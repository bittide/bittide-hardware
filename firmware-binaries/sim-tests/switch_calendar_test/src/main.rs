// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]
#![allow(const_item_mutation)]
#![allow(clippy::empty_loop)]
#![allow(clippy::approx_constant)]

use bittide_hal::{
    index,
    manual_additions::{index::IndexTy, switch_calendar::EntryType},
    shared::types::ValidEntry,
    Index,
};
use ufmt::{uDebug, uwrite, uwriteln};

#[cfg(not(test))]
use riscv_rt::entry;

use bittide_hal::hals::switch_c as hal;

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
    let active_entry0: [Index![17]; 16] = core::array::from_fn(|i| index!(i as u8, n = 17));
    let mut active_entry1: [Index![17]; 16] = active_entry0;
    active_entry1.reverse();

    let cal_active = [
        ValidEntry {
            ve_entry: active_entry0,
            ve_repeat: 8,
        },
        ValidEntry {
            ve_entry: active_entry1,
            ve_repeat: 16,
        },
    ];

    let cal_shadow: [EntryType; 16] = core::array::from_fn(|i| ValidEntry {
        ve_entry: core::array::from_fn(|_j| unsafe { IndexTy::new_unchecked(i as u8) }),
        ve_repeat: i as u16,
    });

    let calendar = &mut INSTANCES.switch;
    // Test End of metacycle register and metacycle count
    calendar.wait_for_end_of_metacycle();
    let bootmetacycle = calendar.metacycle_count();
    calendar.wait_for_end_of_metacycle();
    let next_metacycle = calendar.metacycle_count();
    expect("Metacycle increment", 1, next_metacycle - bootmetacycle);
    //

    // Test reading shadow depth register
    let calendar_depth = calendar.shadow_depth();
    expect("Shadow depth register", cal_shadow.len(), calendar_depth);

    // Test swapping active calendar with shadow calendar
    calendar.swap_calendar();
    calendar.wait_for_end_of_metacycle();
    let calendar_depth = calendar.shadow_depth();
    expect(
        "Shadow depth register post ",
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
            "Reading shadow calendar entry {}/{}",
            i,
            calendar.shadow_depth_index().into_underlying()
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
            "Reading shadow calendar entry post swap {}/{}",
            i,
            calendar.shadow_depth_index().into_underlying()
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
            "Reading shadow calendar pre write: {}/{}",
            i,
            calendar.shadow_depth_index().into_underlying()
        );
        expect(&msg, expected[i], actual);
    }
    calendar.write_shadow_calendar(&cal_shadow);

    expect(
        "Shadow calender depth updated",
        cal_shadow.len(),
        calendar.shadow_depth(),
    );
    let expected = cal_shadow;
    for (i, actual) in calendar.read_shadow_calendar().enumerate() {
        let mut msg = heapless::String::<64>::new();
        _ = uwrite!(
            msg,
            "Reading shadow calendar post write: {}/{}",
            i,
            calendar.shadow_depth_index().into_underlying()
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
