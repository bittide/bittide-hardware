#![no_std]
#![cfg_attr(not(test), no_main)]
#![allow(const_item_mutation)]
#![allow(clippy::empty_loop)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::{uwrite, uwriteln};

use bittide_hal::hals::nested_interconnect as hal;

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
    let mut full_msg = heapless::String::<150>::new();
    write!(full_msg, "FAIL: {}", msg).unwrap();
    test_result(&full_msg)
}

fn expect<T: ufmt::uDebug + PartialEq>(msg: &str, expected: T, actual: T) {
    if expected != actual {
        let mut err = heapless::String::<150>::new();
        uwrite!(err, "{}: expected {:?}, got {:?}", msg, expected, actual).unwrap();
        test_fail(&err);
    }
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Test data: (status_value, control_value) for each peripheral
    let test_values = [
        (0x12345678u32, 0xABCDEF01u32),
        (0xDEADBEEFu32, 0xCAFEBABEu32),
        (0x55AA55AAu32, 0xAA55AA55u32),
    ];

    // Get mutable references to all peripherals
    let mut peripherals = [
        &mut INSTANCES.unnested_peripherals_0,
        &mut INSTANCES.unnested_peripherals_1,
        &mut INSTANCES.unnested_peripherals_2,
        // &mut INSTANCES.l1_nested_peripherals_0,
        // &mut INSTANCES.l1_nested_peripherals_1,
        // &mut INSTANCES.l1_nested_peripherals_2,
        // &mut INSTANCES.l2_nested_peripherals_0,
        // &mut INSTANCES.l2_nested_peripherals_1,
        // &mut INSTANCES.l2_nested_peripherals_2,
    ];

    // Test each peripheral
    for (i, (peripheral, (status_val, control_val))) in
        peripherals.iter_mut().zip(test_values.iter()).enumerate()
    {
        let mut name_buf = heapless::String::<50>::new();

        // Read initial values (should be 0)
        write!(name_buf, "peripheral{}.status.init", i).unwrap();
        expect(&name_buf, 0u32, peripheral.status());
        name_buf.clear();

        write!(name_buf, "peripheral{}.control.init", i).unwrap();
        expect(&name_buf, 0u32, peripheral.control());
        name_buf.clear();

        // Write and read back status
        peripheral.set_status(*status_val);
        write!(name_buf, "peripheral{}.status.readback", i).unwrap();
        expect(&name_buf, *status_val, peripheral.status());
        name_buf.clear();

        // Write and read back control
        peripheral.set_control(*control_val);
        write!(name_buf, "peripheral{}.control.readback", i).unwrap();
        expect(&name_buf, *control_val, peripheral.control());
    }

    // Verify all peripherals are still accessible
    for (i, (peripheral, (status_val, _))) in
        peripherals.iter_mut().zip(test_values.iter()).enumerate()
    {
        let mut name_buf = heapless::String::<50>::new();
        write!(name_buf, "peripheral{}.status.final", i).unwrap();
        expect(&name_buf, *status_val, peripheral.status());
    }

    test_ok()
}

#[panic_handler]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    let uart = unsafe { &mut hal::DeviceInstances::new().uart };
    uwriteln!(uart, "RESULT: PANIC").unwrap();
    loop {}
}
