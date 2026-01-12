// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use bittide_hal::ringbuffer_test::DeviceInstances;
use core::fmt::Write;
#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

/// The ringbuffer size in 64-bit words
const RINGBUFFER_SIZE: usize = 16;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;

    writeln!(uart, "=== Ringbuffer Component Test ===").unwrap();

    let tx_ringbuffer = INSTANCES.transmit_ringbuffer;
    let rx_ringbuffer = INSTANCES.receive_ringbuffer;

    // Test 1: Write pattern to TX buffer
    writeln!(uart, "\n--- Test 1: Write and read pattern ---").unwrap();

    const TEST_SIZE: usize = 4;
    let test_pattern: [[u8; 8]; TEST_SIZE] =
        core::array::from_fn(|i| (0x1000 + i as u64).to_le_bytes());

    writeln!(uart, "Writing test pattern to TX buffer").unwrap();
    for (i, word) in test_pattern.iter().enumerate() {
        tx_ringbuffer
            .set_transmit_buffer(i, *word)
            .expect("TX write failed");
    }

    // Wait for data to propagate through the ringbuffer (need ~16 cycles for full rotation)
    for _ in 0..1000 {
        unsafe { core::arch::asm!("nop") };
    }

    writeln!(uart, "Reading from RX buffer").unwrap();
    let mut rx_data: [[u8; 8]; TEST_SIZE] = [[0u8; 8]; TEST_SIZE];
    for i in 0..TEST_SIZE {
        rx_data[i] = rx_ringbuffer.receive_buffer(i).expect("RX read failed");
    }

    let mut test1_passed = true;
    for i in 0..TEST_SIZE {
        if rx_data[i] != test_pattern[i] {
            writeln!(
                uart,
                "MISMATCH at index {}: expected {:02x?}, got {:02x?}",
                i, test_pattern[i], rx_data[i]
            )
            .unwrap();
            test1_passed = false;
        }
    }

    if test1_passed {
        writeln!(uart, "Test 1 PASSED: Data matches!").unwrap();
    } else {
        writeln!(uart, "Test 1 FAILED: Data mismatch").unwrap();
    }

    // Test 2: Write different pattern and verify
    writeln!(uart, "\n--- Test 2: Second pattern test ---").unwrap();

    let test_pattern2: [[u8; 8]; TEST_SIZE] =
        core::array::from_fn(|i| (0x2000 + i as u64).to_le_bytes());

    writeln!(uart, "Writing second pattern to TX buffer").unwrap();
    for (i, word) in test_pattern2.iter().enumerate() {
        tx_ringbuffer
            .set_transmit_buffer(i, *word)
            .expect("TX write failed");
    }

    // Wait for data to propagate
    for _ in 0..1000 {
        unsafe { core::arch::asm!("nop") };
    }

    writeln!(uart, "Reading from RX buffer").unwrap();
    let mut rx_data2: [[u8; 8]; TEST_SIZE] = [[0u8; 8]; TEST_SIZE];
    for i in 0..TEST_SIZE {
        rx_data2[i] = rx_ringbuffer.receive_buffer(i).expect("RX read failed");
    }

    let mut test2_passed = true;
    for i in 0..TEST_SIZE {
        if rx_data2[i] != test_pattern2[i] {
            writeln!(
                uart,
                "MISMATCH at index {}: expected {:02x?}, got {:02x?}",
                i, test_pattern2[i], rx_data2[i]
            )
            .unwrap();
            test2_passed = false;
        }
    }

    if test2_passed {
        writeln!(uart, "Test 2 PASSED: Data matches!").unwrap();
    } else {
        writeln!(uart, "Test 2 FAILED: Data mismatch").unwrap();
    }

    // Test 3: Full buffer test
    writeln!(uart, "\n--- Test 3: Full buffer test ---").unwrap();

    writeln!(uart, "Writing full buffer pattern to TX").unwrap();
    for i in 0..RINGBUFFER_SIZE {
        let value = (0x3000 + i as u64).to_le_bytes();
        tx_ringbuffer
            .set_transmit_buffer(i, value)
            .expect("TX write failed");
    }

    // Wait for full buffer to propagate
    for _ in 0..2000 {
        unsafe { core::arch::asm!("nop") };
    }

    writeln!(uart, "Reading full buffer from RX").unwrap();
    let mut test3_passed = true;
    for i in 0..RINGBUFFER_SIZE {
        let expected = (0x3000 + i as u64).to_le_bytes();
        let actual = rx_ringbuffer.receive_buffer(i).expect("RX read failed");
        if actual != expected {
            writeln!(
                uart,
                "MISMATCH at index {}: expected {:02x?}, got {:02x?}",
                i, expected, actual
            )
            .unwrap();
            test3_passed = false;
        }
    }

    if test3_passed {
        writeln!(uart, "Test 3 PASSED: Full buffer matches!").unwrap();
    } else {
        writeln!(uart, "Test 3 FAILED: Buffer mismatch").unwrap();
    }

    // Final result
    writeln!(uart, "\n=== Test Summary ===").unwrap();
    if test1_passed && test2_passed && test3_passed {
        writeln!(uart, "*** ALL TESTS PASSED ***").unwrap();
    } else {
        writeln!(uart, "*** SOME TESTS FAILED ***").unwrap();
    }

    loop {
        continue;
    }
}

#[panic_handler]
fn panic_handler(info: &core::panic::PanicInfo) -> ! {
    let mut uart = INSTANCES.uart;
    writeln!(uart, "Panicked! #{info}").unwrap();
    loop {
        continue;
    }
}
