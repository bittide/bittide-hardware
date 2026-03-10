// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use bittide_hal::{
    manual_additions::{ringbuffer_test::ringbuffers::AlignedReceiveBuffer, timer::Duration},
    ringbuffer_test::{devices::TransmitRingbuffer, DeviceInstances},
};
use core::fmt::Write;
#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    let timer = INSTANCES.timer;

    writeln!(uart, "=== Aligned Ringbuffer Test ===").unwrap();

    let tx_ringbuffer = INSTANCES.transmit_ringbuffer_0;
    let rx_ringbuffer = INSTANCES.receive_ringbuffer_0;

    // Step 1: Perform alignment procedure
    writeln!(uart, "\n--- Step 1: Alignment Discovery ---").unwrap();
    writeln!(uart, "Running alignment procedure...").unwrap();

    // Create a copy of rx_ringbuffer for alignment, then get back the aligned buffer
    let rx_copy =
        unsafe { bittide_hal::ringbuffer_test::devices::ReceiveRingbuffer::new(rx_ringbuffer.0) };
    let mut rx_aligned = AlignedReceiveBuffer::new(rx_copy);
    rx_aligned.align(&tx_ringbuffer);
    let alignment_offset = rx_aligned
        .get_alignment_offset()
        .expect("Failed to discover alignment offset");

    writeln!(
        uart,
        "SUCCESS: Discovered alignment offset = {} words ({} bytes)",
        alignment_offset,
        alignment_offset * 8
    )
    .unwrap();

    // Step 2: Test aligned transmission
    writeln!(uart, "\n--- Step 2: Aligned Transmission Test ---").unwrap();
    writeln!(uart, "Writing pattern and verifying with alignment").unwrap();

    // Clear TX buffer
    tx_ringbuffer.clear();

    // Create pattern: each word = frame number
    let tx_pattern: [[u8; 8]; TransmitRingbuffer::DATA_LEN] =
        core::array::from_fn(|i| (0x1000 + i as u64).to_le_bytes());

    writeln!(
        uart,
        "Writing {} words to TX at offset 0",
        TransmitRingbuffer::DATA_LEN
    )
    .unwrap();
    tx_ringbuffer.write_slice(&tx_pattern, 0);

    // Wait for data to propagate
    timer.wait(Duration::from_cycles(
        TransmitRingbuffer::DATA_LEN as u32,
        timer.frequency(),
    ));

    // Read using aligned buffer
    let mut rx_data: [[u8; 8]; TransmitRingbuffer::DATA_LEN] =
        [[0u8; 8]; TransmitRingbuffer::DATA_LEN];
    writeln!(
        uart,
        "Reading {} words from RX at offset 0 (with alignment)",
        TransmitRingbuffer::DATA_LEN
    )
    .unwrap();
    rx_aligned.read_slice(&mut rx_data, 0);

    // Verify all words match
    let mut all_match = true;
    let mut first_mismatch = None;

    for (i, (expected, actual)) in tx_pattern.iter().zip(rx_data.iter()).enumerate() {
        if expected != actual && first_mismatch.is_none() {
            first_mismatch = Some((i, expected, actual));
            all_match = false;
        }
    }

    if all_match {
        writeln!(
            uart,
            "*** TEST PASSED: All {} words matched with alignment! ***",
            TransmitRingbuffer::DATA_LEN
        )
        .unwrap();
    } else {
        writeln!(uart, "\n*** TEST FAILED: Data corruption detected ***").unwrap();

        if let Some((idx, expected, actual)) = first_mismatch {
            writeln!(
                uart,
                "First mismatch at word {}: expected {:02x?}, got {:02x?}",
                idx, expected, actual
            )
            .unwrap();
        }

        writeln!(uart, "\nTX pattern written:").unwrap();
        for (i, word) in tx_pattern.iter().enumerate() {
            write!(uart, "  TX[{:2}]: ", i).unwrap();
            for byte in word {
                write!(uart, "{:02x} ", byte).unwrap();
            }
            writeln!(uart).unwrap();
        }

        writeln!(
            uart,
            "\nRX pattern received (with alignment offset {}):",
            alignment_offset
        )
        .unwrap();
        for (i, (expected, actual)) in tx_pattern.iter().zip(rx_data.iter()).enumerate() {
            write!(uart, "  RX[{:2}]: ", i).unwrap();
            for byte in actual {
                write!(uart, "{:02x} ", byte).unwrap();
            }
            writeln!(uart, "{}", if expected == actual { "✓" } else { "✗" }).unwrap();
        }
    }

    writeln!(uart, "\n=== Test Summary ===").unwrap();
    writeln!(uart, "Alignment discovery: PASS").unwrap();
    writeln!(
        uart,
        "Aligned transmission: {}",
        if all_match { "PASS" } else { "FAIL" }
    )
    .unwrap();

    if all_match {
        writeln!(uart, "\n*** ALL TESTS PASSED ***").unwrap();
    } else {
        writeln!(uart, "\n*** SOME TESTS FAILED ***").unwrap();
    }
    writeln!(uart, "Test done").unwrap();

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
