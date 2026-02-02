// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use bittide_hal::{
    manual_additions::aligned_ringbuffer::{ReceiveRingbuffer, TransmitRingbuffer},
    scatter_gather_pe::DeviceInstances,
    shared_devices::Uart,
    types::ValidEntry_12,
};
use core::fmt::Write;
#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

/// The ringbuffer size in 64-bit words (must match scatter/gather memory size)
const RINGBUFFER_SIZE: usize = 16;

/// Initialize scatter and gather calendars with incrementing counter entries.
/// Each calendar entry has a duration of 0 (no repeat), creating a ringbuffer
/// pattern where index N in TX maps to index N in RX.
fn initialize_calendars(uart: &mut Uart) {
    writeln!(uart, "Initializing scatter/gather calendars").unwrap();

    let scatter_calendar = INSTANCES.scatter_calendar;
    let gather_calendar = INSTANCES.gather_calendar;

    // Write incrementing entries to both calendars
    for n in 0..RINGBUFFER_SIZE {
        let entry = ValidEntry_12 {
            ve_entry: n as u8,
            ve_repeat: 0,
        };

        scatter_calendar.set_shadow_entry(entry);
        scatter_calendar.set_write_addr(n as u8);

        gather_calendar.set_shadow_entry(entry);
        gather_calendar.set_write_addr(n as u8);
    }

    // Set the depth (max index) for both calendars
    scatter_calendar.set_shadow_depth_index((RINGBUFFER_SIZE - 1) as u8);
    gather_calendar.set_shadow_depth_index((RINGBUFFER_SIZE - 1) as u8);

    // Activate the new calendar configurations
    scatter_calendar.set_swap_active(true);
    gather_calendar.set_swap_active(true);

    writeln!(
        uart,
        "Calendars initialized with {} entries",
        RINGBUFFER_SIZE
    )
    .unwrap();
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    let scatter_unit = INSTANCES.scatter_unit;
    let gather_unit = INSTANCES.gather_unit;

    writeln!(uart, "=== Aligned Ringbuffer Test ===").unwrap();

    // Initialize calendars to create ringbuffer behavior
    initialize_calendars(&mut uart);

    // Test 1: Basic unaligned transmission
    writeln!(uart, "\n--- Test 1: Unaligned transmission ---").unwrap();
    writeln!(
        uart,
        "Write to TX buffer and verify it appears somewhere in RX"
    )
    .unwrap();

    let tx_ringbuffer = TransmitRingbuffer::new(gather_unit);
    let mut rx_ringbuffer = ReceiveRingbuffer::new(scatter_unit, 0);

    tx_ringbuffer.clear();

    const TEST_SIZE: usize = 4;
    let test_pattern: [[u8; 8]; TEST_SIZE] =
        core::array::from_fn(|i| (0x1000 + i as u64).to_le_bytes());

    writeln!(uart, "Writing pattern to TX at offset 0").unwrap();
    tx_ringbuffer.write_slice(&test_pattern, 0);

    // Scan entire RX buffer to find the pattern
    writeln!(uart, "Scanning RX buffer for pattern").unwrap();
    let mut found = false;
    let mut found_offset = 0;

    for offset in 0..RINGBUFFER_SIZE {
        let mut rx_data: [[u8; 8]; TEST_SIZE] = [[0u8; 8]; TEST_SIZE];
        rx_ringbuffer.read_slice(&mut rx_data, offset);

        if rx_data == test_pattern {
            found = true;
            found_offset = offset;
            break;
        }
    }

    if found {
        writeln!(uart, "SUCCESS: Pattern found at RX offset {}", found_offset).unwrap();
    } else {
        writeln!(uart, "FAILURE: Pattern not found in RX buffer").unwrap();
    }

    // Test 2: Find alignment offset
    writeln!(uart, "\n--- Test 2: Alignment discovery ---").unwrap();
    writeln!(uart, "Running find_alignment_offset procedure").unwrap();

    tx_ringbuffer.clear();

    let discovered_offset =
        bittide_hal::manual_additions::aligned_ringbuffer::find_alignment_offset(
            &tx_ringbuffer,
            &rx_ringbuffer,
        );

    writeln!(
        uart,
        "SUCCESS: Discovered rx_offset = {}",
        discovered_offset
    )
    .unwrap();

    // Test 3: Aligned transmission
    writeln!(uart, "\n--- Test 3: Aligned transmission ---").unwrap();
    writeln!(
        uart,
        "Write to TX start, read from RX start with alignment offset"
    )
    .unwrap();

    rx_ringbuffer.set_offset(discovered_offset);

    tx_ringbuffer.clear();

    const ALIGNED_TEST_SIZE: usize = 8;
    let aligned_pattern: [[u8; 8]; ALIGNED_TEST_SIZE] =
        core::array::from_fn(|i| (0x2000 + i as u64).to_le_bytes());

    writeln!(
        uart,
        "Writing {} words to TX at offset 0",
        ALIGNED_TEST_SIZE
    )
    .unwrap();
    tx_ringbuffer.write_slice(&aligned_pattern, 0);

    let mut rx_data: [[u8; 8]; ALIGNED_TEST_SIZE] = [[0u8; 8]; ALIGNED_TEST_SIZE];
    writeln!(
        uart,
        "Reading {} words from RX at offset 0",
        ALIGNED_TEST_SIZE
    )
    .unwrap();
    rx_ringbuffer.read_slice(&mut rx_data, 0);

    let aligned_matches = aligned_pattern
        .iter()
        .zip(rx_data.iter())
        .filter(|(a, b)| a == b)
        .count();

    if aligned_matches == ALIGNED_TEST_SIZE {
        writeln!(uart, "SUCCESS: All {} words matched!", ALIGNED_TEST_SIZE).unwrap();
    } else {
        writeln!(
            uart,
            "FAILURE: Only {}/{} words matched",
            aligned_matches, ALIGNED_TEST_SIZE
        )
        .unwrap();
        for i in 0..ALIGNED_TEST_SIZE {
            if aligned_pattern[i] != rx_data[i] {
                writeln!(
                    uart,
                    "  Mismatch at index {}: sent {:?}, received {:?}",
                    i, aligned_pattern[i], rx_data[i]
                )
                .unwrap();
            }
        }
    }

    // Test 4: Wrapping behavior
    writeln!(uart, "\n--- Test 4: Buffer wrapping ---").unwrap();
    writeln!(
        uart,
        "Write slice exceeding buffer end, verify split read/write"
    )
    .unwrap();

    tx_ringbuffer.clear();

    const WRAP_SIZE: usize = 4;
    const WRAP_OFFSET: usize = 15; // Start at 15, will wrap (buffer size is 16)
    let wrap_pattern: [[u8; 8]; WRAP_SIZE] =
        core::array::from_fn(|i| (0x3000 + i as u64).to_le_bytes());

    writeln!(
        uart,
        "Writing {} words at TX offset {} (wraps at boundary)",
        WRAP_SIZE, WRAP_OFFSET
    )
    .unwrap();
    tx_ringbuffer.write_slice(&wrap_pattern, WRAP_OFFSET);

    let mut wrap_rx_data: [[u8; 8]; WRAP_SIZE] = [[0u8; 8]; WRAP_SIZE];
    writeln!(
        uart,
        "Reading {} words from RX offset {}",
        WRAP_SIZE, WRAP_OFFSET
    )
    .unwrap();
    rx_ringbuffer.read_slice(&mut wrap_rx_data, WRAP_OFFSET);

    let wrap_matches = wrap_pattern
        .iter()
        .zip(wrap_rx_data.iter())
        .filter(|(a, b)| a == b)
        .count();

    if wrap_matches == WRAP_SIZE {
        writeln!(
            uart,
            "SUCCESS: All {} words matched across wrap boundary!",
            WRAP_SIZE
        )
        .unwrap();
    } else {
        writeln!(
            uart,
            "FAILURE: Only {}/{} words matched",
            wrap_matches, WRAP_SIZE
        )
        .unwrap();
        for i in 0..WRAP_SIZE {
            if wrap_pattern[i] != wrap_rx_data[i] {
                writeln!(
                    uart,
                    "  Mismatch at index {}: sent {:?}, received {:?}",
                    i, wrap_pattern[i], wrap_rx_data[i]
                )
                .unwrap();
            }
        }
    }

    // Final summary
    writeln!(uart, "\n=== Test Summary ===").unwrap();
    writeln!(
        uart,
        "Unaligned transmission: {}",
        if found { "PASS" } else { "FAIL" }
    )
    .unwrap();
    writeln!(uart, "Alignment discovery: PASS").unwrap();
    writeln!(
        uart,
        "Aligned transmission: {}",
        if aligned_matches == ALIGNED_TEST_SIZE {
            "PASS"
        } else {
            "FAIL"
        }
    )
    .unwrap();
    writeln!(
        uart,
        "Buffer wrapping: {}",
        if wrap_matches == WRAP_SIZE {
            "PASS"
        } else {
            "FAIL"
        }
    )
    .unwrap();

    let all_passed = found && aligned_matches == ALIGNED_TEST_SIZE && wrap_matches == WRAP_SIZE;
    if all_passed {
        writeln!(uart, "\n*** ALL TESTS PASSED ***").unwrap();
    } else {
        writeln!(uart, "\n*** SOME TESTS FAILED ***").unwrap();
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
