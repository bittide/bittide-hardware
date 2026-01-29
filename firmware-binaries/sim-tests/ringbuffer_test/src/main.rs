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

    writeln!(uart, "=== Ringbuffer Loopback Test ===").unwrap();
    writeln!(uart, "TX ringbuffer continuously reads and transmits data").unwrap();
    writeln!(uart, "RX ringbuffer continuously receives and writes data").unwrap();

    let tx_ringbuffer = INSTANCES.transmit_ringbuffer;
    let rx_ringbuffer = INSTANCES.receive_ringbuffer;

    // Create pattern: 4 MSBs = frame number, 4 LSBs = byte index in frame
    // Frame 0: [0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07]
    // Frame 1: [0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17]
    // etc.
    let tx_pattern: [[u8; 8]; RINGBUFFER_SIZE] = core::array::from_fn(|frame| {
        core::array::from_fn(|byte_idx| ((frame as u8) << 4) | (byte_idx as u8))
    });

    // Write pattern to TX buffer
    writeln!(uart, "\nWriting pattern to TX buffer:").unwrap();
    for (i, word) in tx_pattern.iter().enumerate() {
        tx_ringbuffer.set_data(i, *word).expect("TX write failed");
        writeln!(uart, "  TX[{:2}] = {:02x?}", i, word).unwrap();
    }

    // Printing takes time, allowing data to reach RX buffer
    writeln!(uart, "\nSearching for pattern in RX buffer:").unwrap();

    // Find the first frame (frame 0 = [0x00, 0x01, ..., 0x07])
    let first_frame = tx_pattern[0];
    let mut found_offset = None;

    for offset in 0..RINGBUFFER_SIZE {
        if let Some(data) = rx_ringbuffer.data(offset) {
            if data == first_frame {
                found_offset = Some(offset);
                writeln!(uart, "Found first frame at RX[{}]", offset).unwrap();
                break;
            }
        }
    }

    // Check and print the pattern starting at the found offset
    if let Some(offset) = found_offset {
        let mut all_match = true;
        for (i, expected) in tx_pattern.iter().enumerate() {
            let rx_idx = (offset + i) % RINGBUFFER_SIZE;
            let actual = rx_ringbuffer.data(rx_idx).expect("RX read failed");

            writeln!(
                uart,
                "  RX[{:2}] = {:02x?} (expected {:02x?}) {}",
                rx_idx,
                actual,
                *expected,
                if actual == *expected { "✓" } else { "✗" }
            )
            .unwrap();

            if actual != *expected {
                all_match = false;
            }
        }

        if all_match {
            writeln!(uart, "\n*** TEST PASSED: All data matches! ***").unwrap();
        } else {
            writeln!(uart, "\n*** TEST FAILED: Data corruption detected ***").unwrap();
        }
    } else {
        writeln!(
            uart,
            "\n*** TEST FAILED: First frame not found in RX buffer ***"
        )
        .unwrap();
        writeln!(uart, "RX buffer contents:").unwrap();
        for i in 0..RINGBUFFER_SIZE {
            let word = rx_ringbuffer.data(i).expect("RX read failed");
            writeln!(uart, "  RX[{:2}] = {:02x?}", i, word).unwrap();
        }
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
