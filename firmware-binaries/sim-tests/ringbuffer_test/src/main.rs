// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use bittide_hal::{
    manual_additions::timer::Duration,
    ringbuffer_test::{devices::TransmitRingbuffer, DeviceInstances},
};
use core::fmt::Write;
#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

/// The ringbuffer size in 64-bit words
const RINGBUFFER_SIZE: usize = 16;
/// Total number of bytes in the ringbuffer
const TOTAL_BYTES: usize = RINGBUFFER_SIZE * 8;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    let timer = INSTANCES.timer;

    writeln!(uart, "=== Ringbuffer Loopback Test (Byte-Level) ===").unwrap();

    let tx_ringbuffer = INSTANCES.transmit_ringbuffer;
    let rx_ringbuffer = INSTANCES.receive_ringbuffer;

    // Create pattern: 4 MSBs = frame number, 4 LSBs = byte index in frame
    // Frame 0: [0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07]
    // Frame 1: [0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17]
    // etc.
    let tx_pattern: [u8; TOTAL_BYTES] = core::array::from_fn(|byte_idx| {
        let frame = byte_idx / 8;
        let byte_in_frame = byte_idx % 8;
        ((frame as u8) << 4) | (byte_in_frame as u8)
    });

    // Write pattern to TX buffer byte by byte
    for (i, &byte) in tx_pattern.iter().enumerate() {
        unsafe {
            tx_ringbuffer.0.add(i).write_volatile(byte);
        }
    }

    // Wait for data to propagate through the loopback
    timer.wait(Duration::from_cycles(
        TransmitRingbuffer::DATA_LEN as u32,
        timer.frequency(),
    ));

    // Read RX buffer byte by byte
    let mut rx_bytes = [0u8; TOTAL_BYTES];
    for (i, rx) in rx_bytes.iter_mut().enumerate() {
        unsafe {
            *rx = rx_ringbuffer.0.add(i).read_volatile();
        }
    }

    // Find the first frame (frame 0 = [0x00, 0x01, ..., 0x07])
    let first_frame = &tx_pattern[0..8];
    let mut found_offset = None;

    for offset in 0..RINGBUFFER_SIZE {
        let start_byte = offset * 8;
        if &rx_bytes[start_byte..start_byte + 8] == first_frame {
            found_offset = Some(offset);
            break;
        }
    }

    // Check the pattern starting at the found offset
    if let Some(offset) = found_offset {
        let mut all_match = true;
        let mut first_mismatch = None;

        for frame in 0..RINGBUFFER_SIZE {
            let rx_frame = (offset + frame) % RINGBUFFER_SIZE;
            let tx_start = frame * 8;
            let rx_start = rx_frame * 8;

            for byte_in_frame in 0..8 {
                let expected = tx_pattern[tx_start + byte_in_frame];
                let actual = rx_bytes[rx_start + byte_in_frame];

                if actual != expected && first_mismatch.is_none() {
                    first_mismatch = Some((rx_frame, byte_in_frame, expected, actual));
                    all_match = false;
                }
            }
        }

        if all_match {
            writeln!(uart, "*** TEST PASSED: All data matches! ***").unwrap();
        } else {
            writeln!(uart, "\n*** TEST FAILED: Data corruption detected ***").unwrap();

            if let Some((frame, byte_idx, expected, actual)) = first_mismatch {
                writeln!(
                    uart,
                    "First mismatch at RX frame {}, byte {}: expected 0x{:02x}, got 0x{:02x}",
                    frame, byte_idx, expected, actual
                )
                .unwrap();
            }

            writeln!(uart, "\nTX pattern written (byte-by-byte):").unwrap();
            for frame in 0..RINGBUFFER_SIZE {
                let start = frame * 8;
                write!(uart, "  TX[{:2}]: ", frame).unwrap();
                for byte_idx in 0..8 {
                    write!(uart, "{:02x} ", tx_pattern[start + byte_idx]).unwrap();
                }
                writeln!(uart).unwrap();
            }

            writeln!(
                uart,
                "\nRX pattern received (starting at frame {}):",
                offset
            )
            .unwrap();
            for frame in 0..RINGBUFFER_SIZE {
                let rx_frame = (offset + frame) % RINGBUFFER_SIZE;
                let tx_start = frame * 8;
                let rx_start = rx_frame * 8;

                write!(uart, "  RX[{:2}]: ", rx_frame).unwrap();
                let mut frame_matches = true;
                for byte_idx in 0..8 {
                    let expected = tx_pattern[tx_start + byte_idx];
                    let actual = rx_bytes[rx_start + byte_idx];
                    write!(uart, "{:02x} ", actual).unwrap();
                    if actual != expected {
                        frame_matches = false;
                    }
                }
                writeln!(uart, "{}", if frame_matches { "✓" } else { "✗" }).unwrap();
            }
        }
    } else {
        writeln!(
            uart,
            "\n*** TEST FAILED: First frame not found in RX buffer ***"
        )
        .unwrap();

        writeln!(uart, "\nTX pattern written (byte-by-byte):").unwrap();
        for frame in 0..RINGBUFFER_SIZE {
            let start = frame * 8;
            write!(uart, "  TX[{:2}]: ", frame).unwrap();
            for byte_idx in 0..8 {
                write!(uart, "{:02x} ", tx_pattern[start + byte_idx]).unwrap();
            }
            writeln!(uart).unwrap();
        }

        writeln!(uart, "\nRX buffer contents (byte-by-byte):").unwrap();
        for frame in 0..RINGBUFFER_SIZE {
            let start = frame * 8;
            write!(uart, "  RX[{:2}]: ", frame).unwrap();
            for byte_idx in 0..8 {
                write!(uart, "{:02x} ", rx_bytes[start + byte_idx]).unwrap();
            }
            writeln!(uart).unwrap();
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
