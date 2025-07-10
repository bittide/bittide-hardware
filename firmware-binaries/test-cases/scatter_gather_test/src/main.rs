// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use bittide_hal::shared::devices::uart::Uart;
use bittide_sys::gather_unit::GatherUnit;
use bittide_sys::scatter_unit::ScatterUnit;
use core::fmt::Write;
#[cfg(not(test))]
use riscv_rt::entry;

const UART_ADDR: *mut u8 = (0b010 << 29) as *mut u8;
const SCATTER_ADDR: *const () = (0b011 << 29) as *const ();
const GATHER_ADDR: *const () = (0b100 << 29) as *const ();

/// The `MEM_SIZE` defined as the number of 64-bit words in the scatter and
/// gather memory.
const MEM_SIZE: usize = 16;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals.
    let mut uart = unsafe { Uart::new(UART_ADDR) };
    let scatter_unit: ScatterUnit<MEM_SIZE> = unsafe { ScatterUnit::new(SCATTER_ADDR) };
    let gather_unit: GatherUnit<MEM_SIZE> = unsafe { GatherUnit::new(GATHER_ADDR) };

    // The calendars for the scatter and gather units are expected to write the
    // data sent over the link to the same memory location in the scatter unit. This makes
    // this test behave like an 'echo'. The scatter and gather memories are double
    // buffered, and their buffers are swapped at the end of each metacycle, which is the
    // duration of a calendar. For this test two halves of an array of incrementing values
    // are written to the gather memory. These two halves should be read back from the
    // scatter memory.
    // The test takes 4 metacycles:
    //   1: Write first halve of the array to the gather memory.
    //   2: Write second halve of the array to the gather memory, while first halve is
    //      send over the link.
    //   3: Read first halve of array from the scatter memory, while second halve is
    //      send over the link.
    //   3: Read second halve of array from scatter memory.

    let source: [u64; MEM_SIZE * 2] = core::array::from_fn(|i| i as u64);
    let mut destination: [u64; MEM_SIZE * 2] = [99; MEM_SIZE * 2];

    // First metacycle: write to gather A, link sends gather B
    gather_unit.wait_for_new_metacycle();
    gather_unit.write_slice(&source[..MEM_SIZE], 0);

    // Second metacycle, write to gather B, link sends gather A
    gather_unit.wait_for_new_metacycle();
    gather_unit.write_slice(&source[MEM_SIZE..], 0);

    // Third metacycle: read from scatter A
    scatter_unit.wait_for_new_metacycle();
    scatter_unit.read_slice(destination[..MEM_SIZE].as_mut(), 0);

    // Fourth metacycle: read from scatter B
    scatter_unit.wait_for_new_metacycle();
    scatter_unit.read_slice(destination[MEM_SIZE..].as_mut(), 0);

    // Check if slices are equal
    if source == destination {
        writeln!(uart, "Written data was read back correctly").unwrap();
    } else {
        writeln!(uart, "Could not read back written data").unwrap();
        writeln!(uart, "Written to gather memory:").unwrap();
        writeln!(uart, "{:?}", source).unwrap();
        writeln!(uart, "Read from scatter memory:").unwrap();
        writeln!(uart, "{:?}", destination).unwrap();
    }
    loop {
        continue;
    }
}

#[panic_handler]
fn panic_handler(info: &core::panic::PanicInfo) -> ! {
    let mut uart = unsafe { Uart::new(UART_ADDR) };
    writeln!(uart, "Panicked! #{info}").unwrap();
    loop {
        continue;
    }
}
