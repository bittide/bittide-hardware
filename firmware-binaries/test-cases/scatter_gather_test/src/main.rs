// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use bittide_sys::gather_unit::GatherUnit;
use bittide_sys::scatter_unit::ScatterUnit;
use bittide_sys::uart::Uart;
use core::fmt::Write;
#[cfg(not(test))]
use riscv_rt::entry;

const UART_ADDR: *const () = (0b010 << 29) as *const ();
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

    // For this test an array of incrementing values is written to the full gather
    // memory. The calendars for the scatter and gather units are expected to write the
    // data sent over the link to the same memory location in the scatter unit. This makes
    // this test behave like an 'echo'. The scatter and gather memories are double
    // buffered, and their buffers are swapped at the end of each metacycle, which is the
    // duration of a calendar.
    // The test takes 3 metacycles:
    //   1: CPU writes to the gather unit memory
    //   2: Gather memory is written over the link to the scatter memory
    //   3: Read from scatter memory

    let source: [u32; MEM_SIZE] = core::array::from_fn(|i| i as u32);
    let mut destination: [u32; MEM_SIZE] = [0; MEM_SIZE];

    // First metacycle
    gather_unit.wait_for_new_metacycle();
    gather_unit.write_slice(&source, 0);

    // Second metacycle
    gather_unit.wait_for_new_metacycle();

    // Third metacycle
    gather_unit.wait_for_new_metacycle();
    scatter_unit.read_slice(destination.as_mut(), 0);

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
