// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use bittide_hal::scatter_gather_pe::DeviceInstances;
use core::fmt::Write;
#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

/// The `MEM_SIZE` defined as the number of 64-bit words in the scatter and
/// gather memory.
const MEM_SIZE: usize = {
    let gu_size = bittide_hal::hals::scatter_gather_pe::devices::GatherUnit::GATHER_MEMORY_LEN;
    let su_size = bittide_hal::hals::scatter_gather_pe::devices::ScatterUnit::SCATTER_MEMORY_LEN;
    if gu_size != su_size {
        panic!("Gather unit and scatter unit memory sizes are unequal!");
    } else {
        gu_size
    }
};

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals.
    let mut uart = INSTANCES.uart;
    let scatter_unit = INSTANCES.scatter_unit;
    let gather_unit = INSTANCES.gather_unit;

    // The calendars for the scatter and gather units are expected to write the
    // data sent over the link to the same memory location in the scatter unit. This configures
    // them as a sort of ringbuffer and data written to the gather memory should arrive at the same
    // location in the scatter memory after being sent over the link.
    // To time our reads and writes correctly, we wait for the start of a metacycle before
    // writing to the gather memory because that gives us the most time before the data is sent
    // over the link.

    // The test takes 2 metacycles:
    //   1: Write to the gather memory.
    //   2: Read from the scatter memory.

    let source: [u64; MEM_SIZE] = core::array::from_fn(|i| i as u64);
    let mut destination: [u64; MEM_SIZE] = [99; MEM_SIZE];

    // Metacycle 1: write to gather
    gather_unit.wait_for_new_metacycle();
    gather_unit.write_slice(&source[..MEM_SIZE], 0);

    // Metacycle 2: read from scatter
    scatter_unit.wait_for_new_metacycle();
    scatter_unit.read_slice(destination[..MEM_SIZE].as_mut(), 0);

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
    let mut uart = INSTANCES.uart;
    writeln!(uart, "Panicked! #{info}").unwrap();
    loop {
        continue;
    }
}
