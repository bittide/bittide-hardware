// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use bittide_sys::dna_port_e2::dna_to_u128;
use bittide_sys::switch_demo_pe::SwitchDemoProcessingElement;
use bittide_sys::time::{Clock, Duration};
use bittide_sys::uart::Uart;
use core::fmt::Write;
#[cfg(not(test))]
use riscv_rt::entry;

const UART_ADDR: *const () = (0b010 << 29) as *const ();
const CLOCK_ADDR: *const () = (0b011 << 29) as *const ();
const SWITCH_PE_A: *const () = (0b100 << 29) as *const ();
const SWITCH_PE_B: *const () = (0b101 << 29) as *const ();

// Size of buffer in number of "tri-cycles". That is, we always store 3 64-bit words:
// local clock cycle counter, DNA (64 lsbs), DNA (32 msbs, zero-extended).
// Should match `bufferSize` of the associated `switchDemoPeWb` device.
const BUFFER_SIZE: usize = 2;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals.
    let mut uart = unsafe { Uart::new(UART_ADDR) };
    let mut clock = unsafe { Clock::new(CLOCK_ADDR) };
    let switch_pe_a: SwitchDemoProcessingElement<BUFFER_SIZE> =
        unsafe { SwitchDemoProcessingElement::new(SWITCH_PE_A) };
    let switch_pe_b: SwitchDemoProcessingElement<BUFFER_SIZE> =
        unsafe { SwitchDemoProcessingElement::new(SWITCH_PE_B) };

    let first_transfer_start = 0x4000;
    let second_transfer_start = 0x4100;

    // A only writes its own data
    switch_pe_a.set_write(first_transfer_start, 1);
    // B reads data from A
    switch_pe_b.set_read(first_transfer_start, 1);
    // B writes its own data and data received from A
    switch_pe_b.set_write(second_transfer_start, 2);
    // A reads all data from B
    switch_pe_a.set_read(second_transfer_start, 2);

    clock.wait(Duration::from_micros(200));

    // Write the buffer of A over UART
    write!(uart, "Buffer A: [").unwrap();
    switch_pe_a.buffer().enumerate().for_each(|(i, nd)| {
        let sep = if i + 1 < BUFFER_SIZE { ", " } else { "" };
        write!(
            uart,
            "(0x{:X}, 0x{:X}){sep}",
            nd.local_counter,
            dna_to_u128(nd.dna)
        )
        .unwrap();
    });
    writeln!(uart, "]").unwrap();

    // Write the buffer of B over UART
    write!(uart, "Buffer B: [").unwrap();
    switch_pe_b.buffer().enumerate().for_each(|(i, nd)| {
        let sep = if i + 1 < BUFFER_SIZE { ", " } else { "" };
        write!(
            uart,
            "(0x{:X}, 0x{:X}){sep}",
            nd.local_counter,
            dna_to_u128(nd.dna)
        )
        .unwrap();
    });
    writeln!(uart, "]").unwrap();

    writeln!(uart, "Finished").unwrap();

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
