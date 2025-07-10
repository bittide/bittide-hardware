// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]
#![feature(sync_unsafe_cell)]

use bittide_hal::shared::devices::uart::Uart;
use bittide_sys::dna_port_e2::dna_to_u128;
use bittide_sys::switch_demo_pe::SwitchDemoProcessingElement;
use bittide_sys::time::{Clock, Duration};
use bittide_sys::uart::log::LOGGER;

use core::fmt::Write;
use log::{info, LevelFilter};

#[cfg(not(test))]
use riscv_rt::entry;

const UART_ADDR: *mut u8 = (0b010 << 29) as *mut u8;
const CLOCK_ADDR: *const () = (0b011 << 29) as *const ();
const SWITCH_PE_A: *const () = (0b100 << 29) as *const ();
const SWITCH_PE_B: *const () = (0b101 << 29) as *const ();

// Size of buffer in number of "tri-cycles". That is, we always store 3 64-bit words:
// local clock cycle counter, DNA (64 lsbs), DNA (32 msbs, zero-extended).
// Should match `bufferSize` of the associated `switchDemoPeWb` device.
const BUFFER_SIZE: usize = 2;

// See https://github.com/bittide/bittide-hardware/issues/681
#[allow(static_mut_refs)]
#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals.
    let mut uart = unsafe { Uart::new(UART_ADDR) };
    let mut clock = unsafe { Clock::new(CLOCK_ADDR) };
    let switch_pe_a: SwitchDemoProcessingElement<BUFFER_SIZE> =
        unsafe { SwitchDemoProcessingElement::new(SWITCH_PE_A) };
    let switch_pe_b: SwitchDemoProcessingElement<BUFFER_SIZE> =
        unsafe { SwitchDemoProcessingElement::new(SWITCH_PE_B) };

    unsafe {
        let logger = &mut (*LOGGER.get());
        logger.set_logger(uart.clone());
        logger.set_clock(clock.clone());
        logger.display_source = LevelFilter::Info;
        log::set_logger_racy(logger).ok();
        // The 'max_level' is actually the current debug level. Note that the
        // unittest uses a release build, which has 'max_level_info', which sets
        // the actual maximum level.
        log::set_max_level_racy(LevelFilter::Info);
    }

    info!("Local counter: 0x{:X}", switch_pe_a.get_counter());

    let first_transfer_start = 0x10000;
    let second_transfer_start = 0x10100;

    // A only writes its own data
    switch_pe_a.set_write(first_transfer_start, 1);
    // B reads data from A
    switch_pe_b.set_read(first_transfer_start, 1);
    // B writes its own data and data received from A
    switch_pe_b.set_write(second_transfer_start, 2);
    // A reads all data from B
    switch_pe_a.set_read(second_transfer_start, 2);

    clock.wait(Duration::from_micros(600));

    let (rs_a, rc_a) = switch_pe_a.get_read();
    let (rs_b, rc_b) = switch_pe_b.get_read();
    info!("A: readStart: 0x{:X}, readCycles: 0x{:X}", rs_a, rc_a);
    info!("B: readStart: 0x{:X}, readCycles: 0x{:X}", rs_b, rc_b);
    info!("Local counter: 0x{:X}", switch_pe_a.get_counter());

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

    info!("Local counter: 0x{:X}", switch_pe_a.get_counter());

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

    info!("Local counter: 0x{:X}", switch_pe_a.get_counter());

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
