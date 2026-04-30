// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]
#![feature(sync_unsafe_cell)]

use bittide_hal::{hals::switch_demo_pe_test::DeviceInstances, manual_additions::timer::Duration};
use bittide_sys::{switch_demo_pe::SdpeUtils, uart::log::LOGGER};
use core::fmt::Write;
use log::{info, LevelFilter};

#[cfg(not(test))]
use riscv_rt::entry;

const DEVICES: DeviceInstances = unsafe { DeviceInstances::new() };

// Size of buffer in number of "tri-cycles". That is, we always store 3 64-bit words:
// local clock cycle counter, DNA (64 lsbs), DNA (32 msbs, zero-extended).
// Should match `bufferSize` of the associated `switchDemoPeWb` device.
const BUFFER_SIZE: usize = 2;

// See https://github.com/bittide/bittide-hardware/issues/681
#[allow(static_mut_refs)]
#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals.
    let mut uart = DEVICES.uart;
    let timer = DEVICES.timer;
    let mut switch_pe_a = DEVICES.switch_demo_pe_0;
    let mut switch_pe_b = DEVICES.switch_demo_pe_1;

    unsafe {
        let logger = &mut (*LOGGER.get());
        logger.set_logger(uart.clone());
        let log_timer = DEVICES.timer;
        logger.set_timer(log_timer);
        logger.display_source = LevelFilter::Info;
        log::set_logger_racy(logger).ok();
        // The 'max_level' is actually the current debug level. Note that the
        // unittest uses a release build, which has 'max_level_info', which sets
        // the actual maximum level.
        log::set_max_level_racy(LevelFilter::Info);
    }

    info!(
        "Local counter: 0x{:X}",
        switch_pe_a.local_clock_cycle_counter()
    );

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

    timer.wait(Duration::from_micros(600));

    let (rs_a, rc_a) = switch_pe_a.get_read();
    let (rs_b, rc_b) = switch_pe_b.get_read();
    info!("A: readStart: 0x{:X}, readCycles: 0x{:X}", rs_a, rc_a);
    info!("B: readStart: 0x{:X}, readCycles: 0x{:X}", rs_b, rc_b);
    info!(
        "Local counter: 0x{:X}",
        switch_pe_a.local_clock_cycle_counter()
    );

    // Write the buffer of A over UART
    write!(uart, "Buffer A: [").unwrap();
    switch_pe_a.node_data().enumerate().for_each(|(i, nd)| {
        let sep = if i + 1 < BUFFER_SIZE { ", " } else { "" };
        write!(uart, "(0x{:X}, 0x{:X}){sep}", nd.local_counter, nd.dna).unwrap();
    });
    writeln!(uart, "]").unwrap();

    info!(
        "Local counter: 0x{:X}",
        switch_pe_a.local_clock_cycle_counter()
    );

    // Write the buffer of B over UART
    write!(uart, "Buffer B: [").unwrap();
    switch_pe_b.node_data().enumerate().for_each(|(i, nd)| {
        let sep = if i + 1 < BUFFER_SIZE { ", " } else { "" };
        write!(uart, "(0x{:X}, 0x{:X}){sep}", nd.local_counter, nd.dna).unwrap();
    });
    writeln!(uart, "]").unwrap();

    info!(
        "Local counter: 0x{:X}",
        switch_pe_a.local_clock_cycle_counter()
    );

    writeln!(uart, "Finished").unwrap();

    loop {
        continue;
    }
}

#[panic_handler]
fn panic_handler(info: &core::panic::PanicInfo) -> ! {
    let mut uart = DEVICES.uart;
    writeln!(uart, "Panicked! #{info}").unwrap();
    loop {
        continue;
    }
}
