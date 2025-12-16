#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::{
    hals::soft_ugn_demo_mu::DeviceInstances,
    shared_devices::{Transceivers, Uart},
    types::ValidEntry_16,
};
use core::panic::PanicInfo;
use ufmt::uwriteln;
const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg(not(test))]
use riscv_rt::entry;

/// Initialize scatter and gather calendars with incrementing counter entries.
/// Each calendar entry has a duration of 0 (no repeat), with 4000 entries total.
fn initialize_calendars(uart: &mut Uart) {
    const NUM_ENTRIES: usize = 4000;

    // Initialize all scatter calendars
    let calendars = [
        // Scatter calendars
        &INSTANCES.scatter_calendar_0,
        &INSTANCES.scatter_calendar_1,
        &INSTANCES.scatter_calendar_2,
        &INSTANCES.scatter_calendar_3,
        &INSTANCES.scatter_calendar_4,
        &INSTANCES.scatter_calendar_5,
        &INSTANCES.scatter_calendar_6,
        // Gather calendars
        &INSTANCES.gather_calendar_0,
        &INSTANCES.gather_calendar_1,
        &INSTANCES.gather_calendar_2,
        &INSTANCES.gather_calendar_3,
        &INSTANCES.gather_calendar_4,
        &INSTANCES.gather_calendar_5,
        &INSTANCES.gather_calendar_6,
    ];

    uwriteln!(uart, "  Initializing {} calendars", calendars.len()).unwrap();
    // Write entries to all calendars

    for calendar in calendars.iter() {
        for n in 0..NUM_ENTRIES {
            let entry = ValidEntry_16 {
                ve_entry: n as u16,
                ve_repeat: 0,
            };
            calendar.set_shadow_entry(entry);
            calendar.set_write_addr(n as u16);
        }
        calendar.set_shadow_depth_index((NUM_ENTRIES - 1) as u16);
        calendar.set_swap_active(true);
    }

    uwriteln!(uart, "All calendars initialized").unwrap();
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    let transceivers = &INSTANCES.transceivers;

    uwriteln!(uart, "Hello from management unit..").unwrap();

    uwriteln!(uart, "Centering buffer occupancies").unwrap();
    let elastic_buffers = [
        &INSTANCES.elastic_buffer_0,
        &INSTANCES.elastic_buffer_1,
        &INSTANCES.elastic_buffer_2,
        &INSTANCES.elastic_buffer_3,
        &INSTANCES.elastic_buffer_4,
        &INSTANCES.elastic_buffer_5,
        &INSTANCES.elastic_buffer_6,
        &INSTANCES.elastic_buffer_7, // ILA buffer
    ];

    for (i, eb) in elastic_buffers.iter().enumerate() {
        uwriteln!(
            uart,
            "Elastic buffer {}, current occupancy: {}",
            i,
            eb.data_count()
        )
        .unwrap();
        eb.set_occupancy(0);
        eb.set_stable(true);
    }
    uwriteln!(uart, "All elastic buffers centered").unwrap();

    uwriteln!(uart, "Switch transceiver channels to user mode..").unwrap();
    for channel in 0..Transceivers::RECEIVE_READYS_LEN {
        transceivers.set_receive_readys(channel, true);
        transceivers.set_transmit_starts(channel, true);
    }
    uwriteln!(uart, "Done").unwrap();

    uwriteln!(uart, "Starting UGN captures").unwrap();
    let mut capture_ugns = [
        (INSTANCES.capture_ugn_0, false),
        (INSTANCES.capture_ugn_1, false),
        (INSTANCES.capture_ugn_2, false),
        (INSTANCES.capture_ugn_3, false),
        (INSTANCES.capture_ugn_4, false),
        (INSTANCES.capture_ugn_5, false),
        (INSTANCES.capture_ugn_6, false),
    ];
    while capture_ugns.iter().any(|(_, done)| !done) {
        for (i, (capture_ugn, done)) in capture_ugns.iter_mut().enumerate() {
            if *done {
                continue;
            }
            if capture_ugn.has_captured() {
                uwriteln!(
                    uart,
                    "Capture UGN {}: local = {}, remote = {}",
                    i,
                    capture_ugn.local_counter(),
                    u64::from_ne_bytes(capture_ugn.remote_counter())
                )
                .unwrap();
                *done = true;
            }
        }
    }
    uwriteln!(uart, "All UGNs captured").unwrap();

    // Initialize scatter/gather calendars with incrementing counters
    uwriteln!(uart, "Initializing scatter/gather calendars").unwrap();
    initialize_calendars(&mut uart);
    uwriteln!(uart, "All calendars initialized").unwrap();

    // Initialize occupancy ranges for monitoring
    let mut occupancy_ranges: [(i8, i8); 7] = [(0, 0); 7];

    uwriteln!(uart, "Starting elastic buffer occupancy monitoring...").unwrap();

    loop {
        for (i, eb) in elastic_buffers.iter().enumerate() {
            let occupancy = eb.data_count();
            let (min, max) = occupancy_ranges[i];

            // Check if occupancy exceeds current range
            if occupancy < min || occupancy > max {
                uwriteln!(
                    uart,
                    "[WARNING] Port {} occupancy {} exceeds range [{}, {}]",
                    i,
                    occupancy,
                    min,
                    max
                )
                .unwrap();

                if occupancy < min {
                    occupancy_ranges[i].0 = occupancy;
                } else {
                    occupancy_ranges[i].1 = occupancy;
                }
            }
        }
    }
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    #[allow(clippy::empty_loop)]
    loop {}
}
