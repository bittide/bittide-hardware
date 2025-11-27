#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;
use ufmt::uwriteln;
use bittide_hal::{
    hals::soft_ugn_demo_mu::DeviceInstances,
    shared_devices::Uart,
    types::ValidEntry_16,
};
const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg(not(test))]
use riscv_rt::entry;

/// Initialize scatter and gather calendars with incrementing counter entries.
/// Each calendar entry has a duration of 0 (no repeat), with 4000 entries total.
fn initialize_calendars(uart: &mut Uart) {
    const NUM_ENTRIES: usize = 4000;

    // Prepare calendar entries: incrementing counter 0-999 where each entry repeats 100 times
    let calendar_entries: [ValidEntry_16<u16>; NUM_ENTRIES] =
        core::array::from_fn(|i| ValidEntry_16 {
            ve_entry: i as u16,
            ve_repeat: 0,
        });

    // Initialize all scatter calendars
    let scatter_calendars = [
        &INSTANCES.scatter_calendar,
        &INSTANCES.scatter_calendar_1,
        &INSTANCES.scatter_calendar_2,
        &INSTANCES.scatter_calendar_3,
        &INSTANCES.scatter_calendar_4,
        &INSTANCES.scatter_calendar_5,
        &INSTANCES.scatter_calendar_6,
    ];

    for (i, calendar) in scatter_calendars.iter().enumerate() {
        uwriteln!(uart, "  Initializing scatter calendar {}", i).unwrap();
        for (n, entry) in calendar_entries.iter().enumerate() {
            calendar.set_shadow_entry(*entry);
            calendar.set_write_addr(n as u16);
        }
        calendar.set_shadow_depth_index((calendar_entries.len() - 1) as u16);
        calendar.set_swap_active(true);
    }

    // Initialize all gather calendars
    let gather_calendars = [
        &INSTANCES.gather_calendar,
        &INSTANCES.gather_calendar_1,
        &INSTANCES.gather_calendar_2,
        &INSTANCES.gather_calendar_3,
        &INSTANCES.gather_calendar_4,
        &INSTANCES.gather_calendar_5,
        &INSTANCES.gather_calendar_6,
    ];

    for (i, calendar) in gather_calendars.iter().enumerate() {
        uwriteln!(uart, "  Initializing gather calendar {}", i).unwrap();
        for (n, entry) in calendar_entries.iter().enumerate() {
            calendar.set_shadow_entry(*entry);
            calendar.set_write_addr(n as u16);
        }
        calendar.set_shadow_depth_index((calendar_entries.len() - 1) as u16);
        calendar.set_swap_active(true);
    }
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;

    uwriteln!(uart, "Hello from management unit..").unwrap();

    uwriteln!(uart, "Centering buffer occupancies").unwrap();
    for (i, eb) in [
        &INSTANCES.elastic_buffer,
        &INSTANCES.elastic_buffer_1,
        &INSTANCES.elastic_buffer_2,
        &INSTANCES.elastic_buffer_3,
        &INSTANCES.elastic_buffer_4,
        &INSTANCES.elastic_buffer_5,
        &INSTANCES.elastic_buffer_6,
    ]
    .iter()
    .enumerate()
    {
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

    uwriteln!(uart, "Starting UGN captures").unwrap();
    let mut capture_ugns = [
        (INSTANCES.capture_ugn, false),
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
                    capture_ugn.remote_counter()
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

    #[allow(clippy::empty_loop)]
    loop {}
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    #[allow(clippy::empty_loop)]
    loop {}
}
