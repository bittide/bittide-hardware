#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::hals::soft_ugn_demo_mu::DeviceInstances;
use bittide_hal::shared_devices::{Transceivers, Uart};
use bittide_sys::stability_detector::Stability;
use core::panic::PanicInfo;
use ufmt::uwriteln;
const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg(not(test))]
use riscv_rt::entry;

/// Initialize scatter and gather calendars with incrementing counter entries.
/// Each calendar entry has a duration of 0 (no repeat), with 4000 entries total.
fn initialize_calendars(uart: &mut Uart) {
    const NUM_ENTRIES: usize = 4000;

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
    calendars.map(|cal| cal.initialize_as_ringbuffer(NUM_ENTRIES));
    uwriteln!(uart, "All calendars initialized").unwrap();
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    let transceivers = &INSTANCES.transceivers;
    let cc = INSTANCES.clock_control;

    let elastic_buffers = [
        &INSTANCES.elastic_buffer_0,
        &INSTANCES.elastic_buffer_1,
        &INSTANCES.elastic_buffer_2,
        &INSTANCES.elastic_buffer_3,
        &INSTANCES.elastic_buffer_4,
        &INSTANCES.elastic_buffer_5,
        &INSTANCES.elastic_buffer_6,
    ];

    // Keep centering elastic buffers to avoid over/under-flows. Keep track of
    // number of frames changed while centering.
    //
    // TODO: Implement asynchronous communication. The idea is that we center
    //       the elastic buffers and capture the UGNs. From that point on, the
    //       links can be used for arbitrary communication. We'll keep on
    //       moving the buffers to their midpoints to prevent overflows (given
    //       that clock control still might be correcting clocks), so we'll
    //       use `eb_changes` to calculate the final UGN once we determine
    //       the whole network is up and therefore stop modifying the buffer
    //       occupancy.
    uwriteln!(uart, "Continuously center buffer occupancies").unwrap();
    let mut eb_changes: [i8; 7] = [0; 7];
    loop {
        for (i, eb) in elastic_buffers.iter().enumerate() {
            eb_changes[i] += eb.set_occupancy(0);
        }

        // We don't update the stability here, but leave that to callisto. Although
        // we also have access to the 'links_settled' register, we don't want to
        // flood the CC bus.
        let stable = cc.links_stable();
        let stability = Stability {
            stable: stable[0],
            settled: 0,
        };
        if stability.all_stable() {
            uwriteln!(uart, "All links stable").unwrap();
            break;
        }
    }

    for (i, eb) in elastic_buffers.iter().enumerate() {
        uwriteln!(
            uart,
            "Elastic Buffer {}, frames changed: {}",
            i,
            eb_changes[i]
        )
        .unwrap();
        eb.set_stable(true);
    }

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
                    "[INFO] Port {} occupancy {} exceeds range [{}, {}]",
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
