#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::hals::soft_ugn_demo_mu::DeviceInstances;
use bittide_hal::manual_additions::calendar::RingbufferCalendar;
use bittide_hal::shared_devices::Uart;
use bittide_sys::link_startup::LinkStartup;
use bittide_sys::stability_detector::Stability;
use core::panic::PanicInfo;
use ufmt::uwriteln;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg(not(test))]
use riscv_rt::entry;

// Declare the external C main function
extern "C" {
    fn c_main() -> !;
}

/// Initialize scatter and gather calendars with incrementing counter entries.
/// Each calendar entry has a duration of 0 (no repeat), with 4000 entries total.
#[no_mangle]
#[inline(never)]
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
    let mut i = 0;
    calendars.map(|cal| {
        cal.initialize_as_ringbuffer(NUM_ENTRIES);
        uwriteln!(uart, "    Initialized calendar {}", i).unwrap();
        i += 1;
    });
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

    let capture_ugns = [
        INSTANCES.capture_ugn_0,
        INSTANCES.capture_ugn_1,
        INSTANCES.capture_ugn_2,
        INSTANCES.capture_ugn_3,
        INSTANCES.capture_ugn_4,
        INSTANCES.capture_ugn_5,
        INSTANCES.capture_ugn_6,
    ];

    let mut link_startups = [LinkStartup::new(); 7];
    while !link_startups.iter().all(|ls| ls.is_done()) {
        for (i, link_startup) in link_startups.iter_mut().enumerate() {
            link_startup.next(
                transceivers,
                i,
                elastic_buffers[i],
                capture_ugns[i].has_captured(),
            );
        }
    }

    uwriteln!(uart, "Waiting for stability...").unwrap();
    loop {
        // We don't update the stability here, but leave that to callisto. Although
        // we also have access to the 'links_settled' register, we don't want to
        // flood the CC bus.
        let stability = Stability {
            stable: cc.links_stable()[0],
            settled: 0,
        };
        let all_stable = stability.all_stable();
        if all_stable {
            break;
        }
    }

    uwriteln!(uart, "Stopping auto-centering...").unwrap();
    elastic_buffers
        .iter()
        .for_each(|eb| eb.set_auto_center_enable(false));
    elastic_buffers
        .iter()
        .for_each(|eb| eb.wait_auto_center_idle());
    let eb_deltas = elastic_buffers
        .iter()
        .map(|eb| eb.auto_center_total_adjustments());

    for (i, (capture_ugn, eb_delta)) in capture_ugns.iter().zip(eb_deltas).enumerate() {
        uwriteln!(
            uart,
            "Capture UGN {}: local = {}, remote = {}, eb_delta = {}",
            i,
            capture_ugn.local_counter(),
            capture_ugn.remote_counter(),
            eb_delta
        )
        .unwrap();
    }
    uwriteln!(uart, "Captured all hardware UGNs").unwrap();

    // Initialize scatter/gather calendars with incrementing counters
    uwriteln!(uart, "Initializing scatter/gather calendars").unwrap();
    initialize_calendars(&mut uart);
    uwriteln!(uart, "All calendars initialized").unwrap();

    uwriteln!(uart, "Calling C..").unwrap();
    unsafe { c_main() }
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    loop {}
}
