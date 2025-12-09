#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::{hals::switch_demo_mu::DeviceInstances, shared_devices::Transceivers};
use core::panic::PanicInfo;
use ufmt::uwriteln;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg(not(test))]
use riscv_rt::entry;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    let transceivers = &INSTANCES.transceivers;
    let elastic_buffers = [
        &INSTANCES.elastic_buffer_0,
        &INSTANCES.elastic_buffer_1,
        &INSTANCES.elastic_buffer_2,
        &INSTANCES.elastic_buffer_3,
        &INSTANCES.elastic_buffer_4,
        &INSTANCES.elastic_buffer_5,
        &INSTANCES.elastic_buffer_6,
    ];

    // Channels should be enabled by boot program, so we can simply wait here
    uwriteln!(uart, "Waiting for channel negotiations..").unwrap();
    for channel in 0..Transceivers::HANDSHAKES_DONE_LEN {
        while !transceivers.handshakes_done(channel).unwrap_or(false) {}
        uwriteln!(uart, "Channel {} negotiation done.", channel).unwrap();
        uwriteln!(uart, "{:?}", transceivers.statistics(channel)).unwrap();
    }

    // Keep centering elastic buffers to avoid over/under-flows. Keep track of
    // number of frames changed while centering.
    uwriteln!(uart, "Continuously center buffer occupancies").unwrap();
    let mut eb_changes: [i8; 7] = [0; 7];
    // TODO: What is the exit condition for this loop?
    let some_cool_condition = false;
    while !some_cool_condition {
        for (i, eb) in elastic_buffers.iter().enumerate()
        {
            eb_changes[i] += eb.data_count();
            eb.set_occupancy(0);
        }
    }

    for (i, eb) in elastic_buffers.iter().enumerate() {
        uwriteln!(uart, "Elastic Buffer {}, frames changed: {}", i, eb_changes[i]).unwrap();
        eb.set_stable(true);
    }

    // TODO: Print message related to exit condition of eb centering loop
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
                    capture_ugn.remote_counter()
                )
                .unwrap();
                *done = true;
            }
        }
    }
    uwriteln!(uart, "All UGNs captured").unwrap();

    #[allow(clippy::empty_loop)]
    loop {}
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    #[allow(clippy::empty_loop)]
    loop {}
}
