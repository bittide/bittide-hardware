#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::hals::switch_demo_gppe_mu::DeviceInstances;
use bittide_hal::shared_devices::Transceivers;
use bittide_sys::stability_detector::Stability;
use core::panic::PanicInfo;
use ufmt::uwriteln;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg(not(test))]
use riscv_rt::entry;

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

    // Channels should be enabled by boot program, so we can simply wait here
    uwriteln!(uart, "Waiting for channel negotiations..").unwrap();
    for channel in 0..Transceivers::HANDSHAKES_DONE_LEN {
        while !transceivers
            .handshakes_done(channel)
            .expect("Channel out of range")
        {}
        uwriteln!(uart, "Channel {} negotiation done.", channel).unwrap();
        uwriteln!(uart, "{:?}", transceivers.statistics(channel)).unwrap();
    }

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

    for (i, &changes) in eb_changes.iter().enumerate() {
        uwriteln!(uart, "Elastic Buffer {}, frames changed: {}", i, changes).unwrap();
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

    #[allow(clippy::empty_loop)]
    loop {}
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    #[allow(clippy::empty_loop)]
    loop {}
}
