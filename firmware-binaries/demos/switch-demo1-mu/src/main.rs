#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::hals::switch_demo_mu::DeviceInstances;
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
        while !transceivers.handshakes_done(channel).unwrap_or(false) {}
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
    loop {
        elastic_buffers.iter().for_each(|eb| {
            eb.set_occupancy(0);
        });

        // We don't update the stability here, but leave that to callisto. Although
        // we also have access to the 'links_settled' register, we don't want to
        // flood the CC bus.
        let stability = Stability {
            stable: cc.links_stable()[0],
            settled: 0,
        };
        if stability.all_stable() {
            uwriteln!(uart, "All links stable").unwrap();
            break;
        }
    }

    elastic_buffers.iter().for_each(|eb| eb.set_drain_count(0));
    elastic_buffers.iter().for_each(|eb| eb.set_fill_count(0));
    elastic_buffers.iter().for_each(|eb| eb.set_stable(true));
    elastic_buffers.iter().for_each(|eb| eb.clear_flags());

    uwriteln!(uart, "Switch transceiver channels to user mode..").unwrap();
    for channel in 0..Transceivers::RECEIVE_READYS_LEN {
        transceivers.set_receive_readys(channel, true);
        transceivers.set_transmit_starts(channel, true);
    }

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
        for (capture_ugn, done) in capture_ugns.iter_mut() {
            if *done {
                continue;
            }
            if capture_ugn.has_captured() {
                *done = true;
            }
        }
    }

    // Collect EB drain and fill counts before decrease_occupancy
    let eb_drains_before: [u32; 7] = [
        elastic_buffers[0].eb_drain_count(),
        elastic_buffers[1].eb_drain_count(),
        elastic_buffers[2].eb_drain_count(),
        elastic_buffers[3].eb_drain_count(),
        elastic_buffers[4].eb_drain_count(),
        elastic_buffers[5].eb_drain_count(),
        elastic_buffers[6].eb_drain_count(),
    ];
    let eb_fills_before: [u32; 7] = [
        elastic_buffers[0].eb_fill_count(),
        elastic_buffers[1].eb_fill_count(),
        elastic_buffers[2].eb_fill_count(),
        elastic_buffers[3].eb_fill_count(),
        elastic_buffers[4].eb_fill_count(),
        elastic_buffers[5].eb_fill_count(),
        elastic_buffers[6].eb_fill_count(),
    ];

    elastic_buffers
        .iter()
        .for_each(|eb| eb.increase_occupancy(5));

    for (i, eb) in elastic_buffers.iter().enumerate() {
        if eb.overflow() {
            uwriteln!(uart, "[ERROR]: Channel {} elastic buffer overflowed", i).unwrap();
        }
        if eb.underflow() {
            uwriteln!(uart, "[ERROR]: Channel {} elastic buffer underflowed", i).unwrap();
        }
    }

    // Collect EB drain and fill counts after "All UGNs captured" and calculate differences
    for (i, eb) in elastic_buffers.iter().enumerate() {
        let eb_drains_after = eb.eb_drain_count();
        let eb_fills_after = eb.eb_fill_count();
        let drain_diff = eb_drains_after.wrapping_sub(eb_drains_before[i]);
        let fill_diff = eb_fills_after.wrapping_sub(eb_fills_before[i]);

        uwriteln!(uart, "[INFO]: Channel {} EB drains: {}", i, drain_diff).unwrap();
        uwriteln!(
            uart,
            "[INFO]: Channel {} drains:    {}",
            i,
            eb.drain_count()
        )
        .unwrap();
        uwriteln!(uart, "[INFO]: Channel {} fills:     {}", i, eb.fill_count()).unwrap();
        uwriteln!(uart, "[INFO]: Channel {} EB fills:  {}", i, fill_diff).unwrap();
        uwriteln!(uart, "[INFO]: Channel {} count:     {}", i, eb.data_count()).unwrap();
    }

    uwriteln!(uart, "All UGNs captured").unwrap();

    #[allow(clippy::empty_loop)]
    loop {
        for (i, eb) in elastic_buffers.iter().enumerate() {
            if eb.overflow() {
                uwriteln!(uart, "[ERROR]: Channel {} elastic buffer overflowed", i).unwrap();
                panic!();
            }
            if eb.underflow() {
                uwriteln!(uart, "[ERROR]: Channel {} elastic buffer underflowed", i).unwrap();
                panic!();
            }
        }
    }
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    #[allow(clippy::empty_loop)]
    loop {}
}
