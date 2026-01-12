#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::hals::soft_ugn_demo_mu::DeviceInstances;
use bittide_hal::manual_additions::soft_ugn_demo_mu::{
    aligned_ringbuffer::{find_alignment_offset, ReceiveRingbuffer, TransmitRingbuffer},
    soft_ugn_discovery::discover_ugn,
};
use bittide_hal::shared_devices::Transceivers;
use bittide_hal::types::TimeCmd;
use bittide_sys::stability_detector::Stability;
use core::panic::PanicInfo;
use ufmt::uwriteln;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };
const NUM_LINKS: usize = 7;

#[cfg(not(test))]
use riscv_rt::entry;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    let timer = &INSTANCES.timer;
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

    let scatter_units = [
        INSTANCES.scatter_unit_0,
        INSTANCES.scatter_unit_1,
        INSTANCES.scatter_unit_2,
        INSTANCES.scatter_unit_3,
        INSTANCES.scatter_unit_4,
        INSTANCES.scatter_unit_5,
        INSTANCES.scatter_unit_6,
    ];

    let gather_units = [
        INSTANCES.gather_unit_0,
        INSTANCES.gather_unit_1,
        INSTANCES.gather_unit_2,
        INSTANCES.gather_unit_3,
        INSTANCES.gather_unit_4,
        INSTANCES.gather_unit_5,
        INSTANCES.gather_unit_6,
    ];

    uwriteln!(uart, "=== Soft UGN Discovery Demo ===").unwrap();
    uwriteln!(uart, "Management Unit with integrated scatter/gather").unwrap();

    // Step 1: Center elastic buffers
    uwriteln!(uart, "\nStep 1: Centering elastic buffers...").unwrap();
    let mut eb_changes: [i8; NUM_LINKS] = [0; NUM_LINKS];
    loop {
        for (i, eb) in elastic_buffers.iter().enumerate() {
            eb_changes[i] += eb.set_occupancy(0);
        }

        let stable = cc.links_stable();
        let stability = Stability {
            stable: stable[0],
            settled: 0,
        };
        if stability.all_stable() {
            uwriteln!(uart, "  All links stable").unwrap();
            break;
        }
    }

    for (i, eb) in elastic_buffers.iter().enumerate() {
        uwriteln!(uart, "  EB {}: frames changed = {}", i, eb_changes[i]).unwrap();
        eb.set_stable(true);
    }

    // Step 2: Switch transceivers to user mode
    uwriteln!(uart, "\nStep 2: Switching to user mode...").unwrap();
    for channel in 0..Transceivers::RECEIVE_READYS_LEN {
        transceivers.set_receive_readys(channel, true);
        transceivers.set_transmit_starts(channel, true);
    }
    uwriteln!(uart, "  Transceivers in user mode").unwrap();

    // Step 3: Align ringbuffers for each link
    uwriteln!(uart, "\nStep 3: Aligning ringbuffers...").unwrap();
    let mut rx_offsets = [0usize; NUM_LINKS];

    for link in 0..NUM_LINKS {
        let tx = TransmitRingbuffer::new(&gather_units[link]);
        let rx = ReceiveRingbuffer::new(&scatter_units[link], 0);

        let offset = find_alignment_offset(&tx, &rx);
        rx_offsets[link] = offset;
        uwriteln!(uart, "  Link {}: aligned at offset {}", link, offset).unwrap();
    }

    // Step 4: Perform soft UGN discovery
    uwriteln!(uart, "\nStep 4: Discovering UGNs...").unwrap();
    // Get local counter by freezing and reading scratchpad
    timer.set_command(TimeCmd::Capture);
    let local_counter = timer.scratchpad();

    for link in 0..NUM_LINKS {
        let tx = TransmitRingbuffer::new(&gather_units[link]);
        let rx = ReceiveRingbuffer::new(&scatter_units[link], rx_offsets[link]);

        let ugn = discover_ugn(&tx, &rx, local_counter);
        uwriteln!(
            uart,
            "  Link {}: UGN = {} (local={}, remote={})",
            link,
            ugn.value(),
            ugn.local_counter,
            ugn.remote_counter
        )
        .unwrap();
    }

    uwriteln!(uart, "\n=== Discovery Complete ===").unwrap();

    #[allow(clippy::empty_loop)]
    loop {}
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    #[allow(clippy::empty_loop)]
    loop {}
}
