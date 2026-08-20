#![no_std]
#![cfg_attr(not(test), no_main)]
// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Management unit for the decode demo. Brings the links up exactly like the
//! wire demo (link startup, stability, auto-center freeze, hardware-UGN
//! print), publishes the addresses of its configuration and results structs
//! over the UART, then serves runs of the software transport variants (C and
//! A′): the host halts this CPU, writes [`DecodeConfig`] over GDB with `go`
//! set, and resumes; the firmware aligns its ring buffers with its ring
//! neighbors, runs the requested engine, announces completion and dumps
//! nothing itself — the host reads [`DecodeResults`] straight from memory.

use bittide_hal::hals::decode_demo_management_unit::DeviceInstances;
use bittide_hal::manual_additions::ring_buffer::AlignedReceiveBuffer;
use bittide_sys::decode_demo::{
    run_variant_a_prime, run_variant_c, DecodeBuffers, DecodeConfig, DecodeResults, MODE_A_PRIME,
    MODE_C, MODE_IDLE,
};
use bittide_sys::link_startup::LinkStartup;
use bittide_sys::stability_detector::Stability;
use core::panic::PanicInfo;
use core::ptr::{addr_of, addr_of_mut};
use ufmt::uwriteln;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg(not(test))]
use riscv_rt::entry;

static mut CONFIG: DecodeConfig = DecodeConfig::new();
static mut RESULTS: DecodeResults = DecodeResults::new();

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    let timer = INSTANCES.timer;
    let transceivers = &INSTANCES.transceivers;
    let handshakes = &INSTANCES.handshakes;
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
    let rx_buffers = [
        &INSTANCES.receive_ring_buffer_0,
        &INSTANCES.receive_ring_buffer_1,
        &INSTANCES.receive_ring_buffer_2,
        &INSTANCES.receive_ring_buffer_3,
        &INSTANCES.receive_ring_buffer_4,
        &INSTANCES.receive_ring_buffer_5,
        &INSTANCES.receive_ring_buffer_6,
    ];
    let tx_buffers = [
        &INSTANCES.transmit_ring_buffer_0,
        &INSTANCES.transmit_ring_buffer_1,
        &INSTANCES.transmit_ring_buffer_2,
        &INSTANCES.transmit_ring_buffer_3,
        &INSTANCES.transmit_ring_buffer_4,
        &INSTANCES.transmit_ring_buffer_5,
        &INSTANCES.transmit_ring_buffer_6,
    ];

    let capture_ugns = &INSTANCES.capture_ugns;

    let mut link_startups = [LinkStartup::new(); 7];
    while !link_startups.iter().all(|ls| ls.is_done()) {
        for (i, link_startup) in link_startups.iter_mut().enumerate() {
            link_startup.next(transceivers, handshakes, i, elastic_buffers[i]);
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

    uwriteln!(uart, "Start printing hardware UGNs").unwrap();
    for (i, eb_delta) in eb_deltas.enumerate() {
        capture_ugns.set_elastic_buffer_delta(i, eb_delta).unwrap();
        uwriteln!(
            uart,
            "Capture UGN {}: local = {}, remote = {}, eb_delta = {}",
            i,
            capture_ugns.local_counter(i).unwrap(),
            capture_ugns.remote_counter(i).unwrap(),
            eb_delta
        )
        .unwrap();
    }
    // The host parses these addresses from the serial log (they precede the
    // sentinel line it already waits for), writes CONFIG over GDB while this
    // CPU is halted, and reads RESULTS with a GDB memory dump.
    uwriteln!(
        uart,
        "RESULTS @ {:x} CONFIG @ {:x}",
        addr_of!(RESULTS) as usize,
        addr_of!(CONFIG) as usize
    )
    .unwrap();
    uwriteln!(uart, "Printed all hardware UGNs").unwrap();

    loop {
        // `go` is written by the host over GDB (with this CPU halted, so no
        // tearing); read it volatile so the loop is not optimized away.
        let go = unsafe { addr_of!(CONFIG.go).read_volatile() };
        if go != MODE_C && go != MODE_A_PRIME {
            continue;
        }
        let cfg = unsafe { &*addr_of!(CONFIG) };
        let results = unsafe { &mut *addr_of_mut!(RESULTS) };
        let up = (cfg.read_link as usize) % 7;
        let down = (cfg.write_link as usize) % 7;
        let bufs = DecodeBuffers {
            up_rx: rx_buffers[up],
            up_ack_tx: tx_buffers[up],
            down_tx: tx_buffers[down],
            down_ack_rx: rx_buffers[down],
        };

        // Align both neighbor-facing buffer pairs with the marker protocol:
        // the announce toward the upstream neighbor travels on the read
        // link's reverse direction, the downstream neighbor announces to us
        // on the write link's reverse direction. The two alignments MUST be
        // interleaved: this node's upstream-facing alignment pairs with the
        // upstream neighbor's downstream-facing one, so running them
        // sequentially deadlocks the whole ring (every node waits for its
        // upstream's second phase).
        uwriteln!(uart, "Aligning ring buffers...").unwrap();
        let up_rx_copy = unsafe {
            bittide_hal::decode_demo_management_unit::devices::ReceiveRingBuffer::new(
                rx_buffers[up].0,
            )
        };
        let down_rx_copy = unsafe {
            bittide_hal::decode_demo_management_unit::devices::ReceiveRingBuffer::new(
                rx_buffers[down].0,
            )
        };
        let mut up_aligned = AlignedReceiveBuffer::new(up_rx_copy);
        let mut down_aligned = AlignedReceiveBuffer::new(down_rx_copy);
        loop {
            let up_done = up_aligned.align_step(tx_buffers[up]);
            let down_done = down_aligned.align_step(tx_buffers[down]);
            if up_done && down_done {
                break;
            }
        }
        uwriteln!(uart, "Ring buffers aligned").unwrap();

        match go {
            MODE_C => {
                run_variant_c(cfg, &timer, &mut uart, &bufs, results);
                uwriteln!(uart, "Variant C done").unwrap();
            }
            MODE_A_PRIME => {
                run_variant_a_prime(cfg, &timer, &mut uart, &bufs, results);
                uwriteln!(uart, "Variant A' done").unwrap();
            }
            _ => unreachable!(),
        }
        unsafe { addr_of_mut!(CONFIG.go).write_volatile(MODE_IDLE) };
    }
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    loop {}
}
