#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2026 QBayLogic
//
// SPDX-License-Identifier: Apache-2.0

// Management-unit CPU for the Manticore demo.
//
// It performs the SAME generic bittide bring-up as the wire demo — link
// startup, wait for stability, stop elastic-buffer auto-centering, capture and
// print the per-link UGNs, then groom: poll for the host-computed corrections
// and apply them to the elastic buffers. This is what brings the Bittide domain
// up to the stored golden latencies (so the inter-chip seams will run at a
// constant, known latency) and lets the host verify the rig end-to-end.
//
// After grooming it MONITORS the elastic buffers: per-link occupancy
// watermarks, printing one UART line per range excursion (then widening the
// range), so the log stays quiet while the links hold their groomed occupancy
// and records every drift/rail event. Meanwhile the host driver drives the
// Manticore chip's host registers over GDB, halting this CPU briefly around
// each access (program load / run / trace readback). Unlike the wire-demo MU
// there is no application logic of its own.

use bittide_hal::hals::manticore_demo_management_unit::DeviceInstances;
use bittide_hal::manual_additions::signed::Signed;
use bittide_hal::shared_devices::elastic_buffer::ElasticBuffer;
use bittide_sys::link_startup::LinkStartup;
use bittide_sys::stability_detector::Stability;
use core::panic::PanicInfo;
use ufmt::uwriteln;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg(not(test))]
use riscv_rt::entry;

/// Apply a UGN grooming correction to one link's elastic buffer.
///
/// `delta` is the number of frames to insert (positive) or remove (negative).
/// The correction is submitted as a single atomic wishbone write with hardware
/// ack, rather than one frame at a time. The per-frame approach is vulnerable
/// to concurrent Callisto adjustments between iterations, causing the net
/// data_count change to be smaller than intended. A single write limits the
/// Callisto race window to at most one adjustment, keeping the error to ±1 frame.
fn apply_correction(eb: &ElasticBuffer, delta: i64) -> i64 {
    let step = Signed::<32, i32>::new(delta as i32).unwrap();
    eb.set_adjustment(step);
    delta
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
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

    let capture_ugns = [
        INSTANCES.capture_ugn_0,
        INSTANCES.capture_ugn_1,
        INSTANCES.capture_ugn_2,
        INSTANCES.capture_ugn_3,
        INSTANCES.capture_ugn_4,
        INSTANCES.capture_ugn_5,
        INSTANCES.capture_ugn_6,
    ];

    // Bring up all links.
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
        if stability.all_stable() {
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
    for (i, (capture_ugn, eb_delta)) in capture_ugns.iter().zip(eb_deltas).enumerate() {
        capture_ugn.set_elastic_buffer_delta(eb_delta);
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
    uwriteln!(uart, "Printed all hardware UGNs").unwrap();

    // === UGN grooming: poll for host-computed corrections and apply them ===
    // The host reads the UGNs, computes the corrections, halts this CPU over GDB,
    // writes the corrections vector + sets `valid`, and resumes us. We then apply
    // each correction to its link's elastic buffer in one atomic adjustment per
    // link. Using a single adjustment (rather than a per-frame loop) avoids the
    // race where Callisto's concurrent adjustments partially undo the correction.
    let corrections = INSTANCES.ugn_corrections;
    uwriteln!(uart, "Waiting for corrections...").unwrap();
    while !corrections.valid() {
        core::hint::spin_loop();
    }
    uwriteln!(uart, "Corrections valid, applying...").unwrap();

    for (i, eb) in elastic_buffers.iter().enumerate() {
        let target = corrections.corrections(i).unwrap().into_inner();
        let before = eb.data_count().into_inner() as i64;
        apply_correction(eb, target);
        let after = eb.data_count().into_inner() as i64;
        uwriteln!(
            uart,
            "Correction link {}: target = {}, data_count {} -> {}",
            i,
            target,
            before,
            after
        )
        .unwrap();
    }

    uwriteln!(uart, "Corrections applied successfully").unwrap();

    // Bring-up + grooming done; the Bittide domain is stable at the golden
    // latencies. From here the host drives the Manticore chip over GDB (halting
    // this CPU briefly around each access); between accesses we MONITOR the
    // elastic buffers, so the UART log records whether every link stays within
    // bounds during the application run. With clock control live this must
    // hold; a railed buffer (permanent under/overflow) means syntony was lost
    // and the inter-chip seams silently stop delivering frames.
    uwriteln!(
        uart,
        "Manticore MU: bring-up done, monitoring elastic buffers."
    )
    .unwrap();
    // Clear the sticky status records (min/max data count, under/overflow) so
    // the monitor reports the application era only -- bring-up legitimately
    // rails the buffers before auto-centering.
    for eb in elastic_buffers.iter() {
        eb.set_clear_status_registers(true);
        eb.set_clear_status_registers(false);
    }
    // Per-link occupancy watermarks, seeded from the current (groomed)
    // occupancy. The monitor stays SILENT while every link remains inside its
    // observed range and prints one line per range EXCURSION (then widens the
    // range to the new extreme) -- so a healthy run logs just the 7 baselines,
    // while any drift/rail event is recorded with its exact value. Prints are
    // self-limiting: a link can only print on a new extreme.
    let mut lo = [0i8; 7];
    let mut hi = [0i8; 7];
    for (i, eb) in elastic_buffers.iter().enumerate() {
        let dc = eb.data_count().into_inner();
        lo[i] = dc;
        hi[i] = dc;
        uwriteln!(uart, "[EBMON] link {} baseline dc={}", i, dc).unwrap();
    }
    loop {
        for (i, eb) in elastic_buffers.iter().enumerate() {
            let dc = eb.data_count().into_inner();
            if dc < lo[i] {
                uwriteln!(
                    uart,
                    "[EBMON] link {} dc={} below [{}..{}]{}{}",
                    i,
                    dc,
                    lo[i],
                    hi[i],
                    if eb.underflow() { " UF" } else { "" },
                    if eb.overflow() { " OF" } else { "" }
                )
                .unwrap();
                lo[i] = dc;
            } else if dc > hi[i] {
                uwriteln!(
                    uart,
                    "[EBMON] link {} dc={} above [{}..{}]{}{}",
                    i,
                    dc,
                    lo[i],
                    hi[i],
                    if eb.underflow() { " UF" } else { "" },
                    if eb.overflow() { " OF" } else { "" }
                )
                .unwrap();
                hi[i] = dc;
            }
        }
    }
}

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    loop {
        continue;
    }
}
