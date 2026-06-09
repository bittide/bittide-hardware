#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::hals::wire_demo_management_unit::DeviceInstances;
use bittide_hal::manual_additions::signed::Signed;
use bittide_hal::shared_devices::elastic_buffer::ElasticBuffer;
use bittide_sys::link_startup::LinkStartup;
use bittide_sys::stability_detector::Stability;
use core::panic::PanicInfo;
use ufmt::uwriteln;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

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

#[cfg(not(test))]
use riscv_rt::entry;

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

    #[allow(clippy::empty_loop)]
    loop {}
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    loop {}
}
