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

// Keep the elastic buffer occupancy ('data_count', a signed value centered at 0)
// at least this many frames away from full/empty while applying corrections, so a
// correction can never cause an over- or underflow. The buffer is `FifoSize = 5`
// bits wide (range -16..15), so this leaves a comfortable margin.
const SAFE_HI: i64 = 11;
const SAFE_LO: i64 = -12;

// Upper bound on attempts per link, so a correction that cannot be fully applied
// (e.g. it would push the buffer past the safety margin) eventually gives up
// instead of spinning forever.
const MAX_APPLY_ITERS: u32 = 1_000_000;

/// Apply a UGN grooming correction to one link's elastic buffer.
///
/// `delta` is the number of frames to insert (positive) or remove (negative).
/// Frames are applied one at a time, and only while the resulting occupancy stays
/// within `[SAFE_LO, SAFE_HI]`, so the buffer never over- or underflows. If the
/// margin blocks further progress the loop keeps retrying (occupancy may drift
/// back into range) up to `MAX_APPLY_ITERS`. Returns the number of frames actually
/// applied (equal to `delta` on success).
fn apply_correction_safe(eb: &ElasticBuffer, delta: i64) -> i64 {
    let step_up = Signed::<32, i32>::new(1).unwrap();
    let step_down = Signed::<32, i32>::new(-1).unwrap();
    let mut applied: i64 = 0;
    let mut iters: u32 = 0;
    while applied != delta && iters < MAX_APPLY_ITERS {
        iters += 1;
        let occupancy = eb.data_count().into_inner() as i64;
        if applied < delta {
            // Need to insert frames; only do so if there is headroom.
            if occupancy < SAFE_HI {
                eb.set_adjustment(step_up);
                applied += 1;
            }
        } else if occupancy > SAFE_LO {
            // Need to remove frames; only do so if there is headroom.
            eb.set_adjustment(step_down);
            applied -= 1;
        }
    }
    applied
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
    uwriteln!(uart, "Printed all hardware UGNs").unwrap();

    // === UGN grooming: poll for host-computed corrections and apply them ===
    // The host reads the UGNs, computes the corrections, halts this CPU over GDB,
    // writes the corrections vector + sets `valid`, and resumes us. We then apply
    // each correction to its link's elastic buffer, keeping the buffers within a
    // safe occupancy margin.
    let corrections = INSTANCES.ugn_corrections;
    uwriteln!(uart, "Waiting for corrections...").unwrap();
    while !corrections.valid() {
        core::hint::spin_loop();
    }
    uwriteln!(uart, "Corrections valid, applying...").unwrap();

    let mut all_applied = true;
    for (i, eb) in elastic_buffers.iter().enumerate() {
        let target = corrections.corrections(i).unwrap().into_inner();
        let before = eb.data_count().into_inner() as i64;
        let applied = apply_correction_safe(eb, target);
        let after = eb.data_count().into_inner() as i64;
        uwriteln!(
            uart,
            "Correction link {}: target = {}, applied = {}, data_count {} -> {}",
            i,
            target,
            applied,
            before,
            after
        )
        .unwrap();
        if applied != target {
            all_applied = false;
        }
    }

    if all_applied {
        uwriteln!(uart, "Corrections applied successfully").unwrap();
    } else {
        uwriteln!(uart, "Correction apply incomplete").unwrap();
    }

    #[allow(clippy::empty_loop)]
    loop {}
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    loop {}
}
