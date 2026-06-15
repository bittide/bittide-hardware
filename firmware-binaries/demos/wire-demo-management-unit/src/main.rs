#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::hals::wire_demo_management_unit::DeviceInstances;
use bittide_sys::link_startup::LinkStartup;
use bittide_sys::stability_detector::Stability;
use clash_bindings::signed::Signed;
use core::panic::PanicInfo;
use ufmt::uwriteln;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

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
        eb.set_adjustment(Signed::<32, i32>::new(target as i32).unwrap());
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
