#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2026 QBayLogic
//
// SPDX-License-Identifier: Apache-2.0

// Management-unit CPU for the Manticore demo.
//
// It performs ONLY the generic bittide bring-up — link startup, wait for
// stability, stop elastic-buffer auto-centering — which is what makes the
// Bittide domain (and hence the chip's Wishbone host registers) usable. It
// then idles in an infinite loop, so the host driver can halt it over GDB and
// poke the Manticore chip registers (program load / run / trace readback).
//
// Unlike the wire-demo MU, it does NO UGN grooming / application logic: the
// Manticore chip is driven entirely from the host over the chip's DMI window.

use bittide_hal::hals::manticore_demo_management_unit::DeviceInstances;
use bittide_sys::link_startup::LinkStartup;
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

    // Bring up all links.
    let mut link_startups = [LinkStartup::new(); 7];
    while !link_startups.iter().all(|ls| ls.is_done()) {
        for (i, link_startup) in link_startups.iter_mut().enumerate() {
            link_startup.next(transceivers, handshakes, i, elastic_buffers[i]);
        }
    }

    uwriteln!(uart, "Waiting for stability...").unwrap();
    loop {
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

    // Bring-up done; the Bittide domain is stable. Idle so the host driver can
    // halt us and poke the Manticore chip's host registers.
    uwriteln!(uart, "Manticore MU: bring-up done, idling.").unwrap();
    loop {
        core::hint::spin_loop();
    }
}

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    loop {
        continue;
    }
}
