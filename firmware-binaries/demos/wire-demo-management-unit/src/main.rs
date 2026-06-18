#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::hals::wire_demo_management_unit::DeviceInstances;
use bittide_sys::drp::Drp;
use bittide_sys::link_startup::LinkStartup;
use bittide_sys::stability_detector::Stability;
use core::panic::PanicInfo;
use ufmt::uwriteln;

// GTH DRP register addresses (UG576, GTHE3 eye scan / RX margin analysis).
const ES_CONTROL_ADDR: u16 = 0x3C; // ES_EYE_SCAN_EN[8], ES_ERRDET_EN[9], ES_CONTROL[15:10], ES_PRESCALE[4:0]
const ES_HORZ_OFFSET_ADDR: u16 = 0x4F; // [15:4]: horizontal (phase) offset
const ES_ERROR_COUNT_ADDR: u16 = 0x151; // RO
const ES_SAMPLE_COUNT_ADDR: u16 = 0x152; // RO
const ES_CONTROL_STATUS_ADDR: u16 = 0x153; // RO: [0]=DONE, [3:1]=state

/// Validate the DRP read/write path on one channel with a read-modify-write
/// round-trip on `ES_HORZ_OFFSET[15:4]` (a benign eye-scan offset that has no
/// effect on the data path while eye scan is disabled). A distinct per-channel
/// pattern also confirms each GTH channel is addressed independently. Restores
/// the original value (eye scan expects the offset centered at 0).
fn drp_selftest(drp: &Drp, channel: usize) -> &'static str {
    let orig = match drp.read(channel, ES_HORZ_OFFSET_ADDR) {
        Ok(v) => v,
        Err(_) => return "READ_FAIL",
    };
    let pattern = 0x0A0u16 | (channel as u16 & 0xF); // distinct per channel
    if drp
        .modify(channel, ES_HORZ_OFFSET_ADDR, 0xFFF0, pattern << 4)
        .is_err()
    {
        return "WRITE_FAIL";
    }
    let readback = match drp.read(channel, ES_HORZ_OFFSET_ADDR) {
        Ok(v) => v,
        Err(_) => return "READBACK_FAIL",
    };
    let _ = drp.modify(channel, ES_HORZ_OFFSET_ADDR, 0xFFF0, orig);
    if (readback >> 4) == pattern {
        "PASS"
    } else {
        "MISMATCH"
    }
}

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

    // Read out GTH transceiver parameters over the DRP, per link, and dump them
    // over UART. First a read-modify-write self-test proves the DRP path works on
    // hardware; then a few relevant eye-scan / RX-margin registers are dumped.
    let drp = Drp::new(transceivers);
    uwriteln!(uart, "Starting DRP readout").unwrap();
    for channel in 0..7 {
        let selftest = drp_selftest(&drp, channel);
        let status = drp.read(channel, ES_CONTROL_STATUS_ADDR);
        let ctrl = drp.read(channel, ES_CONTROL_ADDR);
        let horz = drp.read(channel, ES_HORZ_OFFSET_ADDR);
        let errc = drp.read(channel, ES_ERROR_COUNT_ADDR);
        let samp = drp.read(channel, ES_SAMPLE_COUNT_ADDR);
        uwriteln!(
            uart,
            "DRP ch{}: selftest={} es_control_status=0x{:x} es_control=0x{:x} es_horz_offset=0x{:x} es_error_count=0x{:x} es_sample_count=0x{:x}",
            channel,
            selftest,
            status.unwrap_or(0xFFFF),
            ctrl.unwrap_or(0xFFFF),
            horz.unwrap_or(0xFFFF),
            errc.unwrap_or(0xFFFF),
            samp.unwrap_or(0xFFFF)
        )
        .unwrap();
    }
    uwriteln!(uart, "DRP readout complete").unwrap();

    #[allow(clippy::empty_loop)]
    loop {}
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    loop {}
}
