#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::hals::soft_ugn_demo_management_unit::DeviceInstances;
use bittide_hal::manual_additions::timer::{Duration, WaitResult};
use bittide_hal::shared_devices::{ElasticBuffer, Timer, Uart};
use bittide_sys::link_startup::LinkStartup;
use bittide_sys::stability_detector::Stability;
use core::panic::PanicInfo;
use ufmt::{uwrite, uwriteln};

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg(not(test))]
use riscv_rt::entry;

/// Length of a single elastic buffer occupancy measurement window
const EB_WINDOW_SECS: u64 = 1;

/// Number of elastic buffer occupancy measurement windows. Together with
/// 'EB_WINDOW_SECS' this makes for a 30 minute measurement.
const EB_WINDOWS: u32 = 30 * 60;

// Declare the external C main function. Returns whether the software UGN
// discovery protocol succeeded.
extern "C" {
    fn c_main() -> bool;
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
    uwriteln!(uart, "Calling C..").unwrap();
    if unsafe { c_main() } {
        measure_elastic_buffers(&mut uart, &INSTANCES.timer, &elastic_buffers);
    }

    #[allow(clippy::empty_loop)]
    loop {}
}

/// Measure elastic buffer occupancies over 'EB_WINDOWS' windows of
/// 'EB_WINDOW_SECS' each. For every window, print the minimum and maximum
/// occupancy seen by the hardware, together with bitmasks of the buffers that
/// underflowed or overflowed. Format:
///
///   EBW <window> <time_us> min=<eb0>,..,<eb6> max=<eb0>,..,<eb6> uf=<mask> of=<mask>
///
/// The hardware status registers of a buffer are read and cleared one after
/// another, so a handful of clock cycles per window go unobserved.
fn measure_elastic_buffers(uart: &mut Uart, timer: &Timer, elastic_buffers: &[&ElasticBuffer]) {
    uwriteln!(
        uart,
        "Measuring elastic buffer occupancies: {} windows of {} s",
        EB_WINDOWS,
        EB_WINDOW_SECS
    )
    .unwrap();

    let window = Duration::from_secs(EB_WINDOW_SECS);
    elastic_buffers
        .iter()
        .for_each(|eb| eb.set_clear_status_registers(true));
    let mut window_end = timer.now() + window;

    for window_nr in 0..EB_WINDOWS {
        if let WaitResult::AlreadyPassed = timer.wait_until(window_end) {
            uwriteln!(uart, "EB window {} deadline missed", window_nr).unwrap();
        }
        let now = timer.now();

        let mut mins = [0i8; 7];
        let mut maxs = [0i8; 7];
        let mut underflows = 0u8;
        let mut overflows = 0u8;
        for (i, eb) in elastic_buffers.iter().enumerate() {
            mins[i] = eb.min_data_count_seen().into_inner();
            maxs[i] = eb.max_data_count_seen().into_inner();
            underflows |= (eb.underflow() as u8) << i;
            overflows |= (eb.overflow() as u8) << i;
            eb.set_clear_status_registers(true);
        }

        uwrite!(uart, "EBW {} {}", window_nr, now.micros()).unwrap();
        write_csv(uart, " min=", &mins);
        write_csv(uart, " max=", &maxs);
        uwriteln!(uart, " uf={} of={}", underflows, overflows).unwrap();

        window_end += window;
    }

    uwriteln!(uart, "EB measurement done").unwrap();
}

/// Write 'prefix' followed by comma separated 'values'
fn write_csv(uart: &mut Uart, prefix: &str, values: &[i8]) {
    uwrite!(uart, "{}", prefix).unwrap();
    for (i, value) in values.iter().enumerate() {
        if i != 0 {
            uwrite!(uart, ",").unwrap();
        }
        uwrite!(uart, "{}", value).unwrap();
    }
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    loop {}
}
