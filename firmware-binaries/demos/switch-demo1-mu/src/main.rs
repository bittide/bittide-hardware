#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::{manual_additions::index::IndexTy, switch_demo_mu::DeviceInstances};
use core::panic::PanicInfo;
use ufmt::uwriteln;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg(not(test))]
use riscv_rt::entry;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;

    uwriteln!(uart, "Hello from management unit..").unwrap();

    uwriteln!(uart, "Centering buffer occupancies").unwrap();
    for (i, eb) in [
        &INSTANCES.elastic_buffer,
        &INSTANCES.elastic_buffer_1,
        &INSTANCES.elastic_buffer_2,
        &INSTANCES.elastic_buffer_3,
        &INSTANCES.elastic_buffer_4,
        &INSTANCES.elastic_buffer_5,
        &INSTANCES.elastic_buffer_6,
    ]
    .iter()
    .enumerate()
    {
        uwriteln!(
            uart,
            "Elastic buffer {}, current occupancy: {}",
            i,
            eb.data_count()
        )
        .unwrap();
        eb.set_occupancy(0);
        eb.set_stable(true);
    }
    uwriteln!(uart, "All elastic buffers centered").unwrap();

    uwriteln!(uart, "Writing crossbar calendar").unwrap();
    let switch_cal = INSTANCES.switch_calendar;
    let nreps = {
        switch_cal.set_read_addr(const { IndexTy::<7, u8>::min_value() });
        switch_cal.shadow_entry().ve_repeat
    };
    uwriteln!(uart, "Using repetition count: {}", nreps).unwrap();
    for (i, n) in (2..=8).enumerate() {
        let idx = IndexTy::<7, u8>::new(i as u8).unwrap();
        switch_cal.set_read_addr(idx);
        let mut shadow_entry = INSTANCES.switch_calendar.shadow_entry();
        shadow_entry.ve_entry[0] = IndexTy::<9, u8>::new(n).unwrap();
        for elem in &mut shadow_entry.ve_entry[1..] {
            *elem = const { unsafe { IndexTy::<9, u8>::new_unchecked(1) } };
        }
        shadow_entry.ve_repeat = nreps;
        switch_cal.set_shadow_entry(shadow_entry);
        switch_cal.set_write_addr(idx);
    }
    switch_cal.set_shadow_depth_index(IndexTy::<7, u8>::new(6).unwrap());
    switch_cal.set_swap_active(true);
    switch_cal.set_end_of_metacycle(true);
    // TODO: Figure out why removing this breaks the test. The above `for` loop plus the
    // next couple exprs should set the correct shadow calendar and wait for it to swap
    // with the active calendar. But if I don't have the section below, and instead
    // replace it with a `switch_cal.set_swap_active(false);` then the test breaks.
    // We should figure out _WHY_ that is in order to be able to remove this
    // (theoretically) useless bit of code.
    for (i, n) in (2..=8).enumerate() {
        let idx = IndexTy::<7, u8>::new(i as u8).unwrap();
        switch_cal.set_read_addr(idx);
        let mut shadow_entry = INSTANCES.switch_calendar.shadow_entry();
        shadow_entry.ve_entry[0] = IndexTy::<9, u8>::new(n).unwrap();
        for elem in &mut shadow_entry.ve_entry[1..] {
            *elem = const { unsafe { IndexTy::<9, u8>::new_unchecked(1) } };
        }
        shadow_entry.ve_repeat = nreps;
        switch_cal.set_shadow_entry(shadow_entry);
        switch_cal.set_write_addr(idx);
    }
    switch_cal.set_shadow_depth_index(IndexTy::<7, u8>::new(6).unwrap());
    switch_cal.set_swap_active(true);
    switch_cal.set_end_of_metacycle(true);
    for i in 0..=switch_cal.shadow_depth_index().into_underlying() {
        let idx = IndexTy::<7, u8>::new(i).unwrap();
        switch_cal.set_read_addr(idx);
        uwriteln!(uart, "calendar[{}]: {:?}", i, switch_cal.shadow_entry()).unwrap();
    }
    uwriteln!(uart, "Crossbar calendar written").unwrap();

    uwriteln!(uart, "Starting UGN captures").unwrap();
    let mut capture_ugns = [
        (INSTANCES.capture_ugn, false),
        (INSTANCES.capture_ugn_1, false),
        (INSTANCES.capture_ugn_2, false),
        (INSTANCES.capture_ugn_3, false),
        (INSTANCES.capture_ugn_4, false),
        (INSTANCES.capture_ugn_5, false),
        (INSTANCES.capture_ugn_6, false),
    ];
    while capture_ugns.iter().any(|(_, done)| !done) {
        for (i, (capture_ugn, done)) in capture_ugns.iter_mut().enumerate() {
            if *done {
                continue;
            }
            if capture_ugn.has_captured() {
                uwriteln!(
                    uart,
                    "Capture UGN {}: local = {}, remote = {}",
                    i,
                    capture_ugn.local_counter(),
                    capture_ugn.remote_counter()
                )
                .unwrap();
                *done = true;
            }
        }
    }
    uwriteln!(uart, "All UGNs captured").unwrap();

    #[allow(clippy::empty_loop)]
    loop {}
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    #[allow(clippy::empty_loop)]
    loop {}
}
