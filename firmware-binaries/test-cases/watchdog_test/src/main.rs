// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use ufmt::uwriteln;

use bittide_hal::{freeze::Timer, manual_additions::timer::Instant, shared::devices::uart::Uart};

#[cfg(not(test))]
use riscv_rt::entry;

const UART_ADDR: *mut u8 = (2 << 29) as *mut u8;
const TIMER_ADDR: *mut u8 = (3 << 29) as *mut u8;
const IDLE_A_ADDR: *const () = (5 << 29) as *const ();
const IDLE_B_ADDR: *const () = (6 << 29) as *const ();

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals.
    let mut uart = unsafe { Uart::new(UART_ADDR) };
    let timer = unsafe { Timer::new(TIMER_ADDR) };

    let t0: Instant = timer.now();
    unsafe { (IDLE_A_ADDR as *mut u8).write_volatile(0) };
    let t1: Instant = timer.now();

    let diff_a = t1 - t0;

    let t0: Instant = timer.now();
    unsafe { (IDLE_B_ADDR as *mut u8).write_volatile(0) };
    let t1: Instant = timer.now();

    let diff_b = t1 - t0;
    uwriteln!(
        uart,
        "Timeout took {} microseconds",
        (diff_b - diff_a).micros()
    )
    .unwrap();
    loop {
        continue;
    }
}

#[panic_handler]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    loop {
        continue;
    }
}
