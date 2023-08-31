#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::cmp::Ordering;

use bittide_sys::clock_control::{ClockControl, SpeedChange};

#[cfg(not(test))]
use riscv_rt::entry;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut cc = unsafe { ClockControl::from_base_addr(0xC000_0000 as *const u32).unwrap() };

    let callisto_reg_addr = 0x0000_0004 as *const u32;
    let mut old_callisto_val = 0;
    loop {
        let callisto_val = unsafe { callisto_reg_addr.read_volatile() };
        let change = match callisto_val.cmp(&old_callisto_val) {
            Ordering::Greater => SpeedChange::SpeedUp,
            Ordering::Less => SpeedChange::SlowDown,
            Ordering::Equal => SpeedChange::NoChange,
        };
        old_callisto_val = callisto_val;
        cc.change_speed(change);
    }
}
