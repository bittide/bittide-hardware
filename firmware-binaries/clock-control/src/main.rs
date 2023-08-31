#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

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
        let mut change = SpeedChange::NoChange;
        if callisto_val > old_callisto_val {
            change = SpeedChange::SpeedUp;
        } else if callisto_val > old_callisto_val {
            change = SpeedChange::SlowDown;
        }
        old_callisto_val = callisto_val;
        cc.change_speed(change);
    }
}
