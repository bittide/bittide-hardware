#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::fmt::Write;
use core::panic::PanicInfo;

#[cfg(not(test))]
use riscv_rt::entry;

use bittide_hal::hals::clock_control_wb as hal;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let peripherals = unsafe { hal::DeviceInstances::new() };
    let mut uart = peripherals.uart;
    let cc = peripherals.clock_control;

    writeln!(uart, "nLinks: {}", cc.n_links()).unwrap();
    writeln!(uart, "linkMask: {}", cc.link_mask()[0]).unwrap();
    writeln!(uart, "linksOk: {}", cc.links_ok()[0]).unwrap();
    writeln!(uart, "linkMaskPopcnt: {}", cc.link_mask_pop_count()).unwrap();
    writeln!(uart, "linksStable: {}", cc.links_stable()[0]).unwrap();
    writeln!(uart, "linksSettled: {}", cc.links_settled()[0]).unwrap();

    write!(uart, "dataCounts: [").unwrap();
    for (idx, dc) in cc.data_counts_volatile_iter().enumerate() {
        if idx > 0 {
            write!(uart, ", ").unwrap();
        }
        write!(uart, "({idx}, {dc})").unwrap();
    }
    writeln!(uart, "]").unwrap();

    loop {
        continue;
    }
}

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    loop {
        continue;
    }
}
