#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;

use bittide_hal::types::SpeedChange;
use core::fmt::Write;
use rand::{distributions::Uniform, rngs::SmallRng, Rng, SeedableRng};

#[cfg(not(test))]
use riscv_rt::entry;

use bittide_hal::hals::clock_control_wb as hal;

const RNG_SEED: [u8; 16] = {
    let rng_seed = core::env!("RNG_SEED").as_bytes();
    let mut out = [0; 16];

    let mut i = 0;
    while i < rng_seed.len() && (i / 8) < 16 {
        let byte = i / 8;
        let shift = i % 8;
        let val = rng_seed[i];
        i += 1;
        let val = match val {
            b'0' => 0,
            b'1' => 1,
            _ => continue,
        };
        out[byte] |= val << shift;
    }
    out
};

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

    let mut rng = SmallRng::from_seed(RNG_SEED);
    let amt = rng.gen_range(16..=32);
    write!(uart, "clockMod: [").unwrap();
    rng.sample_iter(Uniform::new_inclusive(0, 2))
        .take(amt)
        .map(|val| unsafe { core::mem::transmute::<u8, SpeedChange>(val) })
        .enumerate()
        .for_each(|(i, sc)| {
            let sep = if i + 1 < amt { ", " } else { "" };
            cc.set_change_speed(sc);
            write!(uart, "{}{sep}", sc as u8).unwrap();
        });
    writeln!(uart, "]").unwrap();

    // Mark end of transmission - should hopefully be unique enough?
    for _ in 0..cc.link_mask_pop_count() {
        cc.set_change_speed(SpeedChange::NoChange);
    }

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
