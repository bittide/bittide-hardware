#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;

use bittide_hal::shared_devices::clock_control::ClockControl;
use bittide_hal::shared_devices::uart::Uart;
use bittide_hal::types::SpeedChange;
use core::fmt::Write;
use rand::{distributions::Uniform, rngs::SmallRng, Rng, SeedableRng};

#[cfg(not(test))]
use riscv_rt::entry;

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
#[allow(clippy::empty_loop)]
fn main() -> ! {
    #[allow(clippy::zero_ptr)] // we might want to change the address!
    let mut uart = unsafe { Uart::new((0b010 << 29) as *mut u8) };
    let cc = unsafe { ClockControl::new((0b011 << 29) as *mut u8) };

    writeln!(uart, "nLinks: {}", cc.n_links()).unwrap();
    writeln!(uart, "linkMask: {}", cc.link_mask()).unwrap();
    writeln!(uart, "linksOk: {}", cc.links_ok()).unwrap();
    writeln!(uart, "linkMaskPopcnt: {}", cc.link_mask_pop_count()).unwrap();
    writeln!(uart, "linksStable: {}", cc.links_stable()).unwrap();
    writeln!(uart, "linksSettled: {}", cc.links_settled()).unwrap();

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

    loop {}
}

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    loop {
        continue;
    }
}
