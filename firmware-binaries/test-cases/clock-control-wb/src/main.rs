#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;

use bittide_hal::shared::devices::clock_control::{ClockControl, SpeedChange};
use bittide_hal::shared::devices::debug_register::DebugRegister;
use bittide_sys::uart::Uart;
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
    let mut uart = unsafe { Uart::new(0x2000_0000 as *const ()) };
    let cc = unsafe { ClockControl::new(0xC000_0000 as *mut u8) };
    let dbg = unsafe { DebugRegister::new(0xA000_0000 as *mut u8) };

    writeln!(uart, "nLinks: {}", cc.num_links()).unwrap();
    writeln!(uart, "linkMask: {}", cc.link_mask()).unwrap();
    writeln!(uart, "linkMaskPopcnt: {}", cc.up_links()).unwrap();
    writeln!(
        uart,
        "reframingEnabled: {}",
        if dbg.reframing_enabled() {
            "True"
        } else {
            "False"
        }
    )
    .unwrap();
    writeln!(uart, "linksStable: {}", cc.links_stable()).unwrap();
    writeln!(uart, "linksSettled: {}", cc.links_settled()).unwrap();

    write!(uart, "dataCounts: [").unwrap();
    let mut idx = 0;
    let mut first = true;
    while let Some(dc) = cc.data_counts(idx) {
        if !first {
            write!(uart, ", ").unwrap();
        }
        write!(uart, "({idx}, {dc})").unwrap();
        idx += 1;
        first = false;
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
    for _ in 0..cc.up_links() {
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
