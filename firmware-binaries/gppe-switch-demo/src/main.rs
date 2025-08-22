#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::switch_demo_gppe::DeviceInstances;
use core::panic::PanicInfo;
use ufmt::uwriteln;

#[cfg(not(test))]
use riscv_rt::entry;

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    loop {
        continue;
    }
}

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let gather = INSTANCES.gather_unit;
    let scatter = INSTANCES.scatter_unit;
    let meta = INSTANCES.meta_pe_config;
    let dna = INSTANCES.dna;
    let mut uart = INSTANCES.uart;

    let my_dna_raw = dna.dna();
    let my_dna: [u64; 2] = [my_dna_raw as u64, (my_dna_raw >> 64) as u64];

    let mut max_idx = 0;
    let mut read_copy = [0; 64];

    let config_check = || {
        meta.read_metacycle() == 0
            || meta.read_n_addresses() == 0
            || meta.write_metacycle() == 0
            || meta.write_n_addresses() == 0
    };

    while config_check() {}

    loop {
        let cmc = gather.metacycle_count();
        let rmc = meta.read_metacycle();
        let wmc = meta.write_metacycle();
        let num_reads = meta.read_n_addresses() as usize;
        if cmc == rmc {
            let ro = meta.read_offset() as usize;
            scatter
                .scatter_memory_volatile_iter()
                .skip(ro)
                .take(num_reads)
                .enumerate()
                .for_each(|(i, val)| unsafe {
                    max_idx = i;
                    read_copy[i] = val;
                    meta.set_buffer_unchecked(i, val);
                });
        }
        if cmc == wmc {
            let wo = meta.write_offset() as usize;
            meta.buffer_volatile_iter()
                .take(num_reads)
                .chain(core::iter::once(wmc as u64))
                .chain(my_dna.into_iter())
                .enumerate()
                .for_each(|(i, val)| unsafe { gather.set_gather_memory_unchecked(i + wo, val) });
        }
        if cmc > rmc.max(wmc) {
            break;
        } else {
            gather.metacycle_register();
        }
    }
    for (i, &item) in read_copy.iter().enumerate().take(max_idx) {
        uwriteln!(uart, "{}: {}", i, item).unwrap();
    }
    #[allow(clippy::empty_loop)]
    loop {}
}
