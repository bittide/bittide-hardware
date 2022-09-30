#![no_std]
#![no_main]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_sys::gppe::{gather_unit::GatherUnit, scatter_unit::ScatterUnit};

use riscv_rt::entry;

const FRAME_SIZE: usize = 8;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let init = unsafe { bittide_sys::Initialiser::new().unwrap() };
    let mut scatter_unit = unsafe {
        init.initialise_scatter_unit::<FRAME_SIZE>("scatter-unit")
            .unwrap()
    };
    let mut gather_unit = unsafe {
        init.initialise_gather_unit::<FRAME_SIZE>("gather-unit")
            .unwrap()
    };

    wait_for_start(&mut gather_unit, &mut scatter_unit);

    loop {
        for _ in 0..4 {
            gather_unit.wait_for_new_metacycle();
        }

        ping(&mut gather_unit, &mut scatter_unit);
    }
}

fn ping(gu: &mut GatherUnit<FRAME_SIZE>, su: &mut ScatterUnit<FRAME_SIZE>) {
    let mut num_buf = [0; 8];
    su.read_frame_memory(&mut num_buf);
    let num = u64::from_le_bytes(num_buf);

    write_to_io(gu, su, num);

    gu.write_frame_memory(0, num + 1);
}

fn write_to_io(_gu: &mut GatherUnit<FRAME_SIZE>, _su: &mut ScatterUnit<FRAME_SIZE>, _num: u64) {
    // stub for now
}

fn wait_for_start(gu: &mut GatherUnit<FRAME_SIZE>, su: &mut ScatterUnit<FRAME_SIZE>) {
    let mut num_buf = [0; 8];
    loop {
        su.read_frame_memory(&mut num_buf);
        let num = u64::from_le_bytes(num_buf);

        const START_MAGIC_NUMBER: u64 = 1;

        if num == START_MAGIC_NUMBER {
            break;
        }
    }

    // start counting at 0
    gu.write_frame_memory(0, 0u64);
}
