#![no_std]
#![cfg_attr(not(test), no_main)]
#![feature(generic_arg_infer)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::{
    manual_additions::index::IndexTy,
    shared::types::ValidEntry,
    switch_demo_mu::{
        devices::{GatherCalendar, ScatterCalendar, SwitchCalendar},
        DeviceInstances,
    },
};
use core::panic::PanicInfo;
use ufmt::uwriteln;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg(not(test))]
use riscv_rt::entry;

// Compilation should fail if any of the following invariants are not met. In such
// a case, the type variable in `CIdxTy` should be adjusted, and the invariants adjusted.
const _: () = {
    assert!(SwitchCalendar::WRITE_ADDR_SIZE < u8::MAX as usize);
    assert!(SwitchCalendar::READ_ADDR_SIZE < u8::MAX as usize);
    assert!(SwitchCalendar::WRITE_ADDR_SIZE == SwitchCalendar::READ_ADDR_SIZE);
};

const C_WRITE_ADDR_SIZE: usize = SwitchCalendar::WRITE_ADDR_SIZE;
type CIdxTy = IndexTy<{ C_WRITE_ADDR_SIZE as u128 }, u8>;

// Compilation should fail if any of the following invariants are not met. In such
// a case, the type variable in `SgIdxTy` should be adjusted, and the invariants adjusted.
const _: () = {
    assert!(GatherCalendar::WRITE_ADDR_SIZE > u8::MAX as usize);
    assert!(GatherCalendar::WRITE_ADDR_SIZE < u16::MAX as usize);
    assert!(ScatterCalendar::WRITE_ADDR_SIZE > u8::MAX as usize);
    assert!(ScatterCalendar::WRITE_ADDR_SIZE < u16::MAX as usize);
    assert!(GatherCalendar::WRITE_ADDR_SIZE == ScatterCalendar::WRITE_ADDR_SIZE);
};

const SG_WRITE_ADDR_SIZE: usize = GatherCalendar::WRITE_ADDR_SIZE;
type SgIdxTy = IndexTy<{ SG_WRITE_ADDR_SIZE as u128 }, u16>;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    let sc = INSTANCES.switch_calendar;
    let mug = INSTANCES.gather_cal_mu;
    let mus = INSTANCES.scatter_cal_mu;
    let pe0g = INSTANCES.gather_cal_pe0;
    let pe0s = INSTANCES.scatter_cal_pe0;

    let starting_index = {
        sc.set_read_addr(CIdxTy::new(0).unwrap());
        sc.shadow_entry().ve_repeat
    };
    let repetitions = {
        sc.set_read_addr(CIdxTy::new(1).unwrap());
        sc.shadow_entry().ve_repeat
    };

    // Configure switch calendar
    for i in 0..C_WRITE_ADDR_SIZE as u8 {
        let idx = CIdxTy::new(i).unwrap();
        sc.set_write_addr(idx);
        let mut val = if sc.shadow_depth_index().into_underlying() >= i {
            sc.set_read_addr(idx);
            sc.shadow_entry()
        } else {
            Default::default()
        };
        val.ve_repeat = repetitions;
        for i in 0..val.ve_entry.len() {
            let idx = i as u8 + starting_index as u8;
            if let Some(idx) = IndexTy::<_, u8>::new(idx) {
                val.ve_entry[i] = idx;
            } else {
                break;
            }
        }
        sc.set_shadow_entry(val);
    }

    // Configure S/G calendars for MU and GPPE0
    for i in 0..SG_WRITE_ADDR_SIZE as u16 {
        let idx = SgIdxTy::new(i).unwrap();
        mug.set_write_addr(idx);
        mus.set_write_addr(idx);
        pe0g.set_write_addr(idx);
        pe0s.set_write_addr(idx);
        let entry = ValidEntry {
            ve_entry: idx,
            ve_repeat: 0,
        };
        mug.set_shadow_entry(entry);
        mus.set_shadow_entry(entry);
        pe0g.set_shadow_entry(entry);
        pe0s.set_shadow_entry(entry);
    }

    uwriteln!(uart, "Hello from management unit..").unwrap();

    #[allow(clippy::empty_loop)]
    loop {}
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    #[allow(clippy::empty_loop)]
    loop {}
}
