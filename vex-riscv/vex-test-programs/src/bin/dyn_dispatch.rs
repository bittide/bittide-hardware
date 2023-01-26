#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#[cfg(not(test))]
use riscv_rt::entry;

#[cfg(not(test))]
extern crate panic_halt;

const ADDR: *mut u8 = 0x2000_0000 as *mut u8;

fn print(s: &str) {
    for b in s.bytes() {
        unsafe {
            ADDR.write_volatile(b);
        }
    }
}

trait DoThing {
    fn do_thing(&self);
}

struct A;

struct B;

impl DoThing for A {
    #[inline(never)]
    fn do_thing(&self) {
        print("A");
    }
}

impl DoThing for B {
    #[inline(never)]
    fn do_thing(&self) {
        print("B");
    }
}

#[inline(never)]
fn do_thingy(x: &dyn DoThing) {
    x.do_thing();
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    do_thingy(&A);
    do_thingy(&B);

    print("\n");

    loop {
        continue;
    }
}
