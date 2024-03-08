// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]
#![cfg_attr(not(test), no_main)]

use core::fmt::Write;

use heapless::String;
#[cfg(not(test))]
use riscv_rt::entry;

#[cfg(not(test))]
extern crate panic_halt;

const ADDR: *mut u8 = 0x0000_1000 as *mut u8;

fn print(s: &str) {
    for b in s.bytes() {
        unsafe {
            ADDR.write_volatile(b);
        }
    }
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    print("hello, world.\n");

    print("I am here to be debugged!\n");

    loop {
        for i in 0..30 {
            let mut s = String::<16>::new();
            let _ = writeln!(s, "Hey! {i}");
            print(&s);
        }

        print("wheeeey!\n");

        // unsafe {
        //     riscv::asm::ebreak();
        // }
    }
}

#[export_name = "UserSoft"]
fn user_soft_handler() {
    loop {
        print("INTERRUPT UserSoft");
    }
}

#[export_name = "MachineSoft"]
fn machine_soft_handler() {
    loop {
        print("INTERRUPT MachineSoft");
    }
}

#[export_name = "UserTimer"]
fn user_timer_handler() {
    loop {
        print("INTERRUPT UserTimer");
    }
}

#[export_name = "MachineTimer"]
fn machine_timer_handler() {
    loop {
        print("INTERRUPT MachineTimer");
    }
}

#[export_name = "UserExternal"]
fn user_ext_handler() {
    loop {
        print("INTERRUPT UserExternal");
    }
}

#[export_name = "MachineExternal"]
fn machine_ext_handler() {
    loop {
        print("INTERRUPT MachineExternal");
    }
}

#[export_name = "DefaultHandler"]
fn default_handler() {
    loop {
        print("INTERRUPT default handler");
    }
}

#[export_name = "ExceptionHandler"]
fn exception_handler(_trap_frame: &riscv_rt::TrapFrame) -> ! {
    riscv::interrupt::free(|| {
        print("... caught an exception. Looping forever now.\n");
    });
    loop {
        // print("");
        continue;
    }
}
