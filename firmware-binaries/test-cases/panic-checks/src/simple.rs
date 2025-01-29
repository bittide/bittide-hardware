#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_sys::uart::Uart;

#[cfg(not(test))]
use riscv_rt::entry;

const UART_ADDR: *const () = (0b11 << 30) as *const ();

gdb_trace::gdb_panic! {
    unsafe { Uart::new(UART_ADDR) }
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    skooks();
    loop {}
}

misadventures! {
    am i glad hes frozen in_ there and that were out here and_ that_ hes_ the
    sheriff and__ that__ were_ frozen_ out_ here_ and___ that___ were__ in__
    there__ and____ i_ just remembered were___ out__ here__ what i__ want to know
    is wheres the_ caveman
}

macro_rules! misadventures {
    (@rev [] [$($rev:ident)*]) => {
        misadventures! {
            @defs [skooks $($rev)*]
        }
    };
    (@rev [$first:ident $($rest:ident)*] [$($rev:ident)*]) => {
        misadventures! {
            @rev [$($rest)*] [$first $($rev)*]
        }
    };
    (@defs [$last:ident]) => {
        #[inline(never)]
        fn $last() {
            panic!();
        }
    };
    (@defs [$n0:ident $n1:ident $($rest:ident)*]) => {
        #[inline(never)]
        fn $n0() {
            $n1();
        }

        misadventures! {
            @defs [$n1 $($rest)*]
        }
    };
    ($($words:ident)+) => {
        misadventures! {
            @rev [$($words)+] []
        }
    }
}

pub(crate) use misadventures;
