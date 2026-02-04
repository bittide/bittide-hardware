// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]

use core::{fmt::Write, panic::PanicInfo};

/// Internal function for a GDB panic handler. It disables interrupts and attemps to write
/// the panic info to the provided write-able interface. It's expected that the user will
/// be running a GDB process and set a breakpoint to the panic handler so that the output
/// can be read.
pub fn gdb_panic_internal<W: Write>(writer: &mut W, info: &PanicInfo) -> ! {
    riscv::interrupt::machine::disable();
    writeln!(writer, "{info:?}").unwrap();
    loop {
        continue;
    }
}

#[macro_export]
macro_rules! gdb_panic {
    ($writer:expr) => {
        #[panic_handler]
        fn gdb_panic(info: &::core::panic::PanicInfo) -> ! {
            let mut writer = $writer;
            $crate::gdb_panic_internal(&mut writer, info);
        }
    };
}
