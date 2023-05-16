// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use crate::uart::Uart;
use core::fmt::Write;
static mut PANIC_UART: Option<Uart> = None;

pub unsafe fn set_panic_handler_uart(uart: Uart) {
    PANIC_UART = Some(uart);
}

#[panic_handler]
fn panic_handler(info: &core::panic::PanicInfo) -> ! {
    let Some(uart) = (unsafe {
        PANIC_UART.as_mut()
    }) else {
        loop {
            continue;
        }
    };

    let _ = writeln!(uart, "{info}");
    loop {
        continue;
    }
}
