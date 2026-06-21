#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::uwriteln;

use bittide_hal::{
    hals::axi_stream_self_test::DeviceInstances, manual_additions::axi::self_test::self_test,
};

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals.
    let uart = INSTANCES.uart;
    let tx = INSTANCES.axi_stream_tx;
    let rx = INSTANCES.axi_rx_buffer;
    self_test(uart, &tx, &rx);
    loop {
        continue;
    }
}

#[panic_handler]
fn panic_handler(info: &core::panic::PanicInfo) -> ! {
    use ufmt::uWrite;

    let mut uart = INSTANCES.uart;
    ufmt::uwrite!(uart, "Woops, I panicked!\nmsg: ").unwrap();
    uart.write_str(info.message().as_str().unwrap_or("no message"))
        .unwrap();
    ufmt::uwrite!(uart, "\nloc: ").unwrap();
    if let Some(loc) = info.location() {
        let file = loc.file();
        uart.write_str(file).unwrap();
        ufmt::uwrite!(uart, ":{}:{}", loc.line(), loc.column()).unwrap();
    }
    uwriteln!(uart, "").unwrap();
    loop {
        continue;
    }
}
