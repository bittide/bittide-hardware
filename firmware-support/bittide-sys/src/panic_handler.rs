// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;

use crate::println;

#[inline(never)]
#[panic_handler]
pub fn panic(info: &PanicInfo) -> ! {
    match info.location() {
        Some(loc) => {
            println!("A panic happened {}:{}", loc.file(), loc.line());
        }
        None => {
            println!("A panic without location information happened, stopping execution now.");
        }
    }

    loop {
        continue;
    }
}
