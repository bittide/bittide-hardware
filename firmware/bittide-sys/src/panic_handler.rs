// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::fmt::Write;
use core::panic::PanicInfo;

#[inline(never)]
#[panic_handler]
pub fn panic(info: &PanicInfo) -> ! {
    let _ = writeln!(crate::character_device::CharacterDevice, "{}", info);

    loop {
        continue;
    }
}
