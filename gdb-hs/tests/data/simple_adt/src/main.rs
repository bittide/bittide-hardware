// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#[repr(C)]
struct WeirdAlignment {
    a: u8,
    b: u32,
    c: u16,
    d: u64,
    e: u8,
}

fn main() {
    let mut x = WeirdAlignment {
        a: 0x12,
        b: 0x34567890,
        c: 0xABCD,
        d: 0x123456789ABCDEF0,
        e: 0xFF,
    };

    x.a = 0x34;
    x.b = 0x1337_4242;
    x.c = 0xBEEF;
    x.d = 0xDEAD_10C0_DEAD_71C0;
    x.e = 0x11;

    println!("Done: {:x} {:x} {:x} {:x} {:x}", x.a, x.b, x.c, x.d, x.e);
}
