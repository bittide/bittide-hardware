#![no_std]
#![no_main]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::fmt::Write;
use riscv_rt::entry;

use contranomy_sys::println;

const FDT_ADDR: *const u8 = 0x1000_0000 as *const u8;

#[entry]
fn main() -> ! {
    unsafe {
        contranomy_sys::initialise().unwrap();
    }

    let device_tree = unsafe { fdt::Fdt::from_ptr(FDT_ADDR).unwrap() };

    println!("FDT size is {}", device_tree.total_size());

    for node in device_tree.all_nodes() {
        println!("Node {}", node.name);

        for prop in node.properties() {
            match prop_value(prop.value) {
                PropValue::String(s) => println!("  {} = {s:?}", prop.name),
                PropValue::Integer(i) => println!("  {} = {i}", prop.name),
                PropValue::TwoIntegers(a, b) => println!("  {} = ({a}, {b})", prop.name),
                PropValue::Blob(b) => println!("  {} = {b:?}", prop.name),
            }
        }
    }

    loop {
        continue;
    }
}

enum PropValue<'a> {
    String(&'a str),
    Integer(u32),
    TwoIntegers(u32, u32),
    Blob(&'a [u8]),
}

// "best effort" representation of property values
fn prop_value<'a>(prop: &'a [u8]) -> PropValue<'a> {
    if prop.len() == 0 {
        return PropValue::Blob(&[]);
    }

    if prop.len() == 1 {
        return PropValue::Integer(prop[0] as u32);
    }

    if prop[0..prop.len() - 2]
        .iter()
        .all(|c| c.is_ascii() && *c != 0)
        && prop.last().map(|c| *c == 0).unwrap_or(false)
    {
        return PropValue::String(core::str::from_utf8(&prop[0..prop.len() - 1]).unwrap());
    }

    if prop.len() == 4 {
        return PropValue::Integer(u32::from_be_bytes(prop.try_into().unwrap()));
    }

    if prop.len() == 8 {
        return PropValue::TwoIntegers(
            u32::from_be_bytes(prop[0..4].try_into().unwrap()),
            u32::from_be_bytes(prop[4..].try_into().unwrap()),
        );
    }

    return PropValue::Blob(prop);
}
