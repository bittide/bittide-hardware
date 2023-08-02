#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#[cfg(not(test))]
use riscv_rt::entry;

use bittide_sys::{print, println};

const FDT_ADDR: *const u8 = 0x1000_0000 as *const u8;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    unsafe {
        let init = bittide_sys::Initialiser::new().unwrap();
        init.initialise_character_device("character-device")
            .unwrap();
    }

    let device_tree = unsafe { fdt::Fdt::from_ptr(FDT_ADDR).unwrap() };

    println!("FDT size is {}", device_tree.total_size());

    print_fdt(0, &device_tree.find_node("/").unwrap());

    loop {
        continue;
    }
}

fn print_fdt(depth: usize, node: &fdt::node::FdtNode) {
    fn indent(n: usize) {
        for _ in 0..n {
            print!("  ");
        }
    }

    indent(depth);
    println!("Node {}", node.name);

    for prop in node.properties() {
        indent(depth + 1);
        match prop_value(prop.value) {
            PropValue::String(s) => println!("{} = {}", prop.name, s),
            PropValue::Integer(i) => {
                println!("{} = {}", prop.name, best_fit_integer_repr(i).as_str())
            }
            PropValue::TwoIntegers(a, b) => println!(
                "{} = ({}, {})",
                prop.name,
                best_fit_integer_repr(a).as_str(),
                best_fit_integer_repr(b).as_str()
            ),
            PropValue::Blob(b) => println!("{} = {:?}", prop.name, b),
        }
    }

    for child in node.children() {
        println!();
        print_fdt(depth + 1, &child);
    }
}

enum PropValue<'a> {
    String(&'a str),
    Integer(u32),
    TwoIntegers(u32, u32),
    Blob(&'a [u8]),
}

// "best effort" representation of property values
//
// Because FDT properties are byte sequences, they do not have an explicit
// type associated with them which could guide the visual or internal
// representation.
//
// This function attempts to interpret the data in a number of different ways
// and choses the one that makes the most sense (for example string vs number
// pair vs binary blob).
fn prop_value(prop: &'_ [u8]) -> PropValue<'_> {
    if prop.is_empty() {
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

    PropValue::Blob(prop)
}

// Here "best fit" refers to the number system in which the number should be
// displayed in. For some numbers, a base 10 representation is "nicer", while
// for others a base 16 representation is more useful.
fn best_fit_integer_repr(num: u32) -> heapless::String<20> {
    use ufmt::uwrite;

    let mut dec = heapless::String::new();
    let mut hex = heapless::String::new();

    uwrite!(dec, "{}", num).unwrap();
    uwrite!(hex, "{:X}", num).unwrap();

    let dec_zeroes = dec.chars().filter(|c| *c == '0').count();
    let hex_zeroes = hex.chars().filter(|c| *c == '0').count();

    if hex_zeroes > dec_zeroes {
        hex.clear();
        uwrite!(hex, "0x{:X}", num).unwrap();
        hex
    } else {
        dec
    }
}
