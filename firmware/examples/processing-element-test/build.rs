// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::env;
use std::fs;
use std::path::Path;

/// Put the linker script somewhere the linker can find it.
fn main() {
    let out_dir = env::var("OUT_DIR").expect("No out dir");
    let dest_path = Path::new(&out_dir).join("memory.x");
    fs::write(dest_path, include_bytes!("memory.x")).expect("Could not write file");

    if env::var("CARGO_CFG_TARGET_ARCH").unwrap() == "riscv32" {
        println!("cargo:rustc-link-arg=-Tmemory.x");
        println!("cargo:rustc-link-arg=-Tlink.x"); // linker script from riscv-rt
    }
    println!("cargo:rustc-link-search={out_dir}");

    println!("cargo:rerun-if-changed=memory.x");
    println!("cargo:rerun-if-changed=build.rs");
}
