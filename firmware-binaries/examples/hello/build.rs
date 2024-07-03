// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::env;
use std::fs;
use std::path::Path;

use memmap_generate as mm;
use memmap_generate::generators::{
    generate_rust_wrappers, types::DebugDerive, GenerateConfig, ItemScopeMode,
};

/// Put the linker script somewhere the linker can find it.
fn main() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let manifest_dir = Path::new(&manifest_dir);
    let src_dir = manifest_dir.join("src");
    let build_dir = manifest_dir.join("../../../_build");
    let mm_dir = build_dir.join("memory_maps");

    let out_dir = env::var("OUT_DIR").expect("No out dir");
    let out_dir = Path::new(&out_dir);

    // read memory map for the hardware this program will run on and generate
    // wrappers.

    let memory_map = mm_dir.join("vexriscv.json");

    eprintln!("memory map path: {}", memory_map.display());
    let mm_src = std::fs::read_to_string(&memory_map).unwrap();

    let memory_map = mm::parse(&mm_src).unwrap();

    let generate_config = GenerateConfig {
        debug_derive_mode: DebugDerive::None,
        item_scope_mode: ItemScopeMode::OneFile,
    };
    let wrappers = generate_rust_wrappers(&memory_map, &generate_config);
    let wrapper_code = {
        let mut s = String::new();

        s += r#"
        #![allow(non_camel_case_types)]
        #![allow(non_snake_case)]
        #![allow(dead_code)]

        "#;

        for (_, ty_src) in &wrappers.type_defs {
            s += &ty_src.to_string();
        }
        for (_, dev_src) in &wrappers.device_defs {
            s += &dev_src.to_string();
        }
        s += &wrappers.device_instances_struct.to_string();
        s
    };

    let wrapper_src_path = src_dir.join("wrappers");
    let _ = std::fs::create_dir(&wrapper_src_path);

    let memory_map_wrapper_path = wrapper_src_path.join("memory_map.rs");

    std::fs::write(&memory_map_wrapper_path, wrapper_code).unwrap();

    let dest_path = out_dir.join("memory.x");
    fs::write(dest_path, include_bytes!("memory.x")).expect("Could not write file");

    if env::var("CARGO_CFG_TARGET_ARCH").unwrap() == "riscv32" {
        println!("cargo:rustc-link-arg=-Tmemory.x");
        println!("cargo:rustc-link-arg=-Tlink.x"); // linker script from riscv-rt
    }
    println!("cargo:rustc-link-search={}", out_dir.display());

    println!("cargo:rerun-if-changed=memory.x");
    println!("cargo:rerun-if-changed=build.rs");
}
