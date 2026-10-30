// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_build_utils::standard_memmap_build;
use std::path::PathBuf;

mod fru_image;

fn main() {
    standard_memmap_build("ProgramFmcEeprom.json", "DataMemory", "InstructionMemory");

    let out_dir = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    std::fs::write(out_dir.join("fru.bin"), fru_image::fru_image()).unwrap();

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=fru_image.rs");
}
