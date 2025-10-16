// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use memmap_generate::build_utils::standard_memmap_build;

fn main() {
    standard_memmap_build("Ethernet.json", "DataMemory", "InstructionMemory");
    println!("cargo:rerun-if-changed=build.rs");
}
