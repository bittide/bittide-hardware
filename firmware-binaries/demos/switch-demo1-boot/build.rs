// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use memmap_generate::build_utils::standard_memmap_build;

fn main() {
    standard_memmap_build("SwitchDemoBoot.json", "DataMemory", "InstructionMemory");
    println!("cargo:rerun-if-changed=build.rs");
}
