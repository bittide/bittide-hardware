// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_build_utils::standard_memmap_build;

fn main() {
    standard_memmap_build(
        "WireDemoClockControl.json",
        "DataMemory",
        "InstructionMemory",
    );
    println!("cargo:rerun-if-changed=build.rs");
}
