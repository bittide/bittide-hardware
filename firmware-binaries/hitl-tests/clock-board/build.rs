// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_build_utils::standard_memmap_build;

/// Put the linker script somewhere the linker can find it.
fn main() {
    standard_memmap_build(
        "Si539xConfiguration.json",
        "DataMemory",
        "InstructionMemory",
    );
    println!("cargo:rerun-if-changed=build.rs");
}
