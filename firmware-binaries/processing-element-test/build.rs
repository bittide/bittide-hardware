// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use memmap_generate::build_utils::standard_static_memory_build;

fn main() {
    standard_static_memory_build("memory.x");
    println!("cargo:rerun-if-changed=build.rs");
}
