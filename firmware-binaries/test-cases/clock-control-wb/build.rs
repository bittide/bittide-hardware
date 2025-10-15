// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use memmap_generate::build_utils::standard_static_memory_build;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

/// Put the linker script somewhere the linker can find it.
fn main() {
    standard_static_memory_build("memory.x");

    let now = SystemTime::now();
    let rng_seed = now.duration_since(UNIX_EPOCH).unwrap().as_millis();
    println!("cargo:rustc-env=RNG_SEED='{rng_seed:0128b}'");
    println!("cargo:rerun-if-changed=build.rs");
}
