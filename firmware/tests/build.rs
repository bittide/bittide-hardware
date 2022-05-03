// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::env;
use std::fs;
use std::io::Write;
use std::path::Path;

/// Put the linker script somewhere the linker can find it.
fn main() {
    let out_dir = env::var("OUT_DIR").expect("No out dir");
    let dest_path = Path::new(&out_dir).join("memory.x");
    fs::write(&dest_path, include_bytes!("memory.x")).expect("Could not write file");

    println!("cargo:rustc-link-search={}", dest_path.display());

    let target_path = Path::new("target/riscv32imc-unknown-none-elf/release");

    if !target_path.exists() {
        panic!("The firmware integration tests need to be compiled in release mode!");
    }

    let mut artifacts = vec![];

    // write `.expected` files to TARGET dir
    for entry in fs::read_dir("src/bin").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();

        if path.is_dir() {
            continue;
        }

        match path.extension() {
            Some(x) if x == "expected" => {}
            _ => continue,
        }

        let test_name = path.file_stem().unwrap().to_str().unwrap();

        // check if binary file exists
        let test_source_path = Path::new("src/bin").join(format!("{test_name}.rs"));

        if !test_source_path.exists() {
            panic!("Integration test {test_name} has an `.expected` file but no `.rs` file!");
        }

        println!("cargo:rerun-if-changed=src/bin/{test_name}.expected");
        println!("cargo:rerun-if-changed=src/bin/{test_name}.rs");

        fs::copy(&path, target_path.join(format!("{test_name}.expected"))).unwrap();

        artifacts.push(target_path.join(format!("{test_name}.expected")));
        artifacts.push(target_path.join(test_name));
    }

    // check all binary programs have a `.expected` file
    for entry in fs::read_dir("src/bin").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();

        if path.is_dir() {
            continue;
        }

        match path.extension() {
            Some(x) if x == "rs" => {}
            _ => continue,
        }

        let test_name = path.file_stem().unwrap().to_str().unwrap();

        // check if binary file exists
        let test_source_path = Path::new("src/bin").join(format!("{test_name}.expected"));

        if !test_source_path.exists() {
            panic!("Integration test {test_name} has an `.expected` file but no `.rs` file!");
        }
    }

    let mut artifact_file = fs::File::create("target/artifacts").unwrap();
    for artifact in artifacts {
        writeln!(artifact_file, "{}", artifact.display()).unwrap();
    }

    println!("cargo:rerun-if-changed=memory.x");
    println!("cargo:rerun-if-changed=build.rs");
}
