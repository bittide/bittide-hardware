// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;

use memmap_generate::build_utils::memmap_dir;
use memmap_generate::{generate_c_header, parse};

fn main() {
    let memmap_path = memmap_dir();
    println!("cargo:rerun-if-changed={}", memmap_path.display());

    // Output directory for generated headers
    let out_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("generated");
    fs::create_dir_all(&out_dir).expect("Failed to create generated directory");

    let mut memory_maps = BTreeMap::new();

    // Read all memory map JSON files
    for entry in fs::read_dir(&memmap_path).expect("Failed to read memmap directory") {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        // Only process .json files
        if path.extension().and_then(|s| s.to_str()) != Some("json") {
            continue;
        }

        let stem = path
            .file_stem()
            .and_then(|s| s.to_str())
            .expect("Failed to get file stem");

        println!("cargo:rerun-if-changed={}", path.display());

        let src = fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("Failed to read {}: {e}", path.display()));

        let desc = parse(&src).unwrap_or_else(|e| panic!("Failed to parse memory map: {e}"));

        memory_maps.insert(stem.to_string(), desc);
    }

    // Generate C headers for each memory map
    for (name, memmap) in &memory_maps {
        let header_name = format!("{}_memmap", name.to_lowercase());
        let output_path = out_dir.join(format!("{header_name}.h"));

        let header_content = generate_c_header(memmap, &header_name);

        fs::write(&output_path, header_content)
            .unwrap_or_else(|e| panic!("Failed to write {}: {e}", output_path.display()));

        println!(
            "Generated: {name} -> {}",
            output_path.file_name().unwrap().to_string_lossy()
        );
    }
}
