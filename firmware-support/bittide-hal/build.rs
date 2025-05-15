// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::collections::BTreeMap;
use std::io::Write;
use std::{fs::File, path::PathBuf};

use memmap_generate::hal_set::MemoryMapSet;
use memmap_generate::{self as mm, generate_rust_wrappers};

fn memmap_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../")
        .join("_build")
        .join("memory_maps")
}

fn lint_disables_generated_code() -> &'static str {
    r#"
#![allow(unused_imports)]
#![allow(clippy::missing_safety_doc)]
#![allow(clippy::empty_docs)]
#![allow(clippy::unused_unit)]
#![allow(non_snake_case)]
    "#
}

fn main() {
    let mut memory_maps = BTreeMap::new();

    let dir = memmap_dir();
    for dir in dir.read_dir().unwrap() {
        let dir = dir.unwrap();
        if dir.path().is_dir() {
            continue;
        }

        let path = dir.path();

        let Some(extension) = path.extension() else {
            continue;
        };

        if extension != "json" {
            continue;
        }

        let Some(name) = path.file_stem() else {
            continue;
        };

        let src = std::fs::read_to_string(&path).unwrap();

        let desc = mm::parse(&src).unwrap();
        let hal_name = name.to_str().unwrap().to_lowercase();

        memory_maps.insert(hal_name, desc);
    }

    let mut set = MemoryMapSet::new(memory_maps);
    set.filter_devices_by_tag(|tag| tag != "no-generate");

    // we're going for a folder structure like this
    //
    // src/
    //   shared/
    //     devices/
    //       mod.rs
    //     types/
    //       mod.rs
    //   hals/
    //     <hal-name>/
    //       devices/
    //         mod.rs
    //       types/
    //         mod.rs
    //       mod.rs  <- contains the device instances

    let gen_config = mm::GenerateConfig {
        debug_derive_mode: mm::DebugDerive::None,
        item_scope_mode: mm::ItemScopeMode::OneFile,
    };

    let shared = set.shared();
    let shared_wrapper = generate_rust_wrappers(shared, &gen_config);

    let shared_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src")
        .join("shared");

    // types
    {
        std::fs::create_dir_all(shared_path.join("types"))
            .expect("Create (gitignored) `src/shared/types` directory");
        let mut mod_file = File::create(shared_path.join("types").join("mod.rs")).unwrap();

        for ty_name in shared_wrapper.type_defs.keys() {
            writeln!(mod_file, "pub mod {};", ty_name.to_lowercase()).unwrap();
            writeln!(mod_file, "pub use {}::*;", ty_name.to_lowercase()).unwrap();
        }

        for (ty_name, def) in &shared_wrapper.type_defs {
            let mut file = File::create(
                shared_path
                    .join("types")
                    .join(format!("{}.rs", ty_name.to_lowercase())),
            )
            .unwrap();
            writeln!(file, "{}", lint_disables_generated_code()).unwrap();

            writeln!(file, "pub use crate::shared::types::*;").unwrap();
            writeln!(file, "{}", def).unwrap();
        }
    }

    // devices
    {
        std::fs::create_dir_all(shared_path.join("devices"))
            .expect("Create (gitignored) `src/devices` directory");
        let mut mod_file = File::create(shared_path.join("devices").join("mod.rs")).unwrap();

        for device_name in shared_wrapper.device_defs.keys() {
            writeln!(mod_file, "pub mod {};", device_name.to_lowercase()).unwrap();
            writeln!(mod_file, "pub use {}::*;", device_name.to_lowercase()).unwrap();
        }

        for (device_name, def) in &shared_wrapper.device_defs {
            let mut file = File::create(
                shared_path
                    .join("devices")
                    .join(format!("{}.rs", device_name.to_lowercase())),
            )
            .unwrap();
            writeln!(file, "{}", lint_disables_generated_code()).unwrap();
            writeln!(file, "pub use crate::shared::types::*;").unwrap();
            writeln!(file, "{}", def).unwrap();
        }
    }

    // now for the different hals...

    std::fs::create_dir_all(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("src")
            .join("hals"),
    )
    .expect("Create (gitignored) `src/hals` directory");

    let mut all_hals_mod_file = File::create(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("src")
            .join("hals")
            .join("mod.rs"),
    )
    .unwrap();

    for (hal_name, hal_data) in set.non_shared() {
        let wrapper = generate_rust_wrappers(hal_data, &gen_config);
        let hal_mod_name = hal_name.to_lowercase();
        let hal_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("src")
            .join("hals")
            .join(&hal_mod_name);

        std::fs::create_dir_all(&hal_path)
            .unwrap_or_else(|_| panic!("Create `src/hals/{}` directory", hal_path.display()));
        let mut hal_mod_file = File::create(hal_path.join("mod.rs")).unwrap();

        writeln!(hal_mod_file, "{}", lint_disables_generated_code()).unwrap();
        writeln!(hal_mod_file, "pub mod types;").unwrap();
        writeln!(hal_mod_file, "pub mod devices;").unwrap();
        writeln!(hal_mod_file, "pub use types::*;").unwrap();
        writeln!(hal_mod_file, "pub use devices::*;").unwrap();
        writeln!(hal_mod_file).unwrap();
        writeln!(hal_mod_file, "pub use crate::shared::types::*;").unwrap();
        writeln!(hal_mod_file, "pub use crate::shared::devices::*;").unwrap();
        writeln!(hal_mod_file).unwrap();

        writeln!(all_hals_mod_file, "pub mod {hal_mod_name};").unwrap();

        // first types
        {
            std::fs::create_dir_all(hal_path.join("types")).unwrap();
            let mut mod_file = File::create(hal_path.join("types").join("mod.rs")).unwrap();

            for ty_name in wrapper.type_defs.keys() {
                writeln!(mod_file, "pub mod {};", ty_name.to_lowercase()).unwrap();
                writeln!(mod_file, "pub use {}::*;", ty_name.to_lowercase()).unwrap();
            }

            for (ty_name, def) in &wrapper.type_defs {
                let mut file = File::create(
                    hal_path
                        .join("types")
                        .join(format!("{}.rs", ty_name.to_lowercase())),
                )
                .unwrap();
                writeln!(file, "{}", lint_disables_generated_code()).unwrap();
                writeln!(file, "pub use crate::shared::types::*;").unwrap();
                writeln!(file, "pub use crate::hals::{hal_mod_name}::types::*;").unwrap();
                writeln!(file, "{}", def).unwrap();
            }
        }

        // then device types
        {
            std::fs::create_dir_all(hal_path.join("devices")).unwrap();
            let mut mod_file = File::create(hal_path.join("devices").join("mod.rs")).unwrap();

            for dev_name in wrapper.device_defs.keys() {
                writeln!(mod_file, "pub mod {};", dev_name.to_lowercase()).unwrap();
                writeln!(mod_file, "pub use {}::*;", dev_name.to_lowercase()).unwrap();
            }

            for (dev_name, def) in &wrapper.device_defs {
                let mut file = File::create(
                    hal_path
                        .join("devices")
                        .join(format!("{}.rs", dev_name.to_lowercase())),
                )
                .unwrap();
                writeln!(file, "{}", lint_disables_generated_code()).unwrap();
                writeln!(file, "pub use crate::shared::types::*;").unwrap();
                writeln!(file, "pub use crate::hals::{hal_mod_name}::types::*;").unwrap();
                writeln!(file, "{}", def).unwrap();
            }
        }

        // then device instances
        {
            writeln!(hal_mod_file, "{}", wrapper.device_instances_struct).unwrap();
        }
    }
}
