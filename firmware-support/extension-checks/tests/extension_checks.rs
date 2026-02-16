// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::env;

use extension_checks::{
    binary_path, git_root, read_extension_policy, read_package_name, workspace_members,
};
use memmap_generate::riscv_decode::{
    forbid_riscv_instructions_in_binary, require_riscv_instructions_in_binary,
};

fn check_binary(profile: &str, package: &str, cargo_toml: &std::path::Path) {
    let binary = binary_path(profile, package);
    if !binary.exists() {
        panic!(
            "Binary not found at {}. Build binaries first (e.g. cargo build --release).",
            binary.display()
        );
    }

    let Some(policy) = read_extension_policy(cargo_toml) else {
        panic!(
            "[package.metadata.riscv-extensions] section missing in {}",
            cargo_toml.display()
        );
    };

    require_riscv_instructions_in_binary(&binary, &policy.required)
        .unwrap_or_else(|message| panic!("{}", message));
    forbid_riscv_instructions_in_binary(&binary, &policy.forbidden)
        .unwrap_or_else(|message| panic!("{}", message));
}

#[test]
fn extension_checks_release() {
    let profile = env::var("RISCV_BINARY_PROFILE").unwrap_or_else(|_| "release".to_string());
    let firmware_binaries_root = git_root().join("firmware-binaries");
    let workspace_toml = firmware_binaries_root.join("Cargo.toml");
    let members = workspace_members(&workspace_toml);

    for (member, rel_path) in members {
        if member == "extension-checks" {
            continue;
        }

        let cargo_toml = firmware_binaries_root.join(rel_path).join("Cargo.toml");
        let package_name = read_package_name(&cargo_toml);

        check_binary(&profile, &package_name, &cargo_toml);
    }
}
