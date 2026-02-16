// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Utilities for checking RISC-V extension usage in compiled binaries

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use memmap_generate::build_utils::RiscvExtension;
use toml::Value;

/// Parse a RISC-V extension name from a string
pub fn parse_extension(value: &str) -> Option<RiscvExtension> {
    match value {
        "C" => Some(RiscvExtension::C),
        "M" => Some(RiscvExtension::M),
        "F" => Some(RiscvExtension::F),
        "D" => Some(RiscvExtension::D),
        _ => None,
    }
}

/// Parse a list of extension names from TOML value
fn parse_extension_list(value: &Value, key: &str) -> Vec<RiscvExtension> {
    let list = value
        .get(key)
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();

    list.iter()
        .filter_map(|entry| entry.as_str())
        .map(|entry| {
            parse_extension(entry)
                .unwrap_or_else(|| panic!("Unknown RISC-V extension '{}' in metadata", entry))
        })
        .collect()
}

/// Extension policy for a package
#[derive(Debug, Clone)]
pub struct ExtensionPolicy {
    pub required: Vec<RiscvExtension>,
    pub forbidden: Vec<RiscvExtension>,
}

/// Read extension policy from a Cargo.toml file
///
/// Returns None if the package.metadata.riscv-extensions section is missing
pub fn read_extension_policy(cargo_toml: &Path) -> Option<ExtensionPolicy> {
    let text = std::fs::read_to_string(cargo_toml)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", cargo_toml.display(), e));
    let doc: Value = text
        .parse()
        .unwrap_or_else(|e| panic!("Failed to parse {}: {}", cargo_toml.display(), e));

    let metadata = doc
        .get("package")
        .and_then(|pkg| pkg.get("metadata"))
        .and_then(|meta| meta.get("riscv-extensions"))?;

    let required = parse_extension_list(metadata, "required");
    let forbidden = parse_extension_list(metadata, "forbidden");

    Some(ExtensionPolicy {
        required,
        forbidden,
    })
}

/// Read the package name from a Cargo.toml file
pub fn read_package_name(cargo_toml: &Path) -> String {
    let text = std::fs::read_to_string(cargo_toml)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", cargo_toml.display(), e));
    let doc: Value = text
        .parse()
        .unwrap_or_else(|e| panic!("Failed to parse {}: {}", cargo_toml.display(), e));
    doc.get("package")
        .and_then(|pkg| pkg.get("name"))
        .and_then(|name| name.as_str())
        .unwrap_or_else(|| panic!("Missing [package].name in {}", cargo_toml.display()))
        .to_string()
}

/// Find workspace members from a Cargo.toml workspace file
pub fn workspace_members(workspace_toml: &Path) -> BTreeMap<String, PathBuf> {
    let text = std::fs::read_to_string(workspace_toml)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", workspace_toml.display(), e));
    let doc: Value = text
        .parse()
        .unwrap_or_else(|e| panic!("Failed to parse {}: {}", workspace_toml.display(), e));
    let members = doc
        .get("workspace")
        .and_then(|ws| ws.get("members"))
        .and_then(|list| list.as_array())
        .cloned()
        .unwrap_or_default();

    let mut result = BTreeMap::new();
    for entry in members {
        let Some(member) = entry.as_str() else {
            continue;
        };
        result.insert(member.to_string(), PathBuf::from(member));
    }

    result
}

/// Get the git root directory
pub fn git_root() -> PathBuf {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let output = std::process::Command::new("git")
        .args(["rev-parse", "--show-toplevel"])
        .current_dir(&manifest_dir)
        .output()
        .expect("Failed to execute git command");

    if !output.status.success() {
        panic!("Failed to find git root");
    }

    let git_root = String::from_utf8(output.stdout)
        .expect("Git output is not valid UTF-8")
        .trim()
        .to_string();

    PathBuf::from(git_root)
}

/// Get the path to a compiled binary
pub fn binary_path(profile: &str, package: &str) -> PathBuf {
    let target_dir = if let Ok(dir) = std::env::var("CARGO_TARGET_DIR") {
        PathBuf::from(dir)
    } else {
        git_root()
            .join("_build")
            .join("cargo")
            .join("firmware-binaries")
    };

    target_dir
        .join("riscv32imafc-unknown-none-elf")
        .join(profile)
        .join(package)
}
