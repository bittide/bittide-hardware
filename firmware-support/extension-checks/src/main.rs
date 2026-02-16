// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Extension checker - validate RISC-V instruction extension usage in compiled binaries

use std::path::PathBuf;
use std::process::ExitCode;

use clap::{Parser, Subcommand};

use extension_checks::{
    binary_path, git_root, read_extension_policy, read_package_name, workspace_members,
    ExtensionPolicy,
};
use memmap_generate::build_utils::RiscvExtension;
use memmap_generate::riscv_decode::find_used_extensions;

#[derive(Parser)]
#[command(name = "extension-checker")]
#[command(about = "Check RISC-V instruction extension usage in compiled binaries")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Check a specific binary
    Check {
        /// Build profile (e.g., "debug" or "release")
        #[arg(short, long)]
        profile: String,

        /// Binary name (package name)
        #[arg(short, long)]
        binary: String,

        /// Path to Cargo.toml (optional, will be inferred from workspace if not provided)
        #[arg(short, long)]
        cargo_toml: Option<PathBuf>,
    },
    /// Automatically discover and check all workspace members with riscv-extensions metadata
    Discover {
        /// Build profile (e.g., "debug" or "release")
        #[arg(short, long, default_value = "release")]
        profile: String,

        /// Require that all packages have the metadata fields set
        #[arg(short, long, default_value = "false")]
        require_metadata: bool,

        /// Path to workspace Cargo.toml (optional, will use firmware-binaries workspace by default)
        #[arg(short, long)]
        workspace: Option<PathBuf>,
    },
}

#[derive(Debug)]
#[allow(dead_code)]
struct CheckResult {
    binary_name: String,
    policy: ExtensionPolicy,
    used_extensions: Vec<RiscvExtension>,
    missing_required: Vec<RiscvExtension>,
    found_forbidden: Vec<RiscvExtension>,
}

impl CheckResult {
    fn passed(&self) -> bool {
        self.missing_required.is_empty() && self.found_forbidden.is_empty()
    }
}

fn format_extensions(exts: &[RiscvExtension]) -> String {
    if exts.is_empty() {
        "none".to_string()
    } else {
        exts.iter()
            .map(|e| format!("{:?}", e))
            .collect::<Vec<_>>()
            .join(", ")
    }
}

fn check_binary(
    profile: &str,
    binary_name: &str,
    cargo_toml_path: Option<PathBuf>,
) -> Result<CheckResult, String> {
    let cargo_toml = if let Some(path) = cargo_toml_path {
        path
    } else {
        // Try to find it in the workspace
        let firmware_binaries_root = git_root().join("firmware-binaries");
        let workspace_toml = firmware_binaries_root.join("Cargo.toml");
        let members = workspace_members(&workspace_toml);

        // Find the member with this binary name
        let mut found = None;
        for (_member, rel_path) in members {
            let candidate = firmware_binaries_root.join(&rel_path).join("Cargo.toml");
            if read_package_name(&candidate) == binary_name {
                found = Some(candidate);
                break;
            }
        }

        found.ok_or_else(|| {
            format!(
                "Could not find package '{}' in workspace. Try specifying --cargo-toml explicitly.",
                binary_name
            )
        })?
    };

    let Some(policy) = read_extension_policy(&cargo_toml) else {
        return Err(format!(
            "[package.metadata.riscv-extensions] section missing in {}",
            cargo_toml.display()
        ));
    };

    let binary = binary_path(profile, binary_name);
    if !binary.exists() {
        return Err(format!(
            "Binary not found at {}. Build binaries first (e.g., cargo build --release).",
            binary.display()
        ));
    }

    // Print binary info
    println!("\n{}", "=".repeat(70));
    println!("Binary: {}", binary_name);
    println!("Path: {}", binary.display());
    println!("Policy:");
    println!(
        "  Required extensions: {}",
        format_extensions(&policy.required)
    );
    println!(
        "  Forbidden extensions: {}",
        format_extensions(&policy.forbidden)
    );

    // Find used extensions
    let used_extensions = find_used_extensions(&binary)?;
    println!("Used extensions: {}", format_extensions(&used_extensions));

    // Check for missing required extensions
    let missing_required: Vec<_> = policy
        .required
        .iter()
        .filter(|ext| !used_extensions.contains(ext))
        .copied()
        .collect();

    // Check for forbidden extensions
    let found_forbidden: Vec<_> = policy
        .forbidden
        .iter()
        .filter(|ext| used_extensions.contains(ext))
        .copied()
        .collect();

    Ok(CheckResult {
        binary_name: binary_name.to_string(),
        policy,
        used_extensions,
        missing_required,
        found_forbidden,
    })
}

fn check_single_binary(
    profile: &str,
    binary_name: &str,
    cargo_toml_path: Option<PathBuf>,
) -> Result<(), String> {
    let result = check_binary(profile, binary_name, cargo_toml_path)?;

    // Print status
    if result.passed() {
        println!("Status: ✓ PASSED");
    } else {
        println!("Status: ✗ FAILED");
        if !result.missing_required.is_empty() {
            println!(
                "  Missing required extensions: {}",
                format_extensions(&result.missing_required)
            );
        }
        if !result.found_forbidden.is_empty() {
            println!(
                "  Found forbidden extensions: {}",
                format_extensions(&result.found_forbidden)
            );
        }
    }
    println!("{}", "=".repeat(70));

    if result.passed() {
        Ok(())
    } else {
        Err(format!("Binary {} failed extension checks", binary_name))
    }
}

fn discover_and_check(
    profile: &str,
    require_metadata: bool,
    workspace_path: Option<PathBuf>,
) -> Result<(), String> {
    let workspace_toml = if let Some(path) = workspace_path {
        path
    } else {
        git_root().join("firmware-binaries").join("Cargo.toml")
    };

    let workspace_root = workspace_toml
        .parent()
        .ok_or_else(|| "Invalid workspace path".to_string())?;

    let members = workspace_members(&workspace_toml);
    let mut results = Vec::new();
    let mut skipped_count = 0;
    let mut missing_metadata = Vec::new();

    for (member, rel_path) in members {
        // Skip the extension-checks package itself
        if member == "extension-checks" {
            continue;
        }

        let cargo_toml = workspace_root.join(&rel_path).join("Cargo.toml");
        let package_name = read_package_name(&cargo_toml);

        let policy = read_extension_policy(&cargo_toml);

        if policy.is_none() {
            if require_metadata {
                missing_metadata.push(package_name.clone());
            } else {
                println!("⊘ Skipping {} (no riscv-extensions metadata)", package_name);
                skipped_count += 1;
            }
            continue;
        }

        match check_binary(profile, &package_name, Some(cargo_toml)) {
            Ok(result) => {
                if result.passed() {
                    println!("Status: ✓ PASSED");
                } else {
                    println!("Status: ✗ FAILED");
                    if !result.missing_required.is_empty() {
                        println!(
                            "  Missing required extensions: {}",
                            format_extensions(&result.missing_required)
                        );
                    }
                    if !result.found_forbidden.is_empty() {
                        println!(
                            "  Found forbidden extensions: {}",
                            format_extensions(&result.found_forbidden)
                        );
                    }
                }
                println!("{}", "=".repeat(70));
                results.push(result);
            }
            Err(e) => {
                println!("Status: ✗ ERROR");
                println!("  {}", e);
                println!("{}", "=".repeat(70));
                // Track as a failure but continue checking other binaries
            }
        }
    }

    // Print summary report
    println!("\n{}", "=".repeat(70));
    println!("SUMMARY");
    println!("{}", "=".repeat(70));

    if !missing_metadata.is_empty() {
        println!("\n❌ Packages missing metadata:");
        for pkg in &missing_metadata {
            println!("  - {}", pkg);
        }
    }

    let failed_results: Vec<_> = results.iter().filter(|r| !r.passed()).collect();

    if !failed_results.is_empty() {
        println!("\n❌ Binaries that failed extension checks:");
        for result in &failed_results {
            println!("\n  {}:", result.binary_name);
            if !result.missing_required.is_empty() {
                println!(
                    "    Missing required: {}",
                    format_extensions(&result.missing_required)
                );
            }
            if !result.found_forbidden.is_empty() {
                println!(
                    "    Found forbidden: {}",
                    format_extensions(&result.found_forbidden)
                );
            }
        }
    }

    let passed_count = results.iter().filter(|r| r.passed()).count();
    let failed_count = failed_results.len();

    println!("\nResults:");
    println!("  ✓ Passed: {}", passed_count);
    println!("  ✗ Failed: {}", failed_count);
    if skipped_count > 0 {
        println!("  ⊘ Skipped: {}", skipped_count);
    }
    println!("{}", "=".repeat(70));

    if !missing_metadata.is_empty() || !failed_results.is_empty() {
        Err(format!(
            "{} package(s) failed checks",
            missing_metadata.len() + failed_results.len()
        ))
    } else {
        Ok(())
    }
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Check {
            profile,
            binary,
            cargo_toml,
        } => check_single_binary(&profile, &binary, cargo_toml),
        Commands::Discover {
            profile,
            require_metadata,
            workspace,
        } => discover_and_check(&profile, require_metadata, workspace),
    };

    match result {
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("\nError: {}", e);
            ExitCode::FAILURE
        }
    }
}
