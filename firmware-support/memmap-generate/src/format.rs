// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::{path::Path, process::Command};

pub fn format_files(files: &[impl AsRef<Path>]) -> Result<(), String> {
    // we might switch this out for a different formatter at some point
    rustfmt_files(files)
}

fn rustfmt_files(files: &[impl AsRef<Path>]) -> Result<(), String> {
    let mut command = Command::new("rustfmt");

    for file in files {
        let path = file.as_ref();
        command.arg(path);
    }

    let status = command.status().expect("failed to run rustfmt");
    if !status.success() {
        return Err(format!("`rustfmt` failed: {:?}", command));
    }

    Ok(())
}
