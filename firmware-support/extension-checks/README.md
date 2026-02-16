<!--
SPDX-FileCopyrightText: 2026 Google LLC

SPDX-License-Identifier: Apache-2.0
-->
# Extension Checker

A utility for validating RISC-V instruction extension usage in compiled firmware binaries.

## Overview

The extension-checker ensures that firmware binaries:
- Only use instruction extensions supported by their target CPU
- Utilize expected instruction extensions for optimal performance
- Have proper metadata declarations in their `Cargo.toml` files

## Usage

### Check a specific binary

```bash
cargo run --bin extension-checker -- check \
  --profile release \
  --binary <binary-name>
```

Example:
```bash
cargo run --bin extension-checker -- check --profile release --binary watchdog_test
```

### Auto-discover and check all workspace members

```bash
cargo run --bin extension-checker -- discover \
  --profile release
```

This will:
1. Find all packages in the firmware-binaries workspace
2. Check packages that have `[package.metadata.riscv-extensions]` sections
3. Skip packages without the metadata (with a warning)
4. Report any extension violations

### Require metadata on all packages

To enforce that all packages have the metadata fields set:

```bash
cargo run --bin extension-checker -- discover \
  --profile release \
  --require-metadata
```

This will return a non-zero exit code if any packages are missing the metadata section.

## Cargo.toml Metadata

Each firmware package should declare its extension requirements in `Cargo.toml`:

```toml
[package.metadata.riscv-extensions]
required = ["C", "M"]  # Extensions that SHOULD be used
forbidden = ["F", "D"]  # Extensions that MUST NOT be used
```

### Supported Extensions

Currently supported extensions:
- `C` - Compressed instructions
- `M` - Integer multiply/divide
- `F` - Single-precision floating-point
- `D` - Double-precision floating-point

### Required Extensions

Extensions listed in `required` indicate that the compiled binary SHOULD use these instructions. If they're not found, it may indicate:
- Missed optimization opportunities
- Incorrect compiler flags
- Code that doesn't actually use the feature

This generates a warning but may not fail the build.

### Forbidden Extensions

Extensions listed in `forbidden` indicate that the compiled binary MUST NOT use these instructions, typically because:
- The target CPU doesn't support them
- The CPU variant doesn't have the extension enabled

Using forbidden extensions will cause the check to fail with an error.

## Exit Codes

- `0` - All checks passed
- `1` - One or more checks failed

## Integration with Build Scripts

The functions used by extension-checker are also available in the build.rs scripts via the `memmap-generate` crate:

```rust
use memmap_generate::riscv_decode::{
    forbid_riscv_instructions_in_binary,
    require_riscv_instructions_in_binary,
};
```

## Examples

### Check all release binaries
```bash
cargo run --bin extension-checker -- discover --profile release
```

### Check with strict metadata requirements
```bash
cargo run --bin extension-checker -- discover --profile release --require-metadata
```

### Check a specific debug binary
```bash
cargo run --bin extension-checker -- check --profile debug --binary my_firmware
```

### Specify custom Cargo.toml path
```bash
cargo run --bin extension-checker -- check \
  --profile release \
  --binary my_firmware \
  --cargo-toml /path/to/Cargo.toml
```
