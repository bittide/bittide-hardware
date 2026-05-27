// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Regression test for #1182: register `description`s must reach generated
//! Rust getter/setter functions for zero-width registers, not just for
//! scalar and vector registers.

use memmap_generate::backends::rust::testing_utils as backend_rust;
use memmap_generate::parse;

#[test]
fn zero_width_register_descriptions_reach_rust_output() {
    let memmap = parse(include_str!("fixtures/zero_width_with_desc.json")).expect("fixture parses");

    let rendered = backend_rust::generate_device_descs(&memmap)
        .iter()
        .map(|(_name, code)| code.to_string())
        .collect::<String>();

    assert!(
        rendered.contains("WO_DESCRIPTION_MARKER"),
        "expected WriteOnly zero-width setter to carry its description; got:\n{rendered}"
    );
    assert!(
        rendered.contains("RO_DESCRIPTION_MARKER"),
        "expected ReadOnly zero-width getter to carry its description; got:\n{rendered}"
    );
}
