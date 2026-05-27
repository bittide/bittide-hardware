// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Helpers for exercising the Rust backend from tests.

use proc_macro2::TokenStream;

use crate::backends::rust::generate_device_desc;
use crate::input_language::MemoryMapDesc;
use crate::ir::monomorph::MonomorphVariants;
use crate::ir::types::IrCtx;

/// Generate the Rust device descriptions for a single, parsed memory map.
///
/// This runs the full parse-to-codegen pipeline for one HAL: it builds the IR,
/// monomorphises every register's type (nats only, as in the production build
/// scripts), and generates the Rust code for each device. Unlike the build
/// scripts in `bittide-hal`, it does *not* deduplicate types or devices across
/// multiple HALs, which makes it convenient for tests that exercise a single
/// memory map.
///
/// Returns the device name and generated code for each device.
pub fn generate_device_descs(memmap: &MemoryMapDesc) -> Vec<(String, TokenStream)> {
    use crate::ir::input_to_ir::IrInputMapping;
    use crate::ir::monomorph::passes::OnlyNats;
    use crate::ir::monomorph::Monomorpher;

    let mut ctx = IrCtx::new();
    let mut input_mapping = IrInputMapping::default();
    let hal = ctx.add_memory_map_desc(&mut input_mapping, memmap);

    let mut monomorpher = Monomorpher::new(&ctx, &input_mapping);
    let mut varis = MonomorphVariants::default();
    let mut pass = OnlyNats;

    for dev in hal.devices.handles() {
        for reg in ctx.device_descs[dev].registers.handles() {
            monomorpher.monomorph_toplevel_type_refs(
                &mut varis,
                &mut pass,
                std::iter::once(ctx.registers[reg].type_ref),
            );
        }
    }
    monomorpher.monomorph_type_descs(&mut varis, &mut pass);

    hal.devices
        .handles()
        .map(|dev| {
            let (name, code, _refs) = generate_device_desc(&ctx, &varis, dev);
            (name.to_string(), code)
        })
        .collect()
}
