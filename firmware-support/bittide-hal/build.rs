// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::collections::{BTreeMap, BTreeSet};
use std::io::Write;
use std::path::Path;
use std::{fs::File, path::PathBuf};

use memmap_generate::input_language as mm_inp;

use memmap_generate::ir::deduplicate::{deduplicate, deduplicate_type_names};
use memmap_generate::ir::input_to_ir::IrInputMapping;
use memmap_generate::ir::monomorph::passes::OnlyNats;
use memmap_generate::ir::monomorph::{MonomorphVariants, Monomorpher};
use memmap_generate::ir::types::IrCtx;

use memmap_generate::backends::rust::{
    self as backend_rust, generate_device_instances, generate_type_desc, ident, IdentType,
    TypeReferences,
};
use proc_macro2::TokenStream;
use quote::quote;

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
#![allow(clippy::useless_transmute)]
    "#
}

fn main() {
    let dir = memmap_dir();

    println!("cargo::rerun-if-changed={}", dir.display());

    let memory_maps = read_memory_maps(&dir);

    let mut ctx = IrCtx::new();

    let mut hals = vec![];
    let mut hal_names = vec![];

    // create IR and deduplicate.

    let mut input_mapping = IrInputMapping::default();
    for (name, memmap) in memory_maps.iter() {
        let hal_handles = ctx.add_memory_map_desc(&mut input_mapping, memmap);
        hals.push(hal_handles);
        hal_names.push(name.clone());
    }

    let (shared, deduped_hals) = match deduplicate(&ctx, &input_mapping, hals.iter()) {
        Ok(result) => result,
        Err(err) => {
            println!("ERROR!! {:?}", err);
            panic!("ERROR {:?}", err)
        }
    };

    deduplicate_type_names(&mut ctx, &shared);

    // monomorph

    let mut monomorpher = Monomorpher::new(&ctx, &input_mapping);
    let mut varis = MonomorphVariants::default();

    let mut pass = OnlyNats;

    for dev in &shared.deduped_devices {
        let desc = &ctx.device_descs[*dev];
        for reg in &ctx.registers[desc.registers] {
            monomorpher.monomorph_toplevel_type_refs(
                &mut varis,
                &mut pass,
                std::iter::once(reg.type_ref),
            );
        }
    }

    for hal in &deduped_hals {
        for dev in &hal.devices {
            let desc = &ctx.device_descs[*dev];
            for reg in &ctx.registers[desc.registers] {
                monomorpher.monomorph_toplevel_type_refs(
                    &mut varis,
                    &mut pass,
                    std::iter::once(reg.type_ref),
                );
            }
        }
    }

    monomorpher.monomorph_type_descs(&mut varis, &mut pass);

    // add annotations (derives etc)
    //
    let mut annotations = backend_rust::Annotations::default();
    annotate_types(&ctx, &varis, &mut annotations, DebugType::UDebug);

    // used to track files for formatting later on
    let mut generated_files = vec![];

    // we're going for a folder structure like this
    //
    // src/
    //   types/
    //     mod.rs
    //   shared_devices/
    //     mod.rs
    //   hals/
    //     <hal-name>/
    //       devices/
    //         mod.rs
    //       mod.rs  <- contains the device instances

    let src_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src");
    let types_path = src_path.join("types");
    let shared_devices_path = src_path.join("shared_devices");

    // clear out generated shared code first
    _ = std::fs::remove_dir_all(&shared_devices_path);
    _ = std::fs::remove_dir_all(&types_path);

    {
        // types

        std::fs::create_dir_all(&types_path).expect("Create (gitignored) `src/types` directory");
        let mod_file_path = types_path.join("mod.rs");
        let mut mod_file = File::create(&mod_file_path).unwrap();
        generated_files.push(mod_file_path);

        for handle in &shared.deduped_types {
            if ctx.type_primitives.contains(handle) {
                continue;
            }
            if ctx.type_tuples.contains(handle) {
                continue;
            }
            let (name, code, refs) = generate_type_desc(&ctx, &varis, &annotations, *handle);
            let mod_name = ident(IdentType::Module, type_name(name));
            writeln!(mod_file, "pub mod {mod_name};").unwrap();
            writeln!(mod_file, "pub use {mod_name}::*;").unwrap();

            let file_path = types_path.join(format!("{mod_name}.rs"));
            let mut file = File::create(&file_path).unwrap();
            generated_files.push(file_path);

            writeln!(file, "{}", lint_disables_generated_code()).unwrap();
            writeln!(file, "{}", generate_type_ref_imports(&ctx, &refs)).unwrap();
            writeln!(file, "{}", code).unwrap();
        }
    }

    {
        // devices
        std::fs::create_dir_all(&shared_devices_path)
            .expect("Create (gitignored) `src/shared_devices` directory");
        let mod_file_path = shared_devices_path.join("mod.rs");
        let mut mod_file = File::create(&mod_file_path).unwrap();
        generated_files.push(mod_file_path);

        for dev in &shared.deduped_devices {
            if ctx.device_descs[*dev]
                .tags
                .handles()
                .map(|t| &ctx.tags[t])
                .any(|tag| tag == "no-generate")
            {
                continue;
            }

            let (dev_name, code, refs) = backend_rust::generate_device_desc(&ctx, &varis, *dev);
            let file_name = ident(IdentType::Module, dev_name);
            let file_path = shared_devices_path.join(format!("{}.rs", file_name));
            let mut file = File::create(&file_path).unwrap();
            writeln!(file, "{}", lint_disables_generated_code()).unwrap();
            writeln!(file, "{}", generate_type_ref_imports(&ctx, &refs)).unwrap();
            write!(file, "{}", code).unwrap();
            generated_files.push(file_path);

            writeln!(mod_file, "pub mod {file_name};").unwrap();
            writeln!(mod_file, "pub use {file_name}::*;").unwrap();
        }
    }

    // HAL-specific stuff

    // first clear all hals, then recreate the directory
    _ = std::fs::remove_dir_all(src_path.join("hals"));

    std::fs::create_dir_all(src_path.join("hals"))
        .expect("Create (gitignored) `src/hals` directory");

    let all_hals_mod_file_path = src_path.join("hals").join("mod.rs");
    let mut all_hals_mod_file = File::create(&all_hals_mod_file_path).unwrap();
    generated_files.push(all_hals_mod_file_path);

    for (deduped, hal_name) in deduped_hals.iter().zip(&hal_names) {
        let hal_mod_name = ident(IdentType::Module, hal_name);
        let hal_path = src_path.join("hals").join(hal_mod_name.to_string());

        writeln!(all_hals_mod_file, "pub mod {hal_mod_name};").unwrap();

        std::fs::create_dir_all(&hal_path)
            .unwrap_or_else(|_| panic!("Create `src/hals/{}` directory", hal_path.display()));
        let mut hal_mod_file = File::create(hal_path.join("mod.rs")).unwrap();
        generated_files.push(hal_path.join("mod.rs"));
        writeln!(hal_mod_file, "{}", lint_disables_generated_code()).unwrap();

        std::fs::create_dir_all(hal_path.join("devices")).unwrap();
        let mut mod_file = File::create(hal_path.join("devices").join("mod.rs")).unwrap();
        generated_files.push(hal_path.join("devices").join("mod.rs"));

        writeln!(hal_mod_file, "pub mod devices;").unwrap();

        for dev in &deduped.devices {
            if ctx.device_descs[*dev]
                .tags
                .handles()
                .map(|t| &ctx.tags[t])
                .any(|tag| tag == "no-generate")
            {
                continue;
            }
            let (dev_name, code, refs) = backend_rust::generate_device_desc(&ctx, &varis, *dev);
            let file_name = ident(IdentType::Module, dev_name);
            let file_path = hal_path.join("devices").join(format!("{}.rs", file_name));

            let mut file = File::create(&file_path).unwrap();

            writeln!(file, "{}", lint_disables_generated_code()).unwrap();
            writeln!(file, "{}", generate_type_ref_imports(&ctx, &refs)).unwrap();
            write!(file, "{}", code).unwrap();
            generated_files.push(file_path);

            writeln!(mod_file, "pub mod {file_name};").unwrap();
            writeln!(mod_file, "pub use {file_name}::*;").unwrap();
        }

        {
            let code = generate_device_instances(
                &ctx,
                &shared,
                hal_name,
                deduped.tree_elem_range.handles(),
            );
            writeln!(hal_mod_file, "{code}").unwrap();
        }
    }

    memmap_generate::format::format_files(&generated_files).unwrap();
}

#[derive(Debug, Copy, Clone)]
pub enum DebugType {
    Debug,
    UDebug,
    Both,
}

fn annotate_types(
    ctx: &IrCtx,
    varis: &MonomorphVariants,
    annotations: &mut backend_rust::Annotations,
    debug_type: DebugType,
) {
    let mut has_float = BTreeSet::new();

    for (variant_handle, variant) in varis.type_ref_variants.iter() {
        let ty_desc = &ctx.type_descs[variant.original_type_desc];

        for ty_handle in ty_desc.type_ref_range.handles() {
            let ty_handle = variant
                .variable_substitutions
                .get(&ty_handle)
                .copied()
                .unwrap_or(ty_handle);
            match &ctx.type_refs[ty_handle] {
                memmap_generate::ir::types::TypeRef::Float
                | memmap_generate::ir::types::TypeRef::Double => {
                    has_float.insert(variant_handle);
                }
                _ => {}
            }
        }
    }

    loop {
        let size_before = has_float.len();

        for (variant_handle, variant) in varis.type_ref_variants.iter() {
            let ty_desc = &ctx.type_descs[variant.original_type_desc];

            for ty_handle in ty_desc.type_ref_range.handles() {
                let ty_handle = variant
                    .variable_substitutions
                    .get(&ty_handle)
                    .copied()
                    .unwrap_or(ty_handle);
                if let Some(sub) = variant.monomorph_type_ref_substitutions.get(&ty_handle) {
                    if has_float.contains(sub) {
                        has_float.insert(variant_handle);
                    }
                } else if let Some(sub) = varis.type_refs.get(&ty_handle) {
                    if has_float.contains(sub) {
                        has_float.insert(variant_handle);
                    }
                }
            }
        }

        let size_after = has_float.len();

        if size_after == size_before {
            break;
        }
    }

    for (variant_handle, variant) in varis.type_ref_variants.iter() {
        if ctx.type_aliases.contains(&variant.original_type_desc) {
            continue;
        }
        let type_annots = annotations
            .type_annotation
            .entry(variant_handle)
            .or_default();

        let type_imports = annotations.type_imports.entry(variant_handle).or_default();

        let has_float_here = has_float.contains(&variant_handle);
        match (debug_type, has_float_here) {
            (DebugType::Debug | DebugType::Both, true) => {
                type_annots.insert("#[derive(Debug)]".to_string());
            }
            (_, true) => {}
            (DebugType::Debug, _) => {
                type_annots.insert("#[derive(Debug)]".to_string());
            }
            (DebugType::UDebug, _) => {
                type_imports.insert("ufmt::derive::uDebug".to_string());
                type_annots.insert("#[derive(uDebug)]".to_string());
            }
            (DebugType::Both, _) => {
                type_imports.insert("ufmt::derive::uDebug".to_string());
                type_annots.insert("#[derive(Debug, uDebug)]".to_string());
            }
        }

        type_annots.insert("#[derive(Clone, Copy)]".to_string());
        if !has_float_here {
            type_annots.insert("#[derive(PartialEq, Eq, PartialOrd, Ord)]".to_string());
        }
    }
}

fn generate_type_ref_imports(ctx: &IrCtx, refs: &TypeReferences) -> TokenStream {
    let mut code = TokenStream::new();
    for ty_ref in &refs.references {
        let name = &ctx.type_names[*ty_ref].base;
        let module = ident(IdentType::Module, name);
        code.extend(quote! { use crate::types::#module::*; });
    }
    code
}

fn read_memory_maps(
    dir: &Path,
) -> BTreeMap<String, memmap_generate::input_language::MemoryMapDesc> {
    let mut memory_maps = BTreeMap::new();

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
        println!("Parsing memory map: {}", name.to_str().unwrap());
        let src = std::fs::read_to_string(&path).unwrap();

        let desc = mm_inp::parse(&src).unwrap();

        let hal_name = name.to_str().unwrap(); // ident(IdentType::Module, name.to_str().unwrap());

        memory_maps.insert(hal_name.to_string(), desc);
    }

    memory_maps
}

fn type_name(s: &str) -> &str {
    s.split(".").last().unwrap()
}
