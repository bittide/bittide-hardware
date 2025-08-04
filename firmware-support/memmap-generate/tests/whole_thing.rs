use std::fmt::Write;
use std::{collections::BTreeMap, path::PathBuf};

use memmap_generate::{
    backends::rust::{self as backend_rust, generate_type_desc, ident, IdentType},
    input_language as mm_inp,
    ir::{
        deduplicate::{self, deduplicate, deduplicate_type_names},
        input_to_ir::IrInputMapping,
        monomorph::{
            passes::{All, OnlyNats},
            MonomorphVariants, Monomorpher,
        },
        types::IrCtx,
    },
};

#[test]
fn thingy() {
    let dir = memmap_dir();

    println!("cargo::rerun-if-changed={}", dir.display());

    let memory_maps = read_memory_maps(&dir);

    let mut ctx = IrCtx::new();

    let mut hals = vec![];
    let mut hal_names = vec![];
    let mut input_mapping = IrInputMapping::default();
    for (name, memmap) in memory_maps.iter() {
        let hal_handles = ctx.add_memory_map_desc(&mut input_mapping, memmap);
        hals.push(hal_handles);
        hal_names.push(name.clone());
    }

    let (shared, deduped_hals) = match deduplicate(&ctx, &mut input_mapping, hals.iter()) {
        Ok(result) => result,
        Err(err) => {
            println!("ERROR!! {:?}", err);
            panic!("ERROR {:?}", err)
        }
    };

    deduplicate_type_names(&mut ctx, &shared);

    let mut monomorpher = Monomorpher::new(&ctx, &input_mapping);
    let mut varis = MonomorphVariants::default();

    let mut pass = // options
        All;
    OnlyNats;

    /*
    for hal in &hals {
        for desc in &ctx.device_descs[hal.devices] {
            monomorpher.monomorph_toplevel_type_refs(
                &mut varis,
                &mut pass,
                desc.type_ref_range.handles(),
            );
        }
    }
    */

    // /*

    for dev in &shared.deduped_devices {
        let desc = &ctx.device_descs[*dev];
        monomorpher.monomorph_toplevel_type_refs(
            &mut varis,
            &mut pass,
            desc.type_ref_range.handles(),
        );
    }

    for hal in &deduped_hals {
        for dev in &hal.devices {
            let desc = &ctx.device_descs[*dev];
            monomorpher.monomorph_toplevel_type_refs(
                &mut varis,
                &mut pass,
                desc.type_ref_range.handles(),
            );
        }
    }
    // */
    monomorpher.monomorph_type_descs(&mut varis, &mut pass);

    let mut type_sources = BTreeMap::<String, String>::new();

    {
        // types

        for handle in &shared.deduped_types {
            if ctx.type_primitives.contains(handle) {
                continue;
            }
            let (name, code, refs) = generate_type_desc(&ctx, &varis, *handle);
            let mod_name = ident(IdentType::Module, type_name(name));
            let source = type_sources.entry(mod_name.to_string()).or_default();
            writeln!(source, "use crate::primitives::*;").unwrap();
            writeln!(source, "{}", code).unwrap();
        }
    }

    let mut shared_device_sources = BTreeMap::<String, String>::new();
    {
        // devices
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
            let source = shared_device_sources
                .entry(file_name.to_string())
                .or_default();
            writeln!(source, "use crate::primitives::*;").unwrap();
            write!(source, "{}", code).unwrap();
        }
    }

    // HAL-specific stuff

    let mut hal_devices = BTreeMap::<String, BTreeMap<String, String>>::new();

    for ((deduped, hal), hal_name) in deduped_hals.iter().zip(&hals).zip(&hal_names) {
        let hal_mod_name = ident(IdentType::Module, hal_name);

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
            // println!("hal-specific device: {}", file_path.display());
            let source = hal_devices
                .entry(hal_name.clone())
                .or_default()
                .entry(file_name.to_string())
                .or_default();

            writeln!(source, "use crate::primitives::*;").unwrap();
            write!(source, "{}", code).unwrap();
        }
    }

    dbg!(hal_devices);
    panic!()
}

fn read_memory_maps(dir: &PathBuf) -> BTreeMap<String, mm_inp::MemoryMapDesc> {
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

fn memmap_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../")
        .join("_build")
        .join("memory_maps")
}
fn type_name(s: &str) -> &str {
    s.split(".").last().unwrap()
}
