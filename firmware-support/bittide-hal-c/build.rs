// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::collections::BTreeMap;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};

use memmap_generate::backends::c::type_desc::generate_tuple_definition;
use memmap_generate::backends::c::{self as backend_c, ident, IdentType, TypeReferences};
use memmap_generate::build_utils::memmap_dir;
use memmap_generate::input_language as mm_inp;
use memmap_generate::ir::deduplicate::{deduplicate, deduplicate_type_names};
use memmap_generate::ir::input_to_ir::IrInputMapping;
use memmap_generate::ir::monomorph::passes::All;
use memmap_generate::ir::monomorph::{MonomorphVariants, Monomorpher};
use memmap_generate::ir::types::IrCtx;

fn main() {
    let memmap_path = memmap_dir();
    println!("cargo:rerun-if-changed={}", memmap_path.display());

    let memory_maps = read_memory_maps(&memmap_path);

    let mut ctx = IrCtx::new();

    let mut hals = vec![];
    let mut hal_names = vec![];

    // create IR and deduplicate
    //
    let mut input_mapping = IrInputMapping::default();
    for (name, memmap) in memory_maps.iter() {
        let hal_handles = ctx.add_memory_map_desc(&mut input_mapping, memmap);
        hals.push(hal_handles);
        hal_names.push(name.clone());
    }

    let (shared, deduped_hals) = match deduplicate(&ctx, &input_mapping, hals.iter()) {
        Ok(result) => result,
        Err(err) => {
            println!("ERROR while deduplicating!! {:?}", err);
            panic!("ERROR {:?}", err)
        }
    };

    deduplicate_type_names(&mut ctx, &shared);

    // monomorph

    let mut monomorpher = Monomorpher::new(&ctx, &input_mapping);
    let mut varis = MonomorphVariants::default();

    let mut pass = All;

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

    // we're going for a folder structure like this
    //
    // generated/
    //   types/
    //   shared_devices/
    //   hals/
    //     <hal-name>/
    //       devices/
    //     <hal-name>.h  <- contains the device instances

    let src_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("generated");
    let types_path = src_path.join("types");
    let shared_devices_path = src_path.join("shared_devices");

    // clear out generated shared code first
    _ = std::fs::remove_dir_all(&shared_devices_path);
    _ = std::fs::remove_dir_all(&types_path);

    {
        // types

        std::fs::create_dir_all(&types_path)
            .expect("Create (gitignored) `generated/types` directory");

        for handle in &shared.deduped_types {
            if ctx.type_primitives.contains(handle) {
                continue;
            }
            if ctx.type_tuples.contains(handle) {
                continue;
            }
            let (name, code, refs) =
                backend_c::type_desc::generate_type_desc(&ctx, &varis, *handle);
            let mod_name = ident(IdentType::Module, type_name(name));

            let file_path = types_path.join(format!("{mod_name}.h"));
            let mut file = File::create(&file_path).unwrap();
            with_guard(
                &mut file,
                &ident(IdentType::Define, format!("types_{mod_name}_H")),
                |file| {
                    writeln!(file, "{}", gen_imports()).unwrap();

                    generate_type_ref_imports(&ctx, &refs, file);
                    writeln!(file).unwrap();

                    writeln!(file, "{}", code).unwrap();
                },
            );
        }

        if !varis.tuple_variant.is_empty() {
            let mut tuple_file = File::create(types_path.join("tuples.h")).unwrap();
            let mut refs = TypeReferences::default();
            let mut code = String::new();

            for var in varis.tuple_variant.values() {
                generate_tuple_definition(&ctx, &varis, &var.elements, &mut refs, &mut code);
            }

            with_guard(&mut tuple_file, "TYPES_TUPLES_H", |file| {
                writeln!(file, "{}", gen_imports()).unwrap();

                generate_type_ref_imports(&ctx, &refs, file);
                writeln!(file, "{}", code).unwrap();
            });
        }
    }

    {
        // devices
        std::fs::create_dir_all(&shared_devices_path)
            .expect("Create (gitignored) `generated/shared_devices` directory");

        for dev in &shared.deduped_devices {
            if ctx.device_descs[*dev]
                .tags
                .handles()
                .map(|t| &ctx.tags[t])
                .any(|tag| tag == "no-generate")
            {
                continue;
            }

            let (dev_name, code, refs) =
                backend_c::device_desc::generate_device_desc(&ctx, &varis, *dev);
            let file_name = ident(IdentType::Module, dev_name);
            let file_path = shared_devices_path.join(format!("{}.h", file_name));
            let mut file = File::create(&file_path).unwrap();
            with_guard(
                &mut file,
                &ident(IdentType::Define, format!("SHARED_DEVICE_{file_name}_H")),
                |file| {
                    writeln!(file, "{}", gen_imports()).unwrap();

                    generate_type_ref_imports(&ctx, &refs, file);
                    writeln!(file).unwrap();

                    write!(file, "{}", code).unwrap();
                },
            );
        }
    }

    // first clear all hals, then recreate the directory
    _ = std::fs::remove_dir_all(src_path.join("hals"));

    std::fs::create_dir_all(src_path.join("hals"))
        .expect("Create (gitignored) `generated/hals` directory");

    for (deduped, hal_name) in deduped_hals.iter().zip(&hal_names) {
        let hal_mod_name = ident(IdentType::Module, hal_name);
        let hal_path = src_path.join("hals").join(hal_mod_name);

        std::fs::create_dir_all(&hal_path)
            .unwrap_or_else(|_| panic!("Create `generated/hals/{}` directory", hal_path.display()));

        let mut devices_dir_created = false;

        for dev in &deduped.devices {
            if ctx.device_descs[*dev]
                .tags
                .handles()
                .map(|t| &ctx.tags[t])
                .any(|tag| tag == "no-generate")
            {
                continue;
            }
            if !devices_dir_created {
                std::fs::create_dir_all(hal_path.join("devices")).unwrap();
                devices_dir_created = true;
            }

            let (dev_name, code, refs) =
                backend_c::device_desc::generate_device_desc(&ctx, &varis, *dev);
            let file_name = ident(IdentType::Module, dev_name);
            let file_path = hal_path.join("devices").join(format!("{}.h", file_name));
            let mut file = File::create(&file_path).unwrap();
            with_guard(
                &mut file,
                &ident(
                    IdentType::Define,
                    format!("HAL_{hal_name}_DEVICE_{file_name}_H"),
                ),
                |file| {
                    writeln!(file, "{}", gen_imports()).unwrap();

                    generate_type_ref_imports(&ctx, &refs, file);
                    writeln!(file).unwrap();

                    write!(file, "{}", code).unwrap();
                },
            );
        }

        {
            let mut instance_file = File::create(hal_path.join("device_instances.h")).unwrap();
            with_guard(
                &mut instance_file,
                &ident(IdentType::Define, format!("{hal_name}_instances_h")),
                |file| {
                    let code = backend_c::device_instances::generate_device_instances(
                        &ctx,
                        &shared,
                        hal_name,
                        deduped.tree_elem_range.handles(),
                    );
                    writeln!(file, "{code}").unwrap();
                },
            );
        }
    }
}

fn generate_type_ref_imports(ctx: &IrCtx, refs: &backend_c::TypeReferences, file: &mut impl Write) {
    if !refs.tuples.is_empty() {
        writeln!(file, "#include \"types/tuples.h\"").unwrap();
    }

    for ty in &refs.references {
        let name = &ctx.type_names[*ty];
        let mod_name = ident(IdentType::Module, &name.base);
        writeln!(file, "#include \"types/{}.h\"", mod_name).unwrap();
    }
}

fn gen_imports() -> &'static str {
    r#"#include "stdint.h"
#include "stdbool.h"
#include "stddef.h"
#include "string.h"
"#
}

fn with_guard<A>(file: &mut File, guard: &str, f: impl FnOnce(&mut File) -> A) -> A {
    writeln!(file, "#ifndef {guard}").unwrap();
    writeln!(file, "#define {guard}").unwrap();
    writeln!(file).unwrap();

    let res = f(file);

    writeln!(file, "#endif // {guard}").unwrap();

    res
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
