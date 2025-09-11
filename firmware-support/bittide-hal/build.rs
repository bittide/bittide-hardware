// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::collections::{BTreeMap, BTreeSet};
use std::io::Write;
use std::{fs::File, path::PathBuf};

use memmap_generate::generators::types::{ParsedType, TypeGenerator};
use memmap_generate::generators::{ident, IdentType, ItemReferences};
use memmap_generate::hal_set::{MemoryMapSet, TypeDefAnnotations};
use memmap_generate::parse::{
    BuiltinType, NamedConstructor, TypeDefinition, TypeDescription, TypeName,
};
use memmap_generate::{self as mm, HalGenerator};
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
    "#
}

fn has_float(set: &BTreeSet<TypeName>, ty: &ParsedType) -> bool {
    match ty {
        ParsedType::Float | ParsedType::Double => true,
        ParsedType::Vector(_, inner) => has_float(set, inner),
        ParsedType::Maybe(inner) => has_float(set, inner),
        ParsedType::Either(a, b) => has_float(set, a) || has_float(set, b),
        ParsedType::Tuple(parsed_types) => parsed_types.iter().any(|t| has_float(set, t)),
        ParsedType::Custom { name, args } => {
            set.contains(&name) || args.iter().any(|t| has_float(set, t))
        }
        _ => false,
    }
}

fn main() {
    let mut memory_maps = BTreeMap::new();

    let dir = memmap_dir();

    println!("cargo::rerun-if-changed={}", dir.display());

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

        let desc = mm::parse(&src).unwrap();

        let hal_name = ident(IdentType::Module, name.to_str().unwrap());

        memory_maps.insert(hal_name.to_string(), desc);
    }

    let mut gen = HalGenerator::new();

    let mut set = MemoryMapSet::new(memory_maps);
    set.filter_builtin_types();

    // annotate types
    {
        let mut has_floats = BTreeSet::new();

        let mut prev_len = 0;

        // check for references to floats or references to types that reference floats
        loop {
            set.annotate_types(|type_def: &TypeDescription, ann: &mut TypeDefAnnotations| {
                let found_float =
                    match &type_def.definition {
                        TypeDefinition::DataType(named_constructors) => named_constructors
                            .iter()
                            .any(|NamedConstructor(_, con)| match con {
                                memmap_generate::parse::Constructor::Nameless { fields } => {
                                    fields.iter().any(|ty| {
                                        let parsed = TypeGenerator::parse_type_ref(ty);
                                        has_float(&has_floats, &parsed)
                                    })
                                }
                                memmap_generate::parse::Constructor::Record { fields } => {
                                    fields.iter().any(|(_, ty)| {
                                        let parsed = TypeGenerator::parse_type_ref(ty);
                                        has_float(&has_floats, &parsed)
                                    })
                                }
                            }),
                        TypeDefinition::Newtype(_named_constructor) => todo!(),
                        TypeDefinition::Builtin(builtin_type) => {
                            matches!(builtin_type, BuiltinType::Float | BuiltinType::Double)
                        }
                        TypeDefinition::Alias(type_ref) => {
                            let ty = TypeGenerator::parse_type_ref(type_ref);
                            has_float(&has_floats, &ty)
                        }
                    };
                if found_float {
                    has_floats.insert(type_def.name.clone());
                    ann.tags.insert("uses-float".to_string());
                }
            });

            if has_floats.len() == prev_len {
                break; // converged, nice!
            }
            prev_len = has_floats.len();
        }

        set.annotate_types(|type_def: &TypeDescription, ann: &mut TypeDefAnnotations| {
            ann.derives
                .push(quote! { #[derive(Copy, Clone, PartialEq)] });

            // if there's a Float or Double then we don't want to implement ufmt::Debug

            if !has_floats.contains(&type_def.name) {
                ann.derives.push(quote! { #[derive(uDebug)] });
                ann.imports.push(quote! { use ufmt::derive::uDebug; })
            }
        });
    }

    set.filter_devices_by_tag(|tag| tag != "no-generate");

    set.filter_types(|desc, _ann| desc.name.module != "GHC.Tuple");

    // we're going for a folder structure like this
    //
    // src/
    //   shared_devices/
    //     mod.rs
    //   hals/
    //     <hal-name>/
    //       devices/
    //         mod.rs
    //       mod.rs  <- contains the device instances

    // this keeps track of all the files generated or written to, so that they
    // can be formatted
    let mut generated_files = vec![];
    let src_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src");
    let types_path = src_path.join("types");
    let shared_devices_path = src_path.join("shared_devices");

    // clear out generated shared code first
    _ = std::fs::remove_dir_all(&shared_devices_path);
    _ = std::fs::remove_dir_all(&types_path);

    // types
    {
        std::fs::create_dir_all(&types_path).expect("Create (gitignored) `src/types` directory");
        let mod_file_path = types_path.join("mod.rs");
        let mut mod_file = File::create(&mod_file_path).unwrap();
        generated_files.push(mod_file_path);

        let tys = set.types();

        for ty_name in tys.keys() {
            let name = ident(IdentType::Module, type_name(ty_name));
            writeln!(mod_file, "pub mod {name};").unwrap();
            writeln!(mod_file, "pub use {name}::*;").unwrap();
        }

        for (ty_name, (ann, def)) in tys {
            let ty_name = ident(IdentType::Module, type_name(ty_name));
            let file_path = types_path.join(format!("{ty_name}.rs"));
            let mut file = File::create(&file_path).unwrap();
            generated_files.push(file_path);

            let (item_refs, code) = gen.generate_type_description(ann, def);

            writeln!(file, "{}", lint_disables_generated_code()).unwrap();

            writeln!(file, "{}", add_imports(&item_refs)).unwrap();

            for import in &ann.imports {
                writeln!(file, "{}", import).unwrap();
            }

            writeln!(file, "{}", code).unwrap();
        }
    }

    // shared devices
    {
        std::fs::create_dir_all(&shared_devices_path)
            .expect("Create (gitignored) `src/shared_devices` directory");
        let mod_file_path = shared_devices_path.join("mod.rs");
        let mut mod_file = File::create(&mod_file_path).unwrap();
        generated_files.push(mod_file_path);

        let devs = set.shared_devices();

        for device_name in devs.keys() {
            let name = ident(IdentType::Module, type_name(device_name));
            writeln!(mod_file, "pub mod {name};").unwrap();
            writeln!(mod_file, "pub use {name}::*;").unwrap();
        }

        for (device_name, (ann, def)) in devs {
            let device_name = ident(IdentType::Module, type_name(device_name));
            let file_path = shared_devices_path.join(format!("{device_name}.rs"));
            let mut file = File::create(&file_path).unwrap();
            generated_files.push(file_path);

            let (item_refs, code) = gen.generate_device_description(ann, def);

            writeln!(file, "{}", lint_disables_generated_code()).unwrap();
            writeln!(file, "{}", add_imports(&item_refs)).unwrap();
            for import in &ann.imports {
                writeln!(file, "{}", import).unwrap();
            }

            writeln!(file, "{}", code).unwrap();
        }
    }

    // now for the different hals...

    // first clear all hals, then recreate the directory
    _ = std::fs::remove_dir_all(src_path.join("hals"));

    std::fs::create_dir_all(src_path.join("hals"))
        .expect("Create (gitignored) `src/hals` directory");

    let all_hals_mod_file_path = src_path.join("hals").join("mod.rs");
    let mut all_hals_mod_file = File::create(&all_hals_mod_file_path).unwrap();
    generated_files.push(all_hals_mod_file_path);

    let hals = set.hals();

    for (hal_name, hal_data) in hals {
        let hal_mod_name = ident(IdentType::Module, hal_name);
        let hal_path = src_path.join("hals").join(hal_mod_name.to_string());

        std::fs::create_dir_all(&hal_path)
            .unwrap_or_else(|_| panic!("Create `src/hals/{}` directory", hal_path.display()));
        let mut hal_mod_file = File::create(hal_path.join("mod.rs")).unwrap();
        generated_files.push(hal_path.join("mod.rs"));

        writeln!(hal_mod_file, "{}", lint_disables_generated_code()).unwrap();
        writeln!(hal_mod_file, "pub mod devices;").unwrap();
        writeln!(hal_mod_file).unwrap();

        writeln!(all_hals_mod_file, "pub mod {hal_mod_name};").unwrap();

        // device types
        {
            std::fs::create_dir_all(hal_path.join("devices")).unwrap();
            let mut mod_file = File::create(hal_path.join("devices").join("mod.rs")).unwrap();
            generated_files.push(hal_path.join("devices").join("mod.rs"));

            for dev_name in hal_data.devices.keys() {
                let name = ident(IdentType::Module, type_name(dev_name));
                writeln!(mod_file, "pub mod {name};").unwrap();
                writeln!(mod_file, "pub use {name}::*;").unwrap();
            }

            for (dev_name, (ann, def)) in &hal_data.devices {
                let dev_mod_name = ident(IdentType::Module, type_name(dev_name));

                let file_path = hal_path.join("devices").join(format!("{dev_mod_name}.rs"));
                let mut file = File::create(&file_path).unwrap();
                generated_files.push(file_path);

                let (item_refs, code) = gen.generate_device_description(ann, def);

                writeln!(file, "{}", lint_disables_generated_code()).unwrap();
                writeln!(file, "{}", add_imports(&item_refs)).unwrap();

                for import in &ann.imports {
                    writeln!(file, "{}", import).unwrap();
                }
                writeln!(file, "{}", code).unwrap();
            }
        }

        // then device instances
        {
            let imports = add_device_instance_imports(&set, &hal_data.instances);
            let code = gen.generate_instance_struct(&hal_data.instances);
            writeln!(hal_mod_file, "{}\n", imports).unwrap();
            writeln!(hal_mod_file, "{}", code).unwrap();
        }
    }

    memmap_generate::format::format_files(&generated_files).unwrap();
}

fn type_name(s: &str) -> &str {
    s.split(".").last().unwrap()
}

fn add_imports(imports: &ItemReferences) -> TokenStream {
    use memmap_generate::generators::types::PrimitiveType;
    let mut code = TokenStream::new();
    for prim in &imports.primitives {
        let import = match prim {
            PrimitiveType::BitVector => quote! { use crate::primitives::bitvector::BitVector; },
            PrimitiveType::Unsigned => quote! { use crate::primitives::unsigned::Unsigned; },
            PrimitiveType::Signed => quote! { use crate::primitives::signed::Signed; },
            PrimitiveType::Index => quote! { use crate::primitives::index::Index; },
            _ => quote! {},
        };
        code.extend(import);
    }

    for ty in &imports.custom_types {
        let name = type_name(&ty.base);
        let type_name = ident(IdentType::Type, name);
        let module_name = ident(IdentType::Module, name);
        code.extend(quote! {
            use crate::types::#module_name::#type_name;
        });
    }

    code
}

fn add_device_instance_imports(
    set: &MemoryMapSet,
    instances: &BTreeMap<
        String,
        Vec<(
            memmap_generate::hal_set::DeviceInstanceAnnotations,
            memmap_generate::hal_set::DeviceInstance,
        )>,
    >,
) -> TokenStream {
    let mut code = TokenStream::new();
    let shared = set.shared_devices();

    for (dev_name, _instances) in instances {
        let mod_name = ident(IdentType::Module, type_name(dev_name));
        let dev_ident = ident(IdentType::Device, type_name(dev_name));

        if shared.contains_key(dev_name) {
            code.extend(quote! { use crate::shared_devices::#mod_name::#dev_ident; });
        } else {
            code.extend(quote! { use devices::#mod_name::#dev_ident; });
        }
    }

    code
}
