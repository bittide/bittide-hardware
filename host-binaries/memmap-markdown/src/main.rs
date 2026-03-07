// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::{collections::BTreeMap, fs::File, path::Path};

use heck::{ToShoutySnakeCase, ToSnakeCase, ToUpperCamelCase};
use memmap_generate::{
    input_language::{self as mm_inp},
    ir::{
        input_to_ir::IrInputMapping,
        types::{
            DeviceDescription, HalHandles, IrCtx, PathComp, TreeElem, TreeElemType,
            TypeConstructor, TypeDefinition, TypeDescription, TypeRef,
        },
    },
    storage::{Handle, HandleRange},
};
use std::fmt::Write;

struct GenContext<'a> {
    types_path: &'a str,
    devices_path: &'a str,
}

fn main() {
    use std::io::Write;
    let memmap_dir = memmap_generate::build_utils::memmap_dir();
    let memory_maps = read_memory_maps(&memmap_dir);

    let out_dir = memmap_dir.join("../markdown-docs");

    _ = std::fs::remove_dir_all(&out_dir);

    std::fs::create_dir_all(&out_dir).unwrap();

    let mut ctx = IrCtx::new();

    let mut hals = vec![];
    let mut hal_names = vec![];

    let mut input_mapping = IrInputMapping::default();
    for (name, memmap) in memory_maps.iter() {
        let hal_handles = ctx.add_memory_map_desc(&mut input_mapping, memmap);
        hals.push(hal_handles);
        hal_names.push(name.clone());
    }

    for (name, handles) in hal_names.into_iter().zip(hals) {
        let hal_dir = out_dir.join(name.to_snake_case());
        std::fs::create_dir(&hal_dir).unwrap();

        let mut instance_file = File::create(hal_dir.join("instances.md")).unwrap();
        let gen_ctx = GenContext {
            types_path: "./types/",
            devices_path: "./devices/",
        };

        writeln!(instance_file, "# HAL {name}\n").unwrap();
        let instance_table = generate_hal_instances(&gen_ctx, &ctx, &handles);
        writeln!(instance_file, "{instance_table}\n").unwrap();

        writeln!(instance_file, "\n## Types\n").unwrap();

        let types_dir = hal_dir.join("types");
        std::fs::create_dir(&types_dir).unwrap();

        for ty in handles.types.handles() {
            let desc = &ctx.type_descs[ty];
            let name = &ctx.type_names[desc.name];

            if ctx.type_primitives.contains(&ty) {
                continue;
            }

            if ctx.type_tuples.contains(&ty) {
                continue;
            }

            let gen_ctx = GenContext {
                types_path: "./",
                devices_path: "../devices/",
            };

            let mut ty_file =
                File::create(types_dir.join(format!("{}.md", name.base.to_upper_camel_case())))
                    .unwrap();

            writeln!(ty_file, "### {}\n", name.base.to_upper_camel_case()).unwrap();
            let type_desc = generate_type_definition(&gen_ctx, &ctx, desc);
            writeln!(ty_file, "{type_desc}\n").unwrap();

            writeln!(
                instance_file,
                "- [`{}`](types/{}.md)",
                name.base.to_upper_camel_case(),
                name.base.to_upper_camel_case()
            )
            .unwrap();
        }

        let devices_dir = hal_dir.join("devices");
        std::fs::create_dir(&devices_dir).unwrap();
        writeln!(instance_file, "\n## Devices\n").unwrap();

        for dev in handles.devices.handles() {
            let desc = &ctx.device_descs[dev];
            let name = &ctx.identifiers[desc.name];
            let mut dev_file =
                File::create(devices_dir.join(format!("{}.md", name.to_upper_camel_case())))
                    .unwrap();
            let gen_ctx = GenContext {
                types_path: "../types/",
                devices_path: "./",
            };

            writeln!(dev_file, "### {}", name.to_upper_camel_case()).unwrap();
            let device_table = generate_device(&gen_ctx, &ctx, desc);
            writeln!(dev_file, "{device_table}").unwrap();

            writeln!(
                instance_file,
                "- [`{}`](devices/{}.md)",
                name.to_upper_camel_case(),
                name.to_upper_camel_case()
            )
            .unwrap();
        }
    }
}

fn generate_type_definition(gen_ctx: &GenContext, ctx: &IrCtx, desc: &TypeDescription) -> String {
    let mut output = String::new();

    let name = &ctx.type_names[desc.name];
    let args = desc.param_names;

    let mut args_def = String::new();

    if args.len > 0 {
        args_def.push('<');

        for (i, arg) in args.handles().enumerate() {
            let name = ctx.identifiers[arg].to_shouty_snake_case();

            if i > 0 {
                args_def.push_str(", ");
            }

            args_def.push_str(&format!("`{name}`"));

            if ctx.type_param_nats.contains(&arg) {
                args_def.push_str(": number");
            } else {
                args_def.push_str(": type");
            }
        }

        args_def.push('>');
    }

    fn is_fieldless(ctx: &IrCtx, cons: HandleRange<TypeConstructor>) -> bool {
        ctx.type_constructors[cons]
            .iter()
            .all(|con| con.field_types.len == 0)
    }

    match &desc.definition {
        TypeDefinition::DataType {
            names,
            constructors,
        } if constructors.len == 1 => {
            writeln!(output, "Data type\n").unwrap();

            let con_name = names.start;
            let constructor = constructors.start;

            let con_name = ctx.identifiers[con_name].to_upper_camel_case();
            write!(
                output,
                "`newtype` `{}`{} = `{con_name}`",
                name.base.to_upper_camel_case(),
                args_def
            )
            .unwrap();
            let cons = &ctx.type_constructors[constructor];
            if let Some(names) = cons.field_names {
                // named
                writeln!(output, " {{\n").unwrap();
                for (ty, name) in cons.field_types.handles().zip(names.handles()) {
                    let name = ctx.identifiers[name].to_snake_case();
                    writeln!(
                        output,
                        "&emsp;`{}`: {},\n",
                        name,
                        generate_type_name(gen_ctx, ctx, ty, 1)
                    )
                    .unwrap();
                }
                writeln!(output, "}}\n").unwrap();
            } else {
                // nameless
                for ty in cons.field_types.handles() {
                    write!(output, " {}", generate_type_name(gen_ctx, ctx, ty, 1)).unwrap();
                }
                writeln!(output, "\n").unwrap();
            }
        }
        TypeDefinition::DataType {
            names,
            constructors,
        } if is_fieldless(ctx, *constructors) => {
            // "enum" like ADT
            writeln!(output, "Enum\n").unwrap();

            writeln!(
                output,
                "`enum` `{}`{}\n",
                name.base.to_upper_camel_case(),
                args_def
            )
            .unwrap();

            for (i, name) in names.handles().enumerate() {
                let name = ctx.identifiers[name].to_upper_camel_case();
                if i == 0 {
                    write!(output, "&emsp;= ").unwrap();
                } else {
                    write!(output, "&emsp;| ").unwrap();
                }
                writeln!(output, "`{name}`\n").unwrap();
            }
        }
        TypeDefinition::DataType {
            names,
            constructors,
        } => {
            // "SoP" ADT
            writeln!(output, "Data type\n").unwrap();

            writeln!(
                output,
                "`data` `{}`{}\n",
                name.base.to_upper_camel_case(),
                args_def
            )
            .unwrap();

            for (i, (name, con)) in names.handles().zip(constructors.handles()).enumerate() {
                let con_name = ctx.identifiers[name].to_upper_camel_case();
                if i == 0 {
                    write!(output, "&emsp;= ").unwrap();
                } else {
                    write!(output, "&emsp;| ").unwrap();
                }
                write!(output, "`{con_name}`",).unwrap();
                let cons = &ctx.type_constructors[con];
                if let Some(names) = cons.field_names {
                    // named
                    writeln!(output, " {{\n").unwrap();
                    for (ty, name) in cons.field_types.handles().zip(names.handles()) {
                        let name = ctx.identifiers[name].to_snake_case();
                        writeln!(
                            output,
                            "&emsp;&emsp;`{}`: {},\n",
                            name,
                            generate_type_name(gen_ctx, ctx, ty, 1)
                        )
                        .unwrap();
                    }
                    writeln!(output, "&emsp;}}\n").unwrap();
                } else {
                    // nameless
                    for ty in cons.field_types.handles() {
                        write!(output, " {}", generate_type_name(gen_ctx, ctx, ty, 1)).unwrap();
                    }
                    writeln!(output, "\n").unwrap();
                }
            }
        }
        TypeDefinition::Newtype {
            name: con_name,
            constructor,
        } => {
            writeln!(output, "Newtype\n").unwrap();

            let con_name = ctx.identifiers[*con_name].to_upper_camel_case();
            write!(
                output,
                "`newtype {}`{} = `{con_name}`",
                name.base.to_upper_camel_case(),
                args_def
            )
            .unwrap();
            let cons = &ctx.type_constructors[*constructor];
            if let Some(names) = cons.field_names {
                // named
                writeln!(output, " {{\n").unwrap();
                for (ty, name) in cons.field_types.handles().zip(names.handles()) {
                    let name = ctx.identifiers[name].to_snake_case();
                    writeln!(
                        output,
                        "&emsp;{}: {},\n",
                        name,
                        generate_type_name(gen_ctx, ctx, ty, 1)
                    )
                    .unwrap();
                }
                writeln!(output, "}}\n").unwrap();
            } else {
                // nameless
                for ty in cons.field_types.handles() {
                    write!(output, " {}", generate_type_name(gen_ctx, ctx, ty, 1)).unwrap();
                }
                writeln!(output, "\n").unwrap();
            }
        }
        TypeDefinition::Builtin(_builtin_type) => {
            panic!("Builtin types should not have type descriptions generated about them.")
        }
        TypeDefinition::Synonym(handle) => {
            writeln!(output, "Type synonym\n",).unwrap();

            writeln!(
                output,
                "`type {}`{}` = `{}",
                name.base.to_upper_camel_case(),
                args_def,
                generate_type_name(gen_ctx, ctx, *handle, 0)
            )
            .unwrap();
        }
    }

    output
}

fn generate_hal_instances(gen_ctx: &GenContext, ctx: &IrCtx, handles: &HalHandles) -> String {
    let mut instances = vec![];
    for handle in handles.tree_elem_range.handles() {
        let ty = &ctx.tree_elem_types[handle.cast()];
        let device_name = if let TreeElemType::DeviceInstance { device_name } = ty {
            &ctx.identifiers[*device_name]
        } else {
            continue;
        };
        let elem = &ctx.tree_elems[handle];
        instances.push((device_name, elem, handle));
    }

    instances.sort_by(|(_, a, _), (_, b, _)| a.absolute_addr.cmp(&b.absolute_addr));

    let unnameds = instance_unnameds(ctx, &instances);

    let mut output = String::new();

    writeln!(
        output,
        "| Absolute Address | Instance Name | Device Name | Tags |"
    )
    .unwrap();
    writeln!(
        output,
        "|------------------|---------------|-------------|------|"
    )
    .unwrap();

    for (device_name, elem, _handle) in instances {
        let instance_name = instance_name(ctx, &unnameds, device_name, elem.path);
        let device_name = device_name.to_upper_camel_case();
        let tags = &ctx.tags[elem.tags];
        let tags = tags
            .iter()
            .map(|t| format!("`{t}`"))
            .collect::<Vec<_>>()
            .join(", ");
        writeln!(
            output,
            "| 0x{:X} | `{}` | [`{}`]({}{}.md) | {} |",
            elem.absolute_addr, instance_name, device_name, gen_ctx.devices_path, device_name, tags,
        )
        .unwrap();
    }

    output
}

fn generate_type_name(
    gen_ctx: &GenContext,
    ctx: &IrCtx,
    ty: Handle<TypeRef>,
    depth: usize,
) -> String {
    let with_brace = |s| {
        if depth > 0 { format!("({s})") } else { s }
    };
    match &ctx.type_refs[ty] {
        TypeRef::BitVector(handle) => with_brace(format!(
            "`BitVector` {}",
            generate_type_name(gen_ctx, ctx, *handle, depth + 1)
        )),
        TypeRef::Unsigned(handle) => with_brace(format!(
            "`Unsigned` {}",
            generate_type_name(gen_ctx, ctx, *handle, depth + 1)
        )),
        TypeRef::Signed(handle) => with_brace(format!(
            "`Signed` {}",
            generate_type_name(gen_ctx, ctx, *handle, depth + 1)
        )),
        TypeRef::Index(handle) => with_brace(format!(
            "`Index` {}",
            generate_type_name(gen_ctx, ctx, *handle, depth + 1)
        )),
        TypeRef::Bool => "`bool`".to_string(),
        TypeRef::Float => "`float`".to_string(),
        TypeRef::Double => "`double`".to_string(),
        TypeRef::Vector(len, inner) => with_brace({
            format!(
                "`Vec` {} {}",
                generate_type_name(gen_ctx, ctx, *len, depth + 1),
                generate_type_name(gen_ctx, ctx, *inner, depth + 1)
            )
        }),
        TypeRef::Tuple(handle_range) if handle_range.len == 0 => "`unit`".to_string(),
        TypeRef::Tuple(handle_range) => {
            let mut s = "(".to_string();
            for (i, handle) in handle_range.handles().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }
                s.push_str(&generate_type_name(gen_ctx, ctx, handle, 0));
            }
            s.push(')');
            s
        }
        TypeRef::Variable(handle) => ctx.identifiers[*handle].to_shouty_snake_case(),
        TypeRef::Nat(n) => format!("{n}"),
        TypeRef::Reference { name, args } => {
            let ty_name = &ctx.type_names[*name];
            let base_name = ty_name.base.to_upper_camel_case();
            let link = format!("{}{base_name}.md", gen_ctx.types_path);
            let mut output = format!("[`{base_name}`]({link})");
            if args.len > 0 {
                output.push('<');
                for (i, arg) in args.handles().enumerate() {
                    if i > 0 {
                        output.push_str(", ");
                    }
                    output.push_str(&generate_type_name(gen_ctx, ctx, arg, 0));
                }
                output.push('>');
            }
            output
        }
    }
}

fn generate_device(gen_ctx: &GenContext, ctx: &IrCtx, desc: &DeviceDescription) -> String {
    let mut output = String::new();

    writeln!(
        output,
        "| Offset | Size | Name | Access | Type | Description | Tags |"
    )
    .unwrap();
    writeln!(
        output,
        "|--------|------|------|--------|------|-------------|------|"
    )
    .unwrap();

    for reg in desc.registers.handles() {
        let reg_desc = &ctx.registers[reg];
        let name = ctx.identifiers[reg_desc.name].to_snake_case();
        let tags = &ctx.tags[reg_desc.tags];
        let tags = tags
            .iter()
            .map(|t| format!("`{t}`"))
            .collect::<Vec<_>>()
            .join(", ");

        writeln!(
            output,
            "| 0x{:X} | 0x{:X}({}) | `{}` | {:?} | {} | {} | {} |",
            reg_desc.address,
            reg_desc.size,
            reg_desc.size,
            name,
            reg_desc.access,
            generate_type_name(gen_ctx, ctx, reg_desc.type_ref, 0),
            reg_desc.description,
            tags
        )
        .unwrap();
    }

    output
}

fn instance_unnameds(
    ctx: &IrCtx,
    instances: &[(&String, &TreeElem, Handle<TreeElem>)],
) -> BTreeMap<String, usize> {
    let mut map = BTreeMap::new();

    for (device_name, elem, _) in instances {
        let unnameds: &mut usize = map.entry(device_name.to_string()).or_default();
        match ctx.paths[elem.path].last() {
            Some(PathComp::Named { loc: _, name: _ }) => {}
            Some(PathComp::Unnamed(_)) => *unnameds += 1,
            None => panic!(),
        }
    }

    map
}

fn instance_name(
    ctx: &IrCtx,
    instance_unnameds: &BTreeMap<String, usize>,
    device_name: &str,
    path: Handle<memmap_generate::ir::types::Path>,
) -> String {
    let path = &ctx.paths[path];

    match path.last() {
        Some(PathComp::Named { loc: _, name }) => name.clone(),
        Some(PathComp::Unnamed(n)) => {
            if instance_unnameds[device_name] > 1 {
                format!("{device_name}_{n}").to_snake_case()
            } else {
                device_name.to_snake_case()
            }
        }
        None => panic!("all paths should have at least one element"),
    }
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
        let src = std::fs::read_to_string(&path).unwrap();

        let desc = mm_inp::parse(&src).unwrap();

        let hal_name = name.to_str().unwrap();

        memory_maps.insert(hal_name.to_string(), desc);
    }

    memory_maps
}
