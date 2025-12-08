// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::collections::BTreeSet;
use std::fmt::Write;

use crate::backends::c::{generate_variable_binding, tuple_name};
use crate::ir::types::{TypeConstructor, TypeRef};
use crate::storage::HandleRange;
use crate::{
    backends::c::{ident, mono_variant_name, IdentType, TypeReferences},
    input_language::TypeName,
    ir::{
        monomorph::{MonomorphVariants, TypeRefVariant},
        types::{IrCtx, TypeDefinition, TypeDescription},
    },
    storage::Handle,
};

/// Generate code for a type description, taking into account monomorp variants.
///
/// Returns the base-name of the type, the code and any types referenced by the code.
pub fn generate_type_desc<'ir>(
    ctx: &'ir IrCtx,
    varis: &MonomorphVariants,
    handle: Handle<TypeDescription>,
) -> (&'ir str, String, TypeReferences) {
    let mut code = String::new();
    let mut refs = TypeReferences {
        references: BTreeSet::new(),
        tuples: BTreeSet::new(),
    };
    let desc = &ctx.type_descs[handle];
    let type_name = &ctx.type_names[desc.name];
    generate_tag_type(ctx, type_name, desc, &mut code);
    if let Some(variants) = varis.variants_by_type.get(&desc.name) {
        for var_handle in variants.iter() {
            generate_type_definition(ctx, varis, *var_handle, &mut refs, desc, &mut code);
        }
    } else {
        panic!("all types must have been monomorphed")
    }

    (&type_name.base, code, refs)
}

pub fn generate_tuple_definition(
    ctx: &IrCtx,
    varis: &MonomorphVariants,
    elements: &[Handle<TypeRef>],
    refs: &mut TypeReferences,
    code: &mut String,
) {
    let name = tuple_name(ctx, varis, elements.iter().copied());

    writeln!(code, "typedef struct {name} {{").unwrap();

    for (i, elem) in elements.iter().enumerate() {
        write!(code, "  ").unwrap();
        generate_variable_binding(
            ctx,
            varis,
            None,
            refs,
            crate::backends::c::VariableBindingContext::Member(&format!("_{i}")),
            *elem,
            code,
        );
        writeln!(code, ";").unwrap();
    }

    writeln!(code, "}} {name};").unwrap();
}

fn generate_tag_type(ctx: &IrCtx, name: &TypeName, desc: &TypeDescription, code: &mut String) {
    match &desc.definition {
        TypeDefinition::DataType {
            names: _,
            constructors,
        } if is_fieldless(ctx, *constructors) => {}
        TypeDefinition::DataType {
            names: _,
            constructors,
        } if constructors.len < 2 => {}
        TypeDefinition::DataType {
            names,
            constructors: _,
        } => {
            let tag_type_name = format!("{}_Tag", ident(IdentType::Type, &name.base));
            writeln!(code, "typedef enum {tag_type_name} {{").unwrap();

            for var_name in &ctx.identifiers[*names] {
                let variant_name = ident(
                    IdentType::Constant,
                    format!("{}_TAG_{}", &name.base, var_name),
                );
                writeln!(code, "  {variant_name},").unwrap();
            }

            writeln!(code, "}} __attribute__((__packed__)) {tag_type_name};").unwrap();
            writeln!(code).unwrap();
        }
        TypeDefinition::Newtype {
            name: _,
            constructor: _,
        } => {}
        TypeDefinition::Builtin(_builtin_type) => {}
        TypeDefinition::Synonym(_handle) => {}
    }
}

fn generate_type_definition(
    ctx: &IrCtx,
    varis: &MonomorphVariants,
    var_handle: Handle<TypeRefVariant>,
    refs: &mut TypeReferences,
    desc: &TypeDescription,
    code: &mut String,
) {
    let variant = &varis.type_ref_variants[var_handle];
    let raw_name = &ctx.type_names[desc.name];
    let ty_name = mono_variant_name(ctx, variant);

    match &desc.definition {
        TypeDefinition::DataType {
            names: _,
            constructors,
        } if constructors.len == 0 => {
            writeln!(code, "typedef void {ty_name};").unwrap();
        }
        TypeDefinition::DataType {
            names: _,
            constructors,
        } if constructors.len == 1 && is_fieldless(ctx, *constructors) => {
            writeln!(code, "typedef void {ty_name};").unwrap();
        }
        TypeDefinition::DataType {
            names,
            constructors,
        } if constructors.len == 1 => {
            let con = &ctx.type_constructors[constructors.start];
            let con_name = &ctx.identifiers[names.start];

            if con_name != &raw_name.base {
                // panic!(
                //     "Single-constructor data types are required to have the constructor name match the type name. \
                //     Type name: '{type_name:?}', Constructor name: '{constructor_name:?}'",
                //     type_name = raw_name.base,
                //     constructor_name = con_name,
                // )
            }

            writeln!(code, "typedef struct {ty_name} {{").unwrap();

            let names = if let Some(handles) = con.field_names {
                ctx.identifiers[handles]
                    .iter()
                    .map(|name| ident(IdentType::Variable, name))
                    .collect::<Vec<_>>()
            } else {
                (0..con.field_types.len)
                    .map(|i| format!("field_{i}"))
                    .collect()
            };
            for (name, ty) in names.into_iter().zip(con.field_types.handles()) {
                let is_zst = false;
                write!(code, "  ").unwrap();

                if is_zst {
                    write!(code, "// ").unwrap();
                }
                generate_variable_binding(
                    ctx,
                    varis,
                    Some(variant),
                    refs,
                    crate::backends::c::VariableBindingContext::Member(name.as_str()),
                    ty,
                    code,
                );
                writeln!(code, ";").unwrap();
            }

            writeln!(code, "}} {ty_name};").unwrap();
        }
        TypeDefinition::DataType {
            names,
            constructors,
        } if is_fieldless(ctx, *constructors) => {
            writeln!(code, "typedef enum {ty_name} {{").unwrap();

            for name in &ctx.identifiers[*names] {
                let variant_name = ident(IdentType::Constant, format!("{ty_name}_{name}"));
                writeln!(code, "  {variant_name},").unwrap();
            }

            writeln!(code, "}} __attribute__((__packed__)) {ty_name};").unwrap();
        }
        TypeDefinition::DataType {
            names,
            constructors,
        } => {
            let tag_type_name = format!("{}_Tag", ident(IdentType::Type, &raw_name.base));
            writeln!(code, "typedef struct {ty_name} {{").unwrap();
            writeln!(code, "  {tag_type_name} tag;").unwrap();
            writeln!(code, "  union {{").unwrap();

            for (name, con) in names.handles().zip(constructors.handles()) {
                let con = &ctx.type_constructors[con];
                if con.field_types.len == 0 {
                    continue;
                }
                let payload_name = ident(IdentType::Variable, &ctx.identifiers[name]);
                writeln!(code, "    struct {{").unwrap();

                if let Some(names) = con.field_names {
                    for (name, ty) in names.handles().zip(con.field_types.handles()) {
                        let name = ident(IdentType::Variable, &ctx.identifiers[name]);
                        write!(code, "      ").unwrap();
                        generate_variable_binding(
                            ctx,
                            varis,
                            Some(variant),
                            refs,
                            crate::backends::c::VariableBindingContext::Member(name.as_str()),
                            ty,
                            code,
                        );
                        writeln!(code, ";").unwrap();
                    }
                } else {
                    for (i, ty) in con.field_types.handles().enumerate() {
                        write!(code, "      ").unwrap();
                        let name = format!("_{i}");
                        generate_variable_binding(
                            ctx,
                            varis,
                            Some(variant),
                            refs,
                            crate::backends::c::VariableBindingContext::Member(name.as_str()),
                            ty,
                            code,
                        );
                        writeln!(code, ";").unwrap();
                    }
                }

                writeln!(code, "    }} {payload_name};").unwrap();
            }
            writeln!(code, "  }};").unwrap();

            writeln!(code, "}} {ty_name};").unwrap();
        }
        TypeDefinition::Newtype {
            name: _,
            constructor: _,
        } => {
            panic!()
        }
        TypeDefinition::Builtin(_builtin_type) => {
            panic!()
        }
        TypeDefinition::Synonym(handle) => {
            write!(code, "typedef ").unwrap();
            generate_variable_binding(
                ctx,
                varis,
                Some(variant),
                refs,
                crate::backends::c::VariableBindingContext::VarDeclStatement(ty_name.as_str()),
                *handle,
                code,
            );
            writeln!(code, ";").unwrap();
        }
    }
    writeln!(code).unwrap();
}

fn is_fieldless(ctx: &IrCtx, cons: HandleRange<TypeConstructor>) -> bool {
    cons.handles()
        .all(|con| ctx.type_constructors[con].field_types.len == 0)
}
