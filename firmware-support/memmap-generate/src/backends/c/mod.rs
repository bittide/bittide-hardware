// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use std::collections::BTreeSet;

use heck::{ToPascalCase, ToShoutySnakeCase, ToSnakeCase};
use std::fmt::Write;

use crate::{
    input_language::TypeName,
    ir::{
        monomorph::{MonomorphVariants, TypeRefVariant},
        types::{IrCtx, TypeRef},
    },
    storage::{Handle, HandleRange},
};

pub mod device_desc;
pub mod device_instances;
pub mod type_desc;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IdentType {
    Type,
    Device,
    Instance,
    Module,
    Method,
    Variable,
    Constant,
    Define,
    // Just pass through the raw string
    Raw,
}

/// Generate a contextual identifier from a string.
pub fn ident(ident_type: IdentType, n: impl AsRef<str>) -> String {
    let s = n.as_ref();
    let s = s.split(".").last().unwrap();
    match ident_type {
        IdentType::Type => s.to_pascal_case(),
        IdentType::Device => s.to_pascal_case(),
        IdentType::Instance => s.to_snake_case(),
        IdentType::Module => s.to_snake_case(),
        IdentType::Method => s.to_snake_case(),
        IdentType::Variable => s.to_snake_case(),
        IdentType::Constant => s.to_shouty_snake_case(),
        IdentType::Define => s.to_shouty_snake_case(),
        IdentType::Raw => s.to_string(),
    }
}

/// Types referenced during code generation.
#[derive(Default)]
pub struct TypeReferences {
    pub references: BTreeSet<Handle<TypeName>>,
    pub tuples: BTreeSet<HandleRange<TypeRef>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum VariableRefType {
    Const,
    ConstVec,
    ConstBitVector,
    Mutable,
    MutableVec,
    MutableBitVector,
    NoRef,
}

impl VariableRefType {
    fn as_set_arg(ctx: &IrCtx, variant: Option<&TypeRefVariant>, ty: &TypeRef) -> Self {
        match ty {
            TypeRef::BitVector(size) if bv_of_size_1(ctx, variant, *size) => Self::NoRef,
            TypeRef::BitVector(_size) => Self::ConstBitVector,
            TypeRef::Vector(_handle, _handle1) => Self::ConstVec,
            _ => Self::NoRef,
        }
    }

    fn as_get_out_arg(ctx: &IrCtx, variant: Option<&TypeRefVariant>, ty: &TypeRef) -> Self {
        match ty {
            TypeRef::BitVector(size) if bv_of_size_1(ctx, variant, *size) => Self::Mutable,
            TypeRef::BitVector(_size) => Self::MutableBitVector,
            TypeRef::Vector(_handle, _handle1) => Self::MutableVec,
            _ => Self::Mutable,
        }
    }
}

fn bv_of_size_1(ctx: &IrCtx, variant: Option<&TypeRefVariant>, size: Handle<TypeRef>) -> bool {
    let size = lookup_sub(variant, size);
    match &ctx.type_refs[size] {
        TypeRef::Nat(n) => n.div_ceil(8) == 1,
        _ => false,
    }
}

#[derive(Debug, Clone)]
enum VariableBindingContext<'a> {
    SetArg(&'a str),
    OutArg(&'a str),
    Member(&'a str),
    VarDeclStatement(&'a str),
    TypeOnly,
}

#[allow(clippy::too_many_arguments)]
fn generate_variable_binding(
    ctx: &IrCtx,
    varis: &MonomorphVariants,
    variant: Option<&TypeRefVariant>,
    refs: &mut TypeReferences,
    var_binding: VariableBindingContext,
    ty: Handle<TypeRef>,
    code: &mut String,
) {
    let handle = lookup_sub(variant, ty);
    let ty_ref = &ctx.type_refs[handle];

    let (variable_name, ref_ty) = match &var_binding {
        VariableBindingContext::SetArg(name) => {
            (*name, VariableRefType::as_set_arg(ctx, variant, ty_ref))
        }
        VariableBindingContext::OutArg(name) => {
            (*name, VariableRefType::as_get_out_arg(ctx, variant, ty_ref))
        }
        VariableBindingContext::Member(name) => (*name, VariableRefType::NoRef),
        VariableBindingContext::VarDeclStatement(name) => (*name, VariableRefType::NoRef),
        VariableBindingContext::TypeOnly => ("", VariableRefType::NoRef),
    };

    let before_var = match ref_ty {
        VariableRefType::Const => "const *",
        VariableRefType::ConstVec => "const ",
        VariableRefType::ConstBitVector => "const ",
        VariableRefType::Mutable => "*",
        VariableRefType::MutableVec => "",
        VariableRefType::MutableBitVector => "",
        VariableRefType::NoRef => "",
    };

    match &ctx.type_refs[handle] {
        TypeRef::BitVector(handle) => {
            let size = &ctx.type_refs[lookup_sub(variant, *handle)];

            if let TypeRef::Nat(n) = size {
                let n = n.div_ceil(8);
                if n == 1 {
                    write!(code, "uint8_t {before_var}{variable_name}").unwrap();
                } else {
                    write!(code, "uint8_t {before_var}{variable_name}[{n}]").unwrap();
                }
            } else {
                panic!("Unsigned/BitVector with length not known after monomorphisation");
            }
        }
        TypeRef::Unsigned(handle) => {
            let size = &ctx.type_refs[lookup_sub(variant, *handle)];

            if let TypeRef::Nat(n) = size {
                let n = po2_type(*n);
                write!(code, "uint{n}_t {before_var}{variable_name}").unwrap();
            } else {
                panic!("Unsigned/BitVector with length not known after monomorphisation");
            }
        }
        TypeRef::Signed(handle) => {
            let size = &ctx.type_refs[lookup_sub(variant, *handle)];

            if let TypeRef::Nat(n) = size {
                let n = po2_type(*n);
                write!(code, "int{n}_t {before_var}{variable_name}").unwrap();
            } else {
                panic!("Signed with length not known after monomorphisation");
            }
        }
        TypeRef::Index(handle) => {
            let size = &ctx.type_refs[lookup_sub(variant, *handle)];

            if let TypeRef::Nat(n) = size {
                let n = po2_type(n.ilog2() as u64);
                write!(code, "uint{n}_t {before_var}{variable_name}").unwrap()
            } else {
                panic!("Index with length not known after monomorphisation");
            }
        }
        TypeRef::Bool => {
            write!(code, "bool {before_var}{variable_name}").unwrap();
        }
        TypeRef::Float => {
            write!(code, "float {before_var}{variable_name}").unwrap();
        }
        TypeRef::Double => {
            write!(code, "double {before_var}{variable_name}").unwrap();
        }
        TypeRef::Vector(len, inner) => {
            generate_variable_binding(ctx, varis, variant, refs, var_binding.clone(), *inner, code);
            if let TypeRef::Nat(n) = &ctx.type_refs[lookup_sub(variant, *len)] {
                write!(code, "[{n}]").unwrap();
            } else {
                panic!("Vector with non-constant length known")
            }
        }
        TypeRef::Reference { name, args: _ } => {
            refs.references.insert(*name);
            let mono_ref = if let Some(var) = variant {
                var.monomorph_type_ref_substitutions.get(&handle)
            } else {
                None
            };
            let mono_ref = mono_ref.or_else(|| varis.type_refs.get(&handle));

            if let Some(mono_ref) = mono_ref.copied() {
                let mono_variant = &varis.type_ref_variants[mono_ref];
                let name = mono_variant_name(ctx, mono_variant);
                let any_args = mono_variant
                    .argument_mono_values
                    .iter()
                    .any(|s| s.is_none());
                if any_args {
                    panic!("all type arguments should have been removed during monomorphization")
                }
                write!(code, "{name} {before_var}{variable_name}").unwrap();
            } else {
                panic!("all types should have monomorph variants")
            }
        }
        TypeRef::Tuple(handle_range) => {
            refs.tuples.insert(*handle_range);
            write!(
                code,
                "{} {before_var}{variable_name}",
                tuple_name(ctx, varis, handle_range.handles())
            )
            .unwrap()
        }
        TypeRef::Variable(_) => {
            panic!("type level variable found, these are not supported in C")
        }
        TypeRef::Nat(_) => panic!("type level natural found, these are not supported in C"),
    }
}

fn lookup_sub(variant: Option<&TypeRefVariant>, handle: Handle<TypeRef>) -> Handle<TypeRef> {
    let mut handle = handle;
    let subs = if let Some(var) = variant {
        &var.variable_substitutions
    } else {
        return handle;
    };
    while let Some(sub) = subs.get(&handle).copied() {
        handle = sub;
    }
    handle
}
fn po2_type(n: u64) -> u64 {
    let n = n.max(8);
    if n.is_power_of_two() {
        n
    } else {
        n.next_power_of_two()
    }
}

fn tuple_name(
    ctx: &IrCtx,
    _varis: &MonomorphVariants,
    elems: impl ExactSizeIterator<Item = Handle<TypeRef>>,
) -> String {
    let n = elems.len();

    if n == 0 {
        return "unit".to_string();
    }

    let mut name = format!("tuple{n}");

    for elem in elems {
        name.push('_');
        name.push_str(&type_to_ident(ctx, elem));
    }
    name
}

fn mono_variant_name(ctx: &IrCtx, var: &TypeRefVariant) -> String {
    let desc = &ctx.type_descs[var.original_type_desc];
    let ty_name = &ctx.type_names[desc.name];
    let args = var
        .argument_mono_values
        .iter()
        .map(|arg| arg.map(|val| type_to_ident(ctx, val)));

    let has_mono_args = args.clone().any(|arg| arg.is_some());

    let mut arg_names = String::new();
    if has_mono_args {
        for arg in args {
            arg_names.push('_');
            if let Some(val) = arg {
                arg_names.push_str(&val);
            }
        }
    }

    let type_name_base = ident(IdentType::Type, &ty_name.base);
    ident(IdentType::Raw, format!("{type_name_base}{arg_names}"))
}

fn type_to_ident(ctx: &IrCtx, ty: Handle<TypeRef>) -> String {
    match &ctx.type_refs[ty] {
        TypeRef::BitVector(handle) => {
            let len = type_to_ident(ctx, *handle);
            format!("bv{len}")
        }
        TypeRef::Unsigned(handle) => {
            let len = type_to_ident(ctx, *handle);
            format!("u{len}")
        }
        TypeRef::Signed(handle) => {
            let len = type_to_ident(ctx, *handle);
            format!("s{len}")
        }
        TypeRef::Index(handle) => {
            let len = type_to_ident(ctx, *handle);
            format!("i{len}")
        }
        TypeRef::Bool => "bool".to_string(),
        TypeRef::Float => "f32".to_string(),
        TypeRef::Double => "f64".to_string(),
        TypeRef::Vector(size_handle, inner) => {
            let size = type_to_ident(ctx, *size_handle);
            let inner = type_to_ident(ctx, *inner);
            format!("vec_{size}_{inner}")
        }
        TypeRef::Tuple(handle_range) if handle_range.len == 0 => "unit".to_string(),
        TypeRef::Tuple(handle_range) => {
            let n = handle_range.len;
            let mut name = format!("tuple{n}");
            for handle in handle_range.handles() {
                name.push('_');
                name.push_str(&type_to_ident(ctx, handle));
            }
            name
        }
        TypeRef::Variable(handle) => {
            let name = &ctx.identifiers[*handle];
            format!("var_{name}")
        }
        TypeRef::Nat(n) => format!("{}", n),
        TypeRef::Reference { name, args } => {
            let type_name = &ctx.type_names[*name];
            let mut ident = type_name.base.clone();
            for arg in args.handles() {
                ident.push('_');
                ident.push_str(&type_to_ident(ctx, arg));
            }
            ident
        }
    }
}
