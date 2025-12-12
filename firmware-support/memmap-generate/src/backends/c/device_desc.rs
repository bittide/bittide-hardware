// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use std::collections::BTreeSet;

use std::fmt::Write;

use crate::{
    backends::c::{
        bv_of_size_1, generate_variable_binding, ident, lookup_sub, IdentType, TypeReferences,
        VariableBindingContext,
    },
    input_language::RegisterAccess,
    ir::{
        monomorph::{MonomorphVariants, TypeRefVariant},
        types::{DeviceDescription, IrCtx, RegisterDescription, TypeRef},
    },
    storage::Handle,
};

pub fn generate_device_desc<'ir>(
    ctx: &'ir IrCtx,
    varis: &MonomorphVariants,
    handle: Handle<DeviceDescription>,
) -> (&'ir str, String, TypeReferences) {
    // should we generate header and source separately?
    let mut refs = TypeReferences {
        references: BTreeSet::new(),
        tuples: BTreeSet::new(),
    };
    let desc = &ctx.device_descs[handle];
    let name = &ctx.identifiers[desc.name];

    let name_ident = ident(IdentType::Device, name);

    let mut code = format!(
        r#"
typedef struct {name_ident} {{
    volatile uint8_t *base;
}} {name_ident};

"#
    );

    let mut consts = desc
        .registers
        .handles()
        .filter_map(|reg| generate_const(ctx, &name_ident, reg))
        .peekable();
    if consts.peek().is_some() {
        writeln!(code, "enum {{").unwrap();

        for (name, val) in consts {
            writeln!(code, "  {name} = {val},").unwrap();
        }

        writeln!(code, "}};").unwrap();
        writeln!(code).unwrap();
    }

    for reg in desc.registers.handles() {
        generate_reg_get_func(ctx, varis, &mut refs, &mut code, &name_ident, reg);
        generate_reg_set_func(ctx, varis, &mut refs, &mut code, &name_ident, reg);
    }

    (name, code, refs)
}

fn generate_const(
    ctx: &IrCtx,
    dev_name: &str,
    reg: Handle<RegisterDescription>,
) -> Option<(String, u64)> {
    let desc = &ctx.registers[reg];
    let ty = &ctx.type_refs[desc.type_ref];

    let variables = match ty {
        TypeRef::BitVector(handle) | TypeRef::Unsigned(handle) | TypeRef::Signed(handle) => {
            let TypeRef::Nat(width) = &ctx.type_refs[*handle] else {
                panic!("BitVector/Unsigned/Signed need to have constant widths")
            };
            Some(("WIDTH", *width))
        }
        TypeRef::Index(handle) => {
            let TypeRef::Nat(size) = &ctx.type_refs[*handle] else {
                panic!("Index needs to have constant size")
            };
            Some(("SIZE", *size))
        }
        TypeRef::Vector(len, _) => {
            let TypeRef::Nat(length) = &ctx.type_refs[*len] else {
                panic!("Vector needs to have constant length")
            };
            Some(("LEN", *length))
        }
        _ => None,
    };
    let (suffix, value) = variables?;
    let desc_name = &ctx.identifiers[desc.name];
    let const_name = ident(
        IdentType::Constant,
        format!("{dev_name}_{desc_name}_{suffix}"),
    );
    Some((const_name, value))
}

fn generate_reg_get_func(
    ctx: &IrCtx,
    varis: &MonomorphVariants,
    refs: &mut TypeReferences,
    code: &mut String,
    dev_name: &str,
    handle: Handle<RegisterDescription>,
) {
    let reg = &ctx.registers[handle];
    if !matches!(
        reg.access,
        RegisterAccess::ReadOnly | RegisterAccess::ReadWrite
    ) {
        return;
    }

    let desc = &reg.description;
    let raw_ty = &ctx.type_refs[reg.type_ref];

    if !desc.is_empty() {
        writeln!(code, "/// {desc}").unwrap();
    }

    if reg.tags.len > 0 {
        if !desc.is_empty() {
            writeln!(code, "///").unwrap();
        }
        writeln!(code, "/// Tags").unwrap();
        writeln!(code, "///").unwrap();
        for tag in &ctx.tags[reg.tags] {
            writeln!(code, "/// - `{tag}`").unwrap();
        }
    }

    if matches!(raw_ty, TypeRef::BitVector(size) if !bv_of_size_1(ctx, None, *size)) {
        let func_name = ident(
            IdentType::Method,
            format!("{dev_name}_get_{}", ctx.identifiers[reg.name]),
        );
        let mut out_arg = String::new();
        generate_variable_binding(
            ctx,
            varis,
            None,
            refs,
            VariableBindingContext::OutArg("out"),
            reg.type_ref,
            &mut out_arg,
        );

        writeln!(
            code,
            "static inline void {func_name}({dev_name} dev, {out_arg}) {{"
        )
        .unwrap();

        let base_addr = format!("(dev.base + {})", reg.address);

        write_type_to_addr(
            ctx,
            varis,
            None,
            refs,
            (&base_addr, ValueType::VolatilePointer),
            ("out", ValueType::Pointer),
            reg.type_ref,
            "    ",
            code,
        );

        writeln!(code, "}}").unwrap();
    } else if let TypeRef::Vector(len, inner) = raw_ty {
        let mut out_arg = String::new();
        generate_variable_binding(
            ctx,
            varis,
            None,
            refs,
            VariableBindingContext::OutArg("out"),
            *inner,
            &mut out_arg,
        );
        let mut ty_ref = String::new();
        generate_variable_binding(
            ctx,
            varis,
            None,
            refs,
            VariableBindingContext::TypeOnly,
            *inner,
            &mut ty_ref,
        );
        let ty_ref = ty_ref.trim();
        let func_name = ident(
            IdentType::Method,
            format!("{dev_name}_get_{}", ctx.identifiers[reg.name]),
        );
        writeln!(
            code,
            "static inline bool {func_name}({dev_name} dev, size_t idx, {out_arg}) {{"
        )
        .unwrap();

        let TypeRef::Nat(n) = &ctx.type_refs[*len] else {
            panic!("register with vec-type must have constant length")
        };

        writeln!(code, "  if (idx >= {n}) {{").unwrap();
        writeln!(code, "    return false;").unwrap();
        writeln!(code, "  }} else {{").unwrap();
        let base_addr = format!("((dev.base + {}) + (idx * sizeof({ty_ref})))", reg.address);
        write_type_to_addr(
            ctx,
            varis,
            None,
            refs,
            (&base_addr, ValueType::VolatilePointer),
            ("out", ValueType::Pointer),
            *inner,
            "    ",
            code,
        );
        writeln!(code, "    return true;").unwrap();
        writeln!(code, "  }}").unwrap();

        writeln!(code, "}}").unwrap();

        writeln!(code).unwrap();

        writeln!(
            code,
            "static inline void {func_name}_unchecked({dev_name} dev, size_t idx, {out_arg}) {{"
        )
        .unwrap();
        write_type_to_addr(
            ctx,
            varis,
            None,
            refs,
            (&base_addr, ValueType::VolatilePointer),
            ("out", ValueType::Pointer),
            *inner,
            "    ",
            code,
        );
        writeln!(code, "}}").unwrap();
    } else if ctx.tags[reg.tags].iter().any(|tag| tag == "zero-width") {
        let func_name = ident(
            IdentType::Method,
            format!("{dev_name}_get_{}", ctx.identifiers[reg.name]),
        );
        writeln!(code, "static inline void {func_name}({dev_name} dev) {{").unwrap();
        writeln!(
            code,
            "  uint8_t scratch = *(volatile uint8_t*)(dev.base + {});",
            reg.address
        )
        .unwrap();
        writeln!(code, "  (void)scratch;").unwrap();

        writeln!(code, "}}").unwrap();
    } else {
        let func_name = ident(
            IdentType::Method,
            format!("{dev_name}_get_{}", ctx.identifiers[reg.name]),
        );
        let mut ret_ty = String::new();
        generate_variable_binding(
            ctx,
            varis,
            None,
            refs,
            VariableBindingContext::TypeOnly,
            reg.type_ref,
            &mut ret_ty,
        );
        writeln!(code, "static inline {ret_ty}{func_name}({dev_name} dev) {{").unwrap();
        writeln!(
            code,
            "  return *(volatile {ret_ty}*)(dev.base + {});",
            reg.address
        )
        .unwrap();

        writeln!(code, "}}").unwrap();
    }
    writeln!(code).unwrap();
}

#[allow(clippy::too_many_arguments)]
fn generate_reg_set_func(
    ctx: &IrCtx,
    varis: &MonomorphVariants,
    refs: &mut TypeReferences,
    code: &mut String,
    dev_name: &str,
    handle: Handle<RegisterDescription>,
) {
    let reg = &ctx.registers[handle];
    if !matches!(
        reg.access,
        RegisterAccess::WriteOnly | RegisterAccess::ReadWrite
    ) {
        return;
    }

    let desc = &reg.description;
    let raw_ty = &ctx.type_refs[reg.type_ref];

    if !desc.is_empty() {
        writeln!(code, "/// {desc}").unwrap();
    }

    if reg.tags.len > 0 {
        if !desc.is_empty() {
            writeln!(code, "///").unwrap();
        }
        writeln!(code, "/// Tags").unwrap();
        writeln!(code, "///").unwrap();
        for tag in &ctx.tags[reg.tags] {
            writeln!(code, "/// - `{tag}`").unwrap();
        }
    }

    if matches!(raw_ty, TypeRef::BitVector(size) if !bv_of_size_1(ctx, None, *size)) {
        let mut val_arg = String::new();
        generate_variable_binding(
            ctx,
            varis,
            None,
            refs,
            VariableBindingContext::SetArg("val"),
            reg.type_ref,
            &mut val_arg,
        );
        let func_name = ident(
            IdentType::Method,
            format!("{dev_name}_set_{}", ctx.identifiers[reg.name]),
        );
        writeln!(
            code,
            "static inline void {func_name}({dev_name} dev, {val_arg}) {{"
        )
        .unwrap();

        let dest_addr = format!("dev.base + {}", reg.address);

        write_type_to_addr(
            ctx,
            varis,
            None,
            refs,
            ("val", ValueType::Pointer),
            (&dest_addr, ValueType::VolatilePointer),
            reg.type_ref,
            "    ",
            code,
        );

        writeln!(code, "}}").unwrap();
    } else if let TypeRef::Vector(len, inner) = raw_ty {
        let mut val_arg = String::new();
        generate_variable_binding(
            ctx,
            varis,
            None,
            refs,
            VariableBindingContext::SetArg("val"),
            *inner,
            &mut val_arg,
        );
        let mut ty_ref = String::new();
        generate_variable_binding(
            ctx,
            varis,
            None,
            refs,
            VariableBindingContext::TypeOnly,
            *inner,
            &mut ty_ref,
        );
        let ty_ref = ty_ref.trim();
        let func_name = ident(
            IdentType::Method,
            format!("{dev_name}_set_{}", ctx.identifiers[reg.name]),
        );
        writeln!(
            code,
            "static inline bool {func_name}({dev_name} dev, size_t idx, {val_arg}) {{"
        )
        .unwrap();

        let TypeRef::Nat(n) = &ctx.type_refs[*len] else {
            panic!("register with vec-type must have constant length")
        };

        writeln!(code, "  if (idx >= {n}) {{").unwrap();
        writeln!(code, "    return false;").unwrap();
        writeln!(code, "  }} else {{").unwrap();
        let dest_addr = format!("((dev.base + {}) + (idx * sizeof({ty_ref})))", reg.address);
        write_type_to_addr(
            ctx,
            varis,
            None,
            refs,
            ("val", ValueType::Value),
            (&dest_addr, ValueType::VolatilePointer),
            *inner,
            "    ",
            code,
        );
        writeln!(code, "    return true;").unwrap();
        writeln!(code, "  }}").unwrap();

        writeln!(code, "}}").unwrap();

        writeln!(code).unwrap();

        writeln!(
            code,
            "static inline void {func_name}_unchecked({dev_name} dev, size_t idx, {val_arg}) {{"
        )
        .unwrap();
        write_type_to_addr(
            ctx,
            varis,
            None,
            refs,
            ("val", ValueType::Value),
            (&dest_addr, ValueType::VolatilePointer),
            *inner,
            "    ",
            code,
        );
        writeln!(code, "}}").unwrap();
    } else if ctx.tags[reg.tags].iter().any(|tag| tag == "zero-width") {
        let func_name = ident(
            IdentType::Method,
            format!("{dev_name}_set_{}", ctx.identifiers[reg.name]),
        );
        writeln!(code, "static inline void {func_name}({dev_name} dev) {{").unwrap();
        writeln!(
            code,
            "  *(volatile uint8_t*)(dev.base + {}) = 0;",
            reg.address
        )
        .unwrap();

        writeln!(code, "}}").unwrap();
    } else {
        let func_name = ident(
            IdentType::Method,
            format!("{dev_name}_set_{}", ctx.identifiers[reg.name]),
        );
        let mut ret_ty = String::new();
        generate_variable_binding(
            ctx,
            varis,
            None,
            refs,
            VariableBindingContext::TypeOnly,
            reg.type_ref,
            &mut ret_ty,
        );
        let mut val_arg = String::new();
        generate_variable_binding(
            ctx,
            varis,
            None,
            refs,
            VariableBindingContext::SetArg("val"),
            reg.type_ref,
            &mut val_arg,
        );
        writeln!(
            code,
            "static inline void {func_name}({dev_name} dev, {val_arg}) {{"
        )
        .unwrap();
        writeln!(
            code,
            "  *(volatile {ret_ty}*)(dev.base + {}) = val;",
            reg.address
        )
        .unwrap();

        writeln!(code, "}}").unwrap();
    }
    writeln!(code).unwrap();
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum ValueType {
    Pointer,
    VolatilePointer,
    Value,
}

#[allow(clippy::too_many_arguments)]
fn write_type_to_addr(
    ctx: &IrCtx,
    varis: &MonomorphVariants,
    variant: Option<&TypeRefVariant>,
    refs: &mut TypeReferences,
    (base_addr, base_ty): (&str, ValueType),
    (destination_var, dest_ty): (&str, ValueType),
    ty_handle: Handle<TypeRef>,
    indent: &str,
    code: &mut String,
) {
    let ty_handle = lookup_sub(variant, ty_handle);
    let ty = &ctx.type_refs[ty_handle];

    match ty {
        TypeRef::BitVector(size) if !bv_of_size_1(ctx, variant, *size) => {
            let mut ty_ref = String::new();
            generate_variable_binding(
                ctx,
                varis,
                None,
                refs,
                VariableBindingContext::TypeOnly,
                ty_handle,
                &mut ty_ref,
            );
            let ty_ref = ty_ref.trim();
            writeln!(
                code,
                "{indent}memcpy_volatile({destination_var}, {base_addr}, sizeof({ty_ref}));"
            )
            .unwrap();
        }
        TypeRef::Vector(len, inner) => {
            let TypeRef::Nat(_n) = &ctx.type_refs[*len] else {
                panic!("register with vec-type must have constant length")
            };
            let mut ty_ref = String::new();
            generate_variable_binding(
                ctx,
                varis,
                None,
                refs,
                VariableBindingContext::TypeOnly,
                *inner,
                &mut ty_ref,
            );

            writeln!(
                code,
                "{indent}memcpy_volatile({destination_var}, {base_addr}, sizeof({ty_ref}));"
            )
            .unwrap();
        }
        _ => {
            let mut ty_ref = String::new();
            generate_variable_binding(
                ctx,
                varis,
                None,
                refs,
                VariableBindingContext::TypeOnly,
                ty_handle,
                &mut ty_ref,
            );
            let ty_ref = ty_ref.trim();
            let base_deref = match base_ty {
                ValueType::Pointer => "*",
                ValueType::VolatilePointer => &format!("*(volatile {ty_ref}*)"),
                ValueType::Value => "",
            };
            let dest_deref = match dest_ty {
                ValueType::Pointer => "*",
                ValueType::VolatilePointer => &format!("*(volatile {ty_ref}*)"),
                ValueType::Value => "",
            };
            writeln!(
                code,
                "{indent}{dest_deref}{destination_var} = {base_deref}{base_addr};",
            )
            .unwrap();
        }
    }
}
