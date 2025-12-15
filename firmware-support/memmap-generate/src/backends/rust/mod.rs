// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    str::FromStr,
};

use heck::{ToPascalCase, ToShoutySnakeCase, ToSnakeCase};
use proc_macro2::{Ident, Literal, Span, TokenStream};
use quote::{quote, ToTokens};

use crate::{
    backends::all_instance_names,
    input_language::{RegisterAccess, TypeName},
    ir::{
        deduplicate::HalShared,
        monomorph::{MonomorphVariants, TypeRefVariant},
        types::{
            DeviceDescription, IrCtx, RegisterDescription, TreeElem, TypeDefinition,
            TypeDescription, TypeRef,
        },
    },
    storage::Handle,
};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IdentType {
    Type,
    TypeVariable,
    Device,
    Instance,
    Module,
    Method,
    Variable,
    Constant,
    // Just pass through the raw string
    Raw,
}

/// Generate a contextual identifier from a string.
pub fn ident(ident_type: IdentType, n: impl AsRef<str>) -> Ident {
    let s = n.as_ref();
    let s = s.split(".").last().unwrap();
    let s = match ident_type {
        IdentType::Type => s.to_pascal_case(),
        IdentType::TypeVariable => s.to_shouty_snake_case(),
        IdentType::Device => s.to_pascal_case(),
        IdentType::Instance => s.to_snake_case(),
        IdentType::Module => s.to_snake_case(),
        IdentType::Method => s.to_snake_case(),
        IdentType::Variable => s.to_snake_case(),
        IdentType::Constant => s.to_shouty_snake_case(),
        IdentType::Raw => s.to_string(),
    };

    Ident::new(&s, Span::call_site())
}

/// User provided annotations. Will be considered during code generation.
#[derive(Default)]
pub struct Annotations {
    pub type_annotation: HashMap<Handle<TypeRefVariant>, BTreeSet<String>>,
    pub type_imports: HashMap<Handle<TypeRefVariant>, BTreeSet<String>>,
    pub type_tags: HashMap<Handle<TypeRefVariant>, BTreeSet<String>>,
}

/// Generate code for a type description, taking into account monomorp variants.
///
/// Returns the base-name of the type, the code and any types referenced by the code.
pub fn generate_type_desc<'ir>(
    ctx: &'ir IrCtx,
    varis: &MonomorphVariants,
    anns: &Annotations,
    handle: Handle<TypeDescription>,
) -> (&'ir str, proc_macro2::TokenStream, TypeReferences) {
    let mut refs = TypeReferences {
        references: BTreeSet::new(),
    };
    let desc = &ctx.type_descs[handle];
    let type_name = &ctx.type_names[desc.name];
    let variants = if let Some(variants) = varis.variants_by_type.get(&desc.name) {
        let mut all_imports = BTreeSet::new();

        let variant_toks = variants.iter().map(|var_handle| {
            generate_type_definition(ctx, varis, *var_handle, anns, &mut refs, desc)
        });

        for var in variants {
            if let Some(imports) = anns.type_imports.get(var) {
                all_imports.extend(imports)
            }
        }

        let imports = all_imports
            .into_iter()
            .map(|s| TokenStream::from_str(s).expect("invalid TokenStream for import"))
            .map(|import| quote! { use #import; });

        quote! {
            #(#imports)*
            #(#variant_toks)*
        }
    } else {
        quote! {
            compile_error!("Type `#type_name` has no monomorph variant");
        }
    };

    (&type_name.base, variants, refs)
}

/// Generate code for device instances in a HAL.
pub fn generate_device_instances(
    ctx: &IrCtx,
    shared: &HalShared,
    hal_name: &str,
    tree_elems: impl Iterator<Item = Handle<TreeElem>> + Clone,
) -> TokenStream {
    let hal_ident = ident(IdentType::Module, hal_name);
    let (name_mapping, name_counts) = all_instance_names(ctx, tree_elems.clone());
    let mut names_used = BTreeMap::new();

    let (imports, fields) = tree_elems
        .filter_map(|handle| match ctx.tree_elem_types[handle.cast()] {
            crate::ir::types::TreeElemType::DeviceInstance { device_name } => {
                let elem = &ctx.tree_elems[handle];
                let addr = elem.absolute_addr;
                if ctx.tags[elem.tags].iter().any(|tag| tag == "no-generate") {
                    return None;
                }
                Some((handle, device_name, addr))
            }
            crate::ir::types::TreeElemType::Interconnect { .. } => None,
        })
        .map(move |(handle, dev_name, abs_addr)| {
            let name = &ctx.identifiers[dev_name];
            let dev_ident = ident(IdentType::Device, name);

            let instance_name_ident = {
                let raw_name = name_mapping[&handle];
                if name_counts[raw_name] > 1 {
                    // if there's more than one instance with the same name,
                    // generate suffixes, `_0`, `_1` etc
                    let count = names_used
                        .entry(raw_name)
                        .and_modify(|n| *n += 1)
                        .or_insert(0);
                    ident(IdentType::Instance, format!("{raw_name}_{count}"))
                } else {
                    ident(IdentType::Instance, raw_name)
                }
            };

            use std::str::FromStr;
            let addr = Literal::from_str(&format!("0x{:X}", abs_addr)).unwrap();

            let field_def = quote! { pub #instance_name_ident: #dev_ident };
            let field_init =
                quote! { #instance_name_ident: unsafe { #dev_ident::new(#addr as *mut u8) } };
            let import = if shared.deduped_devices.iter().any(|dev| {
                let device_desc = &ctx.device_descs[*dev];
                let found = &ctx.identifiers[device_desc.name];
                found == name
            }) {
                (
                    dev_ident.to_string(),
                    quote! { use crate::shared_devices::#dev_ident; },
                )
            } else {
                (
                    dev_ident.to_string(),
                    quote! { use crate::hals::#hal_ident::devices::#dev_ident; },
                )
            };
            (import, (field_def, field_init))
        })
        .unzip::<_, _, BTreeMap<_, _>, Vec<_>>();
    let (field_defs, field_inits) = fields.into_iter().unzip::<_, _, Vec<_>, Vec<_>>();

    let imports = imports.values();
    quote! {

        #(#imports)*
        pub struct DeviceInstances {
            #(#field_defs,)*
        }

        impl DeviceInstances {
            pub const unsafe fn new() -> Self {
                DeviceInstances {
                    #(#field_inits,)*
                }
            }
        }
    }
}

fn generate_type_definition(
    ctx: &IrCtx,
    varis: &MonomorphVariants,
    var_handle: Handle<TypeRefVariant>,
    anns: &Annotations,
    refs: &mut TypeReferences,
    desc: &TypeDescription,
) -> TokenStream {
    let variant = &varis.type_ref_variants[var_handle];
    let raw_name = &ctx.type_names[desc.name];
    let (ty_name, has_mono_args) = mono_variant_name(ctx, variant);

    let args = {
        let arg_decls = desc
            .param_names
            .handles()
            .zip(&variant.argument_mono_values)
            .filter_map(|(handle, val)| {
                if val.is_none() {
                    let name = ident(IdentType::TypeVariable, &ctx.identifiers[handle]);
                    if ctx.type_param_nats.contains(&handle) {
                        Some(quote! { const #name: u128 })
                    } else {
                        Some(quote! { #name })
                    }
                } else {
                    None
                }
            });
        if variant.argument_mono_values.iter().any(|val| val.is_none()) {
            quote! { < #(#arg_decls,)* > }
        } else {
            quote! {}
        }
    };

    let attrs = {
        let repr = generate_repr(ctx, desc);
        let naming_attr = if has_mono_args {
            quote! { #[allow(non_camel_case_types)]}
        } else {
            TokenStream::new()
        };
        let annots = if let Some(code) = anns.type_annotation.get(&var_handle) {
            code
        } else {
            &BTreeSet::new()
        };
        let annots = annots
            .iter()
            .map(|s| TokenStream::from_str(s).expect("invalid TokenStream for type annotations"))
            .map(|toks| quote! { #toks });
        quote! {
            #naming_attr
            #repr
            #(#annots)*
        }
    };

    match &desc.definition {
        TypeDefinition::DataType {
            names: _,
            constructors,
        } if constructors.len == 0 => {
            quote! {
                #attrs
                pub struct #ty_name;
            }
        }
        TypeDefinition::DataType {
            names,
            constructors,
        } if constructors.len == 1 => {
            let con = &ctx.type_constructors[constructors.start];
            let con_name = &ctx.identifiers[names.start];

            if con_name != &raw_name.base {
                panic!(
                    "Single-constructor data types are required to have the constructor name match the type name. \
                    Type name: '{type_name:?}', Constructor name: '{constructor_name:?}'",
                    type_name = raw_name.base,
                    constructor_name = con_name,
                )
            }

            if let Some(names) = con.field_names {
                let fields_toks =
                    names
                        .handles()
                        .zip(con.field_types.handles())
                        .map(|(name, ty)| {
                            let id = ident(IdentType::Variable, &ctx.identifiers[name]);
                            let ty = generate_type_ref(ctx, varis, Some(variant), refs, ty);
                            quote! { pub #id: #ty }
                        });
                quote! {
                    #attrs
                    pub struct #ty_name #args {
                        #(#fields_toks,)*
                    }
                }
            } else {
                let fields_toks = con
                    .field_types
                    .handles()
                    .map(|f| generate_type_ref(ctx, varis, Some(variant), refs, f));
                let body = if con.field_types.len == 0 {
                    quote! {}
                } else {
                    quote! { ( #(pub #fields_toks,)* ) }
                };
                quote! {
                    #attrs
                    pub struct #ty_name #args #body;
                }
            }
        }
        TypeDefinition::DataType {
            names,
            constructors,
        } => {
            let cons_toks =
                names
                    .handles()
                    .zip(constructors.handles())
                    .map(|(name_handle, con_handle)| {
                        let con_name = &ctx.identifiers[name_handle];
                        let con = &ctx.type_constructors[con_handle];
                        let id = ident(IdentType::Type, con_name);

                        if let Some(field_names) = con.field_names {
                            let fields_toks = field_names
                                .handles()
                                .zip(con.field_types.handles())
                                .map(|(name, ty)| {
                                    let name = ident(IdentType::Variable, &ctx.identifiers[name]);
                                    let ty = generate_type_ref(ctx, varis, Some(variant), refs, ty);
                                    quote! { #name: #ty }
                                });
                            quote! {
                                #id {
                                    #(#fields_toks,)*
                                }
                            }
                        } else {
                            let fields_toks = con
                                .field_types
                                .handles()
                                .map(|ty| generate_type_ref(ctx, varis, Some(variant), refs, ty));
                            if con.field_types.len == 0 {
                                quote! { #id }
                            } else {
                                quote! { #id( #(#fields_toks,)* ) }
                            }
                        }
                    });
            quote! {
                #attrs
                pub enum #ty_name #args {
                    #(#cons_toks,)*
                }
            }
        }
        TypeDefinition::Newtype {
            name: _,
            constructor: _,
        } => {
            quote! { compile_error!("TODO"); }
        }
        TypeDefinition::Builtin(_builtin_type) => {
            quote! { compile_error!("TODO"); }
        }
        TypeDefinition::Synonym(handle) => {
            let ty = generate_type_ref(ctx, varis, Some(variant), refs, *handle);

            quote! { #attrs pub type #ty_name #args = #ty; }
        }
    }
}

fn generate_repr(ctx: &IrCtx, desc: &TypeDescription) -> TokenStream {
    match &desc.definition {
        TypeDefinition::DataType {
            names: _,
            constructors,
        } if constructors.len == 0 => quote! {},
        TypeDefinition::DataType {
            names: _,
            constructors,
        } if constructors.len == 1 => quote! { #[repr(C)] },
        TypeDefinition::DataType {
            names: _,
            constructors,
        } => {
            let fieldless = constructors
                .handles()
                .map(|handle| &ctx.type_constructors[handle])
                .all(|con| con.field_types.len == 0);
            let n = po2_type(constructors.len.ilog2() as u64);
            let repr = ident(IdentType::Raw, format!("u{}", n));

            if fieldless {
                quote! { #[repr(#repr) ]}
            } else {
                quote! { #[repr(C, #repr) ]}
            }
        }
        TypeDefinition::Newtype {
            name: _,
            constructor: _,
        } => {
            quote! { #[repr(transparent)] }
        }
        TypeDefinition::Builtin(_builtin_type) => quote! {},
        TypeDefinition::Synonym(_handle) => quote! {},
    }
}

/// Generate code for a device description
///
/// Takes into account monomorphization.
///
/// Returns the name of the device, generated code and any types referenced in the code.
pub fn generate_device_desc<'ir>(
    ctx: &'ir IrCtx,
    varis: &MonomorphVariants,
    handle: Handle<DeviceDescription>,
) -> (&'ir str, proc_macro2::TokenStream, TypeReferences) {
    let mut refs = TypeReferences {
        references: BTreeSet::new(),
    };
    let desc = &ctx.device_descs[handle];
    let name = &ctx.identifiers[desc.name];

    let name_ident = ident(IdentType::Device, name);

    let get_funcs = desc
        .registers
        .handles()
        .map(|reg| generate_reg_get_method(ctx, varis, &mut refs, reg))
        .collect::<Vec<_>>();
    let set_funcs = desc
        .registers
        .handles()
        .map(|reg| generate_reg_set_method(ctx, varis, &mut refs, reg))
        .collect::<Vec<_>>();

    let consts = desc
        .registers
        .handles()
        .map(|reg| generate_const(ctx, varis, &mut refs, reg));

    let code = quote! {
        pub struct #name_ident(pub *mut u8);

        impl #name_ident {
            #(#consts)*

            pub const unsafe fn new(addr: *mut u8) -> Self {
                Self(addr)
            }

            #(#get_funcs)*

            #(#set_funcs)*

        }
    };

    (name, code, refs)
}

fn generate_const(
    ctx: &IrCtx,
    varis: &MonomorphVariants,
    refs: &mut TypeReferences,
    reg: Handle<RegisterDescription>,
) -> TokenStream {
    let desc = &ctx.registers[reg];
    let ty = &ctx.type_refs[desc.type_ref];

    let variables = match ty {
        TypeRef::BitVector(handle) | TypeRef::Unsigned(handle) | TypeRef::Signed(handle) => {
            let width = generate_type_ref(ctx, varis, None, refs, *handle);
            Some(("WIDTH", width))
        }
        TypeRef::Index(handle) => {
            let size = generate_type_ref(ctx, varis, None, refs, *handle);
            Some(("SIZE", size))
        }
        TypeRef::Vector(len, _) => {
            let len = generate_type_ref(ctx, varis, None, refs, *len);
            Some(("LEN", len))
        }
        _ => None,
    };
    let Some((suffix, value)) = variables else {
        return TokenStream::new();
    };
    let desc_name = &ctx.identifiers[desc.name];
    let const_name = ident(IdentType::Constant, format!("{desc_name}_{suffix}"));
    quote! {
        pub const #const_name: usize = #value;
    }
}

fn generate_reg_get_method(
    ctx: &IrCtx,
    varis: &MonomorphVariants,
    refs: &mut TypeReferences,
    reg: Handle<RegisterDescription>,
) -> TokenStream {
    let reg = &ctx.registers[reg];
    if !matches!(
        reg.access,
        RegisterAccess::ReadOnly | RegisterAccess::ReadWrite
    ) {
        return quote! {};
    }

    let name = ident(IdentType::Variable, &ctx.identifiers[reg.name]);
    let offset = reg.address as usize;

    let desc = &reg.description;
    let desc = if desc.is_empty() {
        quote! {}
    } else {
        quote! { #[doc = #desc]}
    };

    let raw_ty = &ctx.type_refs[reg.type_ref];

    if let TypeRef::Vector(len, inner) = raw_ty {
        let unchecked_name = ident(IdentType::Method, format!("{name}_unchecked"));
        let iter_name = ident(IdentType::Method, format!("{name}_volatile_iter"));

        let scalar_ty = generate_type_ref(ctx, varis, None, refs, *inner);
        let size = generate_type_ref(ctx, varis, None, refs, *len);

        quote! {
            #desc
            pub fn #name(&self, idx: usize) -> Option<#scalar_ty> {
                if idx >= #size {
                    None
                } else {
                    Some(unsafe { self.#unchecked_name(idx)})
                }
            }

            #desc
            pub unsafe fn #unchecked_name(&self, idx: usize) -> #scalar_ty {
                let ptr = self.0.add(#offset).cast::<#scalar_ty>();

                ptr.add(idx).read_volatile()
            }

            #desc
            pub fn #iter_name(&self) -> impl DoubleEndedIterator<Item = #scalar_ty> + '_ {
                (0..#size).map(|i| unsafe {
                    self.#unchecked_name(i)
                })
            }
        }
    } else if ctx.tags[reg.tags].iter().any(|tag| tag == "zero-width") {
        let ty = generate_type_ref(ctx, varis, None, refs, reg.type_ref);
        quote! {
            pub fn #name(&self) -> #ty {
                let _ = unsafe {
                    self.0.add(#offset).cast::<u8>().read_volatile()
                };
                unsafe { core::mem::transmute(()) }
            }
        }
    } else {
        let ty = generate_type_ref(ctx, varis, None, refs, reg.type_ref);
        quote! {
            #desc
            pub fn #name(&self) -> #ty {
                unsafe {
                    self.0.add(#offset).cast::<#ty>().read_volatile()
                }
            }
        }
    }
}

fn generate_reg_set_method(
    ctx: &IrCtx,
    varis: &MonomorphVariants,
    refs: &mut TypeReferences,
    reg: Handle<RegisterDescription>,
) -> TokenStream {
    let reg = &ctx.registers[reg];
    if !matches!(
        reg.access,
        RegisterAccess::WriteOnly | RegisterAccess::ReadWrite
    ) {
        return quote! {};
    }

    let name = ident(
        IdentType::Variable,
        format!("set_{}", &ctx.identifiers[reg.name]),
    );
    let offset = reg.address as usize;

    let desc = &reg.description;
    let desc = if desc.is_empty() {
        quote! {}
    } else {
        quote! { #[doc = #desc]}
    };

    let raw_ty = &ctx.type_refs[reg.type_ref];

    if let TypeRef::Vector(len, inner) = raw_ty {
        let unchecked_name = ident(IdentType::Method, format!("{name}_unchecked"));

        let scalar_ty = generate_type_ref(ctx, varis, None, refs, *inner);
        let size = generate_type_ref(ctx, varis, None, refs, *len);

        quote! {
            #desc
            pub fn #name(&self, idx: usize, val: #scalar_ty) -> Option<()> {
                if idx >= #size {
                    None
                } else {
                    unsafe { self.#unchecked_name(idx, val) };
                    Some(())
                }
            }

            #desc
            pub unsafe fn #unchecked_name(&self, idx: usize, val: #scalar_ty) {
                let ptr = self.0.add(#offset).cast::<#scalar_ty>();
                ptr.add(idx).write_volatile(val);
            }

        }
    } else if ctx.tags[reg.tags].iter().any(|tag| tag == "zero-width") {
        let ty = generate_type_ref(ctx, varis, None, refs, reg.type_ref);
        quote! {
            pub fn #name(&self, _val: #ty) {
                unsafe {
                    self.0.add(#offset).cast::<u8>().write_volatile(0)
                }
            }
        }
    } else {
        let ty = generate_type_ref(ctx, varis, None, refs, reg.type_ref);
        quote! {
            #desc
            pub fn #name(&self, val: #ty) {
                unsafe {
                    self.0.add(#offset).cast::<#ty>().write_volatile(val)
                }
            }
        }
    }
}

/// Types referenced during code generation.
pub struct TypeReferences {
    pub references: BTreeSet<Handle<TypeName>>,
}

fn generate_type_ref(
    ctx: &IrCtx,
    varis: &MonomorphVariants,
    variant: Option<&TypeRefVariant>,
    refs: &mut TypeReferences,
    ty: Handle<TypeRef>,
) -> TokenStream {
    let handle = lookup_sub(variant, ty);
    match &ctx.type_refs[handle] {
        TypeRef::BitVector(handle) => {
            let size = &ctx.type_refs[lookup_sub(variant, *handle)];

            if let TypeRef::Nat(n) = size {
                // TODO maybe special case on n == 1 like the C backend?
                let n = n.div_ceil(8) as usize;
                let n_lit = Literal::usize_unsuffixed(n);
                quote! { [u8; #n_lit] }
            } else {
                quote! { compile_error!("BitVector with length not known after monomorphisation") }
            }
        }
        TypeRef::Unsigned(handle) => {
            let size = &ctx.type_refs[lookup_sub(variant, *handle)];

            if let TypeRef::Nat(n) = size {
                let n = po2_type(*n);
                let name = format!("u{n}");
                ident(IdentType::Raw, name).into_token_stream()
            } else {
                quote! { compile_error!("Unsigned with length not known after monomorphisation") }
            }
        }
        TypeRef::Signed(handle) => {
            let size = &ctx.type_refs[lookup_sub(variant, *handle)];

            if let TypeRef::Nat(n) = size {
                let n = po2_type(*n);
                let name = format!("i{n}");
                ident(IdentType::Raw, name).into_token_stream()
            } else {
                quote! { compile_error!("Signed with length not known after monomorphisation") }
            }
        }
        TypeRef::Index(handle) => {
            let size = &ctx.type_refs[lookup_sub(variant, *handle)];

            if let TypeRef::Nat(n) = size {
                let n = po2_type(n.ilog2() as u64);
                let name = format!("u{n}");
                ident(IdentType::Raw, name).into_token_stream()
            } else {
                quote! { compile_error!("Index with length not known after monomorphisation") }
            }
        }
        TypeRef::Bool => quote! { bool },
        TypeRef::Float => quote! { f32 },
        TypeRef::Double => quote! { f64 },
        TypeRef::Vector(len, inner) => {
            let len = generate_type_ref(ctx, varis, variant, refs, *len);
            let inner = generate_type_ref(ctx, varis, variant, refs, *inner);
            quote! { [#inner; #len] }
        }
        TypeRef::Tuple(handle_range) => {
            let tys = handle_range
                .handles()
                .map(|ty| generate_type_ref(ctx, varis, variant, refs, ty));
            quote! {
                ( #(#tys,)*)
            }
        }
        TypeRef::Variable(handle) => {
            let name = &ctx.identifiers[*handle];
            let name_ident = ident(IdentType::TypeVariable, name);
            quote! { #name_ident }
        }
        TypeRef::Nat(n) => {
            let n = Literal::u64_unsuffixed(*n);
            quote! { #n }
        }
        TypeRef::Reference { name, args } => {
            refs.references.insert(*name);
            let mono_ref = if let Some(var) = variant {
                var.monomorph_type_ref_substitutions.get(&handle)
            } else {
                None
            };
            let mono_ref = mono_ref.or_else(|| varis.type_refs.get(&handle));

            if let Some(mono_ref) = mono_ref.copied() {
                let mono_variant = &varis.type_ref_variants[mono_ref];
                let (name, _has_mono_arg) = mono_variant_name(ctx, mono_variant);
                let any_args = mono_variant
                    .argument_mono_values
                    .iter()
                    .any(|s| s.is_none());
                let args = args
                    .handles()
                    .zip(&mono_variant.argument_mono_values)
                    .filter_map(|(arg, arg_val)| {
                        if let Some(_val) = arg_val {
                            None
                        } else {
                            Some(arg)
                        }
                    })
                    .map(|arg| generate_type_ref(ctx, varis, variant, refs, arg));
                let args = if any_args {
                    quote! { < #(#args, )* > }
                } else {
                    quote! {}
                };
                quote! {
                    #name #args
                }
            } else {
                let ty_ident = ident(IdentType::Type, &ctx.type_names[*name].base);
                let args = if args.len > 0 {
                    let args = args
                        .handles()
                        .map(|arg| generate_type_ref(ctx, varis, variant, refs, arg));
                    quote! {
                        < #(#args,)* >
                    }
                } else {
                    quote! {}
                };

                quote! {
                    #ty_ident #args
                }
            }
        }
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

fn mono_variant_name(ctx: &IrCtx, var: &TypeRefVariant) -> (Ident, bool) {
    let desc = &ctx.type_descs[var.original_type_desc];
    let ty_name = &ctx.type_names[desc.name];
    let args = var
        .argument_mono_values
        .iter()
        .map(|arg| arg.map(|val| type_to_ident(ctx, val)));

    let has_mono_args = args.clone().any(|arg| arg.is_some());

    let mut arg_names = String::new();
    if has_mono_args {
        for arg in args.flatten() {
            arg_names.push('_');
            arg_names.push_str(&arg);
        }
    }

    let type_name_base = ident(IdentType::Type, &ty_name.base);
    (
        ident(IdentType::Raw, format!("{type_name_base}{arg_names}")),
        has_mono_args,
    )
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
                let add = type_to_ident(ctx, handle);
                if !add.is_empty() {
                    name.push('_');
                    name.push_str(&add);
                }
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
                let add = type_to_ident(ctx, arg);
                if !add.is_empty() {
                    ident.push('_');
                    ident.push_str(&add);
                }
            }
            ident
        }
    }
}
