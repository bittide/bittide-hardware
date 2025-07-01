// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::str::FromStr;

use crate::{
    generators::{generate_tag_docs, generic_name, ident, IdentType},
    hal_set::TypeDefAnnotations,
    parse::{Type, TypeDefinition, VariantDesc},
};
use proc_macro2::Literal;
use quote::{quote, ToTokens};

fn clog2(n: &u64) -> u64 {
    n.ilog2().max(8) as u64
}

fn next_pow2(n: &u64) -> u64 {
    n.next_power_of_two().max(8)
}

pub struct TypeGenerator;

impl TypeGenerator {
    pub fn new() -> Self {
        TypeGenerator
    }

    #[allow(clippy::only_used_in_recursion)]
    pub fn generate_type_ref(&mut self, ty: &Type) -> proc_macro2::TokenStream {
        match ty {
            Type::Bool => quote! { bool },
            Type::Float => quote! { f32 },
            Type::Double => quote! { f64 },
            Type::BitVector(n) | Type::Unsigned(n) => {
                let ty_name = ident(IdentType::Raw, format!("u{}", next_pow2(n)));
                quote! { #ty_name }
            }
            Type::Signed(n) => {
                let ty_name = ident(IdentType::Raw, format!("i{}", next_pow2(n)));
                quote! { #ty_name }
            }
            Type::Index(n) => {
                let n_lit = Literal::from_str(&n.to_string()).unwrap();

                quote! { Index![#n_lit] }
            }
            Type::Vec(n, ty) => {
                let inner = self.generate_type_ref(ty);
                let n = Literal::from_str(&n.to_string()).unwrap();

                quote! { [#inner; #n] }
            }
            Type::Reference(name, args) => {
                let ty_name = ident(IdentType::Type, name);
                let args = args
                    .iter()
                    .map(|x| self.generate_type_ref(x))
                    .collect::<Vec<_>>();
                if args.is_empty() {
                    quote! { #ty_name }
                } else {
                    quote! { #ty_name < #(#args,)* > }
                }
            }
            Type::Variable(i) => generic_name(*i).to_token_stream(),
            Type::SumOfProducts { .. } => panic!("SOP definitions can't be references"),
        }
    }

    pub fn generate_type_def(
        &mut self,
        ann: &TypeDefAnnotations,
        ty: &TypeDefinition,
    ) -> proc_macro2::TokenStream {
        let name = ident(IdentType::Type, &ty.name);

        let repr = self.generate_repr(ty);

        let derives = &ann.derives;
        let tags = generate_tag_docs(None.into_iter(), ann.tags.iter().map(String::as_str));

        let attrs = quote! { #tags #repr #(#derives)* };

        let generics = if ty.generics > 0 {
            let names = (0..ty.generics).map(generic_name).collect::<Vec<_>>();
            quote! { < #(#names,)* > }
        } else {
            quote! {}
        };

        if let Type::SumOfProducts { variants } = &ty.definition {
            match variants.as_slice() {
                // Empty type (unit)
                [] => quote! {
                    #attrs
                    pub struct #name #generics;
                },

                // Single constructor
                [variant_desc @ VariantDesc {
                    name: variant_name,
                    fields,
                }] => match fields.as_slice() {
                    [] => {
                        if &ty.name == variant_name {
                            quote! {
                                #attrs
                                pub struct #name #generics;
                            }
                        } else {
                            panic!(
                                "Single-constructor data types are required to have the constructor name match the type name. \
                                Type name: '{type_name:?}', Constructor name: '{constructor_name:?}'",
                                type_name = ty.name,
                                constructor_name = variant_name
                            )
                        }
                    }
                    xs => {
                        if &ty.name == variant_name {
                            let no_names = xs.iter().all(|v| v.name.is_empty());
                            let all_names = xs.iter().all(|v| !v.name.is_empty());

                            if no_names == all_names && !xs.is_empty() {
                                panic!("Variant fields must either ALL be named or NONE be named.");
                            }

                            if no_names {
                                let fields = xs
                                    .iter()
                                    .map(|f| self.generate_type_ref(&f.type_))
                                    .collect::<Vec<_>>();
                                quote! {
                                    #attrs
                                    pub struct #name #generics (#(pub #fields,)*);
                                }
                            } else {
                                let fields = xs
                                    .iter()
                                    .map(|f| {
                                        let name = ident(IdentType::Variable, &f.name);
                                        let ty = self.generate_type_ref(&f.type_);
                                        quote! { pub #name: #ty, }
                                    })
                                    .collect::<Vec<_>>();
                                quote! {
                                    #attrs
                                    pub struct #name #generics {
                                        #(#fields)*
                                    }
                                }
                            }
                        } else {
                            let variant_source = self.generate_variant(variant_desc);
                            quote! {
                                #attrs
                                pub enum #name #generics {
                                    #variant_source
                                }
                            }
                        }
                    }
                },

                // Multiple constructors
                variants => {
                    let variant_sources = variants
                        .iter()
                        .map(|x| self.generate_variant(x))
                        .collect::<Vec<_>>();
                    quote! {
                        #attrs
                        pub enum #name #generics {
                            #(#variant_sources)*
                        }
                    }
                }
            }
        } else {
            let inner = self.generate_type_ref(&ty.definition);
            quote! {
                #attrs
                pub struct #name #generics(pub #inner);
            }
        }
    }

    fn generate_repr(&mut self, ty: &TypeDefinition) -> proc_macro2::TokenStream {
        if let Type::SumOfProducts { variants } = &ty.definition {
            match variants.as_slice() {
                [] => quote! {},
                [x] => {
                    if x.name == ty.name {
                        quote! { #[repr(C)] }
                    } else {
                        panic!(
                            "Single-constructor data types are required to have the constructor name match the type name. \
                            Type name: {type_name:?}, Constructor name: {constructor_name:?}",
                            type_name = ty.name,
                            constructor_name = x.name
                        )
                    }
                }
                variants => {
                    let fieldless = variants.iter().all(|desc| desc.fields.is_empty());
                    let n = clog2(&(variants.len() as u64));
                    let repr = ident(IdentType::Raw, format!("u{}", n));
                    if fieldless {
                        quote! { #[repr(#repr)] }
                    } else {
                        quote! { #[repr(C, #repr)] }
                    }
                }
            }
        } else {
            quote! {}
        }
    }

    fn generate_variant(&mut self, v: &VariantDesc) -> proc_macro2::TokenStream {
        let name = ident(IdentType::Type, &v.name);

        let no_names = v.fields.iter().all(|f| f.name.is_empty());
        let all_names = v.fields.iter().all(|f| !f.name.is_empty());

        if no_names == all_names && !v.fields.is_empty() {
            panic!("Variant fields must either ALL be named or NONE be named.");
        }
        if v.fields.is_empty() {
            quote! {
                #name,
            }
        } else if no_names {
            let fields = v
                .fields
                .iter()
                .map(|f| self.generate_type_ref(&f.type_))
                .collect::<Vec<_>>();
            quote! {
                #name (#(#fields,)*),
            }
        } else {
            let fields = v
                .fields
                .iter()
                .map(|f| {
                    let name = ident(IdentType::Variable, &f.name);
                    let ty = self.generate_type_ref(&f.type_);
                    quote! { #name: #ty, }
                })
                .collect::<Vec<_>>();
            quote! {
                #name {
                    #(#fields)*
                },
            }
        }
    }
}

impl Default for TypeGenerator {
    fn default() -> Self {
        Self::new()
    }
}
