// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use crate::{
    generators::{generic_name, ident, DebugDerive},
    parse::{Type, TypeDefinition, VariantDesc},
};

use proc_macro2::{Ident, Span};
use quote::{quote, ToTokens};

fn next_pw2(n: &u64) -> u64 {
    n.next_power_of_two().max(8)
}

pub fn generate_type_ref(ty: &Type) -> proc_macro2::TokenStream {
    match ty {
        Type::Bool => quote! { bool },
        Type::BitVector(n) | Type::Unsigned(n) => {
            let ty_name = ident(format!("u{}", next_pw2(n)));
            quote! { #ty_name }
        }
        Type::Signed(n) => {
            let ty_name = ident(format!("i{}", next_pw2(n)));
            quote! { #ty_name }
        }
        Type::Vec(n, ty) => {
            let inner = generate_type_ref(&ty);
            quote! { [#inner; #n] }
        }
        Type::Reference(name, args) => {
            let ty_name = ident(name);
            let args = args.into_iter().map(generate_type_ref).collect::<Vec<_>>();
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

pub fn generate_type_def(ty: &TypeDefinition, debug: DebugDerive) -> proc_macro2::TokenStream {
    let name = ident(&ty.name);

    let repr = generate_repr(ty);
    let derives = match debug {
        DebugDerive::None => quote! {
            #[derive(Copy, Clone, PartialEq, Eq)]
        },
        DebugDerive::Std => quote! {
            #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        },
        DebugDerive::Ufmt => quote! {
            #[derive(uDebug, Copy, Clone, PartialEq, Eq)]
        },
    };

    let attrs = quote! { #repr #derives };

    let generics = if ty.generics > 0 {
        let names = (0..ty.generics).map(generic_name).collect::<Vec<_>>();
        quote! { < #(#names,)* > }
    } else {
        quote! {}
    };

    if let Type::SumOfProducts {
        name: _name,
        meta: _,
        variants,
    } = &ty.definition
    {
        match variants.as_slice() {
            [] => quote! {
                #attrs
                pub struct #name #generics;
            },
            [var @ VariantDesc {
                name: var_name,
                fields,
            }] => match fields.as_slice() {
                [] => {
                    if &ty.name == var_name {
                        quote! {
                            #attrs
                            pub struct #name #generics;
                        }
                    } else {
                        panic!("Single-constructor data types are required to have the constructor name match the type name.")
                        // let var_name = Ident::new(var_name, Span::call_site());
                        // quote! {
                        //     #attrs
                        //     pub enum #name #generics {
                        //         #var_name
                        //     }
                        // }
                    }
                }
                xs => {
                    if &ty.name == var_name {
                        let no_names = xs.iter().all(|v| v.name.is_empty());
                        let all_names = xs.iter().all(|v| !v.name.is_empty());

                        if no_names == all_names && !xs.is_empty() {
                            panic!("Variant fields must either ALL be named or NONE be named.");
                        }

                        if no_names {
                            let fields = xs
                                .iter()
                                .map(|f| generate_type_ref(&f.type_))
                                .collect::<Vec<_>>();
                            quote! {
                                #attrs
                                pub struct #name #generics (#(pub #fields,)*);
                            }
                        } else {
                            let fields = xs
                                .iter()
                                .map(|f| {
                                    let name = Ident::new(&f.name, Span::call_site());
                                    let ty = generate_type_ref(&f.type_);
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
                        let var = generate_variant(var);
                        quote! {
                            #attrs
                            pub enum #name #generics {
                                #var
                            }
                        }
                    }
                }
            },
            vars => {
                let variants = vars.iter().map(generate_variant).collect::<Vec<_>>();
                quote! {
                    #attrs
                    pub enum #name #generics {
                        #(#variants)*
                    }
                }
            }
        }
    } else {
        let inner = generate_type_ref(&ty.definition);
        quote! {
            #attrs
            pub struct #name #generics(pub #inner);
        }
    }
}

fn generate_repr(ty: &TypeDefinition) -> proc_macro2::TokenStream {
    if let Type::SumOfProducts {
        name: _,
        meta: _,
        variants,
    } = &ty.definition
    {
        match variants.as_slice() {
            [] => quote! {},
            [x] => {
                if &x.name == &ty.name {
                    quote! { #[repr(C)] }
                } else {
                    panic!("Single-constructor data types are required to have the constructor name match the type name.")
                    // quote! { #[repr(u8)] }
                }
            }
            vars => {
                let n = next_pw2(&(vars.len() as u64));
                let repr = ident(format!("u{}", n));
                quote! { #[repr(#repr)] }
            }
        }
    } else {
        quote! {}
    }
}

fn generate_variant(v: &VariantDesc) -> proc_macro2::TokenStream {
    let name = ident(&v.name);

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
            .map(|f| generate_type_ref(&f.type_))
            .collect::<Vec<_>>();
        quote! {
            #name (#(#fields,)*),
        }
    } else {
        let fields = v
            .fields
            .iter()
            .map(|f| {
                let name = ident(&f.name);
                let ty = generate_type_ref(&f.type_);
                quote! { #name: #ty, }
            })
            .collect::<Vec<_>>();
        quote! {
            #name {
                #(#fields)*
            }
        }
    }
}
