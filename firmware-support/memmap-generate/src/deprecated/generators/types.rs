// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::collections::BTreeSet;

use crate::{
    deprecated::generators::{generate_tag_docs, ident, IdentType},
    deprecated::hal_set::TypeDefAnnotations,
    input_language::{
        Constructor, NamedConstructor, TypeArg, TypeDefinition, TypeDescription, TypeName, TypeRef,
    },
};
use proc_macro2::Literal;
use quote::quote;

fn clog2(n: &u64) -> u64 {
    n.ilog2().max(8) as u64
}

pub enum ParsedType {
    BitVector(Box<ParsedType>),
    Unsigned(Box<ParsedType>),
    Signed(Box<ParsedType>),
    Index(Box<ParsedType>),
    Bool,
    Float,
    Double,
    Vector(Box<ParsedType>, Box<ParsedType>),
    Maybe(Box<ParsedType>),
    Either(Box<ParsedType>, Box<ParsedType>),
    Tuple(Vec<ParsedType>),
    Custom {
        name: TypeName,
        args: Vec<ParsedType>,
    },
    Variable(String),
    Nat(u64),
}

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq, Hash, Clone)]
pub enum PrimitiveType {
    BitVector,
    Unsigned,
    Signed,
    Index,
    Bool,
    Float,
    Double,
    Vector,
    Maybe,
    Either,
}

pub struct TypeGenerator {
    referenced_custom_types: BTreeSet<TypeName>,
    referenced_primitives: BTreeSet<PrimitiveType>,
}

impl TypeGenerator {
    pub fn new() -> Self {
        TypeGenerator {
            referenced_custom_types: Default::default(),
            referenced_primitives: Default::default(),
        }
    }

    pub fn clear(&mut self) {
        self.referenced_custom_types.clear();
        self.referenced_primitives.clear();
    }

    pub fn referenced_primitives(&self) -> &BTreeSet<PrimitiveType> {
        &self.referenced_primitives
    }

    pub fn referenced_custom_types(&self) -> &BTreeSet<TypeName> {
        &self.referenced_custom_types
    }

    pub fn parse_type_ref(ty: &TypeRef) -> ParsedType {
        match ty {
            TypeRef::TypeReference { base, args } => {
                match (base.base.as_str(), base.module.as_str()) {
                    ("BitVector", "Clash.Sized.Internal.BitVector") => {
                        ParsedType::BitVector(Box::new(Self::parse_type_ref(&args[0])))
                    }
                    ("Signed", "Clash.Sized.Internal.Signed") => {
                        ParsedType::Signed(Box::new(Self::parse_type_ref(&args[0])))
                    }
                    ("Unsigned", "Clash.Sized.Internal.Unsigned") => {
                        ParsedType::Unsigned(Box::new(Self::parse_type_ref(&args[0])))
                    }
                    ("Index", "Clash.Sized.Internal.Index") => {
                        ParsedType::Index(Box::new(Self::parse_type_ref(&args[0])))
                    }
                    ("Float", "GHC.Types") => ParsedType::Float,
                    ("Double", "GHC.Types") => ParsedType::Double,
                    ("Bool", "GHC.Types") => ParsedType::Bool,
                    ("Vec", "Clash.Sized.Vector") => ParsedType::Vector(
                        Box::new(Self::parse_type_ref(&args[0])),
                        Box::new(Self::parse_type_ref(&args[1])),
                    ),
                    ("Unit", "GHC.Tuple") => ParsedType::Tuple(vec![]),
                    ("Either", "GHC.Internal.Data.Either") => ParsedType::Either(
                        Box::new(Self::parse_type_ref(&args[0])),
                        Box::new(Self::parse_type_ref(&args[1])),
                    ),
                    ("Maybe", "GHC.Internal.Maybe") => {
                        ParsedType::Maybe(Box::new(Self::parse_type_ref(&args[0])))
                    }
                    (name, "GHC.Tuple") if name.starts_with("Tuple") => {
                        ParsedType::Tuple(args.into_iter().map(Self::parse_type_ref).collect())
                    }
                    (_, _) => ParsedType::Custom {
                        name: base.clone(),
                        args: args.iter().map(Self::parse_type_ref).collect(),
                    },
                }
            }
            TypeRef::Variable(name) => ParsedType::Variable(name.clone()),
            TypeRef::Nat(n) => ParsedType::Nat(*n),
            TypeRef::Tuple(type_refs) => {
                ParsedType::Tuple(type_refs.into_iter().map(Self::parse_type_ref).collect())
            }
        }
    }

    pub fn generate_parsed_type_ref(&mut self, ty: &ParsedType) -> proc_macro2::TokenStream {
        match ty {
            ParsedType::BitVector(parsed_type) => {
                self.referenced_primitives.insert(PrimitiveType::BitVector);
                let n = self.generate_parsed_type_ref(&parsed_type);
                quote! { BitVector<#n> }
            }
            ParsedType::Unsigned(parsed_type) => {
                self.referenced_primitives.insert(PrimitiveType::Unsigned);
                let n = self.generate_parsed_type_ref(&parsed_type);
                quote! { Unsigned<#n> }
            }
            ParsedType::Signed(parsed_type) => {
                self.referenced_primitives.insert(PrimitiveType::Signed);
                let n = self.generate_parsed_type_ref(&parsed_type);
                quote! { Signed<#n> }
            }
            ParsedType::Index(parsed_type) => {
                self.referenced_primitives.insert(PrimitiveType::Index);
                let n = self.generate_parsed_type_ref(&parsed_type);
                quote! { Index<#n> }
            }
            ParsedType::Bool => {
                self.referenced_primitives.insert(PrimitiveType::Bool);
                quote! { bool }
            }
            ParsedType::Float => {
                self.referenced_primitives.insert(PrimitiveType::Float);
                quote! { f32 }
            }
            ParsedType::Double => {
                self.referenced_primitives.insert(PrimitiveType::Double);
                quote! { f64 }
            }
            ParsedType::Vector(n, t) => {
                self.referenced_primitives.insert(PrimitiveType::Vector);
                let n_toks = self.generate_parsed_type_ref(&*n);
                let t_toks = self.generate_parsed_type_ref(&*t);
                quote! { [#t_toks ; #n_toks ] }
            }
            ParsedType::Maybe(t) => {
                self.referenced_primitives.insert(PrimitiveType::Maybe);
                let t_toks = self.generate_parsed_type_ref(&*t);
                quote! {
                    Option<#t_toks>
                }
            }
            ParsedType::Either(a, b) => {
                self.referenced_primitives.insert(PrimitiveType::Either);
                let a_toks = self.generate_parsed_type_ref(&*a);
                let b_toks = self.generate_parsed_type_ref(&*b);
                quote! {
                    Result<#a_toks, #b_toks>
                }
            }
            ParsedType::Tuple(tys) => {
                let tys_toks = tys
                    .into_iter()
                    .map(|t| {
                        let ty = self.generate_parsed_type_ref(t);
                        quote! { #ty , }
                    })
                    .collect::<Vec<_>>();
                quote! { ( #(#tys_toks)* ) }
            }
            ParsedType::Custom { name, args } => {
                self.referenced_custom_types.insert(name.clone());
                let ty_ident = ident(IdentType::Type, &name.base);
                let args = args
                    .into_iter()
                    .map(|t| {
                        let t = self.generate_parsed_type_ref(t);
                        quote! { #t , }
                    })
                    .collect::<Vec<_>>();
                if args.is_empty() {
                    quote! { #ty_ident }
                } else {
                    quote! { #ty_ident< #(#args)* >}
                }
            }
            ParsedType::Variable(name) => {
                let var_name = ident(IdentType::TypeVariable, &name);
                quote! { #var_name }
            }
            ParsedType::Nat(n) => {
                let lit = Literal::u128_unsuffixed(*n as u128);
                quote! { #lit }
            }
        }
    }

    pub fn generate_type_ref(&mut self, ty: &TypeRef) -> proc_macro2::TokenStream {
        let parsed = Self::parse_type_ref(ty);
        self.generate_parsed_type_ref(&parsed)
    }

    pub fn generate_type_desc(
        &mut self,
        ann: &TypeDefAnnotations,
        desc: &TypeDescription,
    ) -> proc_macro2::TokenStream {
        let name = ident(IdentType::Type, &desc.name.base);

        let args = if desc.type_args.is_empty() {
            quote! {}
        } else {
            let args = desc
                .type_args
                .iter()
                .map(|arg| match arg {
                    TypeArg::Type { name } => {
                        let var_name = ident(IdentType::TypeVariable, name);
                        quote! { #var_name, }
                    }
                    TypeArg::Number { name } => {
                        let var_name = ident(IdentType::TypeVariable, name);
                        quote! { const #var_name: u128, }
                    }
                })
                .collect::<Vec<_>>();
            quote! {
                < #(#args)* >
            }
        };

        let repr = self.generate_repr(&desc.definition);

        let derives = &ann.derives;
        let tags = generate_tag_docs(None.into_iter(), ann.tags.iter().map(String::as_str));

        let attrs = quote! { #tags #repr #(#derives)* };

        match &desc.definition {
            TypeDefinition::DataType(constructors) if constructors.is_empty() => {
                // TODO handle phantom types for args and stuff
                quote! {
                    #attrs
                    pub struct #name;
                }
            }
            TypeDefinition::DataType(constructors) if constructors.len() == 1 => {
                let constructor = &constructors[0];

                if constructor.0 != desc.name.base {
                    panic!(
                        "Single-constructor data types are required to have the constructor name match the type name. \
                        Type name: '{type_name:?}', Constructor name: '{constructor_name:?}'",
                        type_name = desc.name.base,
                        constructor_name = constructor.0
                    )
                }

                match &constructor.1 {
                    Constructor::Nameless { fields } => {
                        let fields_toks = fields.iter().map(|f| self.generate_type_ref(f));

                        let body = if fields.is_empty() {
                            quote! {}
                        } else {
                            quote! { (#(pub #fields_toks,)*) }
                        };

                        quote! {
                            #attrs
                            pub struct #name #args #body;
                        }
                    }
                    Constructor::Record { fields } => {
                        let fields_toks = fields.iter().map(|(name, ty)| {
                            let id = ident(IdentType::Variable, name);
                            let ty = self.generate_type_ref(ty);
                            quote! { pub #id: #ty }
                        });
                        quote! {
                            #attrs
                            pub struct #name #args {
                                #(#fields_toks,)*
                            }
                        }
                    }
                }
            }
            TypeDefinition::DataType(constructors) => {
                let variants = constructors.iter().map(|NamedConstructor(name, con)| {
                    let id = ident(IdentType::Type, name);
                    match con {
                        Constructor::Nameless { fields } => {
                            let fields_toks = fields.iter().map(|f| self.generate_type_ref(f));
                            if fields.is_empty() {
                                quote! { #id, }
                            } else {
                                quote! {
                                    #id(#(#fields_toks,)*),
                                }
                            }
                        }
                        Constructor::Record { fields } => {
                            let fields = fields.iter().map(|(name, ty)| {
                                let name = ident(IdentType::Variable, name);
                                let ty = self.generate_type_ref(ty);
                                quote! { #name: #ty }
                            });

                            quote! {
                                #id {
                                    #(#fields,)*
                                },
                            }
                        }
                    }
                });
                quote! {
                    #attrs
                    pub enum #name #args {
                        #(#variants)*
                    }
                }
            }
            TypeDefinition::Newtype(constructor) => {
                if constructor.0 != desc.name.base {
                    panic!(
                        "newtypes are required to have the constructor name match the type name. \
                        Type name: '{type_name:?}', Constructor name: '{constructor_name:?}'",
                        type_name = desc.name.base,
                        constructor_name = constructor.0
                    )
                }

                match &constructor.1 {
                    Constructor::Nameless { fields } => {
                        if fields.len() != 1 {
                            panic!(
                                "newtypes are required to have only one field \
                            Type name: '{type_name:?}''",
                                type_name = desc.name.base,
                            );
                        }

                        let field = &fields[0];
                        let ty = self.generate_type_ref(field);
                        quote! {
                            #attrs
                            pub struct #name #args (pub #ty);
                        }
                    }
                    Constructor::Record { fields } => {
                        if fields.len() != 1 {
                            panic!(
                                "newtypes are required to have only one field \
                            Type name: '{type_name:?}''",
                                type_name = desc.name.base,
                            );
                        }

                        let (field_name, ty) = &fields[0];
                        let field_name = ident(IdentType::Variable, field_name);
                        let ty = self.generate_type_ref(ty);
                        quote! {
                            #attrs
                            pub struct #name #args {
                                pub #field_name: #ty
                            }
                        }
                    }
                }
            }
            TypeDefinition::Builtin(_builtin_type) => quote! {},
            TypeDefinition::Synonym(type_ref) => {
                let parsed_type = Self::parse_type_ref(type_ref);
                let ty = self.generate_parsed_type_ref(&parsed_type);
                quote! {
                    pub type #name #args = #ty;
                }
            }
        }
    }

    fn generate_repr(&mut self, desc: &TypeDefinition) -> proc_macro2::TokenStream {
        match desc {
            TypeDefinition::DataType(named_constructors) if named_constructors.is_empty() => {
                quote! {}
            }
            TypeDefinition::DataType(named_constructors) if named_constructors.len() == 1 => {
                quote! { #[repr(C)] }
            }
            TypeDefinition::DataType(named_constructors) => {
                let fieldless = named_constructors
                    .iter()
                    .all(|NamedConstructor(_name, con)| match con {
                        Constructor::Nameless { fields } => fields.is_empty(),
                        Constructor::Record { fields } => fields.is_empty(),
                    });
                let n = clog2(&(named_constructors.len() as u64));
                let repr = ident(IdentType::Raw, format!("u{}", n));
                if fieldless {
                    quote! { #[repr(#repr)] }
                } else {
                    quote! { #[repr(C, #repr)] }
                }
            }
            TypeDefinition::Newtype(_named_constructor) => {
                quote! { #[repr(transparent)] }
            }
            TypeDefinition::Builtin(_builtin_type) => quote! {},
            TypeDefinition::Synonym(_type_ref) => quote! {},
        }
    }
}

impl Default for TypeGenerator {
    fn default() -> Self {
        Self::new()
    }
}
