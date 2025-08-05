// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use heck::ToShoutySnakeCase;
use quote::quote;

use crate::{
    generators::{generate_tag_docs, ident, types::TypeGenerator, IdentType},
    hal_set::DeviceDescAnnotations,
    parse::{DeviceDesc, RegisterAccess, RegisterDesc, Type},
};

pub struct DeviceGenerator;

impl DeviceGenerator {
    pub fn new() -> Self {
        DeviceGenerator
    }

    pub fn generate_device_type(
        &mut self,
        ann: &DeviceDescAnnotations,
        desc: &DeviceDesc,
        ty_gen: &mut TypeGenerator,
    ) -> proc_macro2::TokenStream {
        let name = ident(IdentType::Device, &desc.name);
        let description = &desc.description;

        let get_funcs = desc
            .registers
            .iter()
            .map(|reg| {
                let can_read = reg.access == RegisterAccess::ReadOnly
                    || reg.access == RegisterAccess::ReadWrite;
                if !can_read {
                    return quote! {};
                }

                let name = ident(IdentType::Variable, &reg.name);
                let offset = reg.address as usize;

                let reg_description = &reg.description;

                if let Type::Vec(len, inner) = &reg.reg_type {
                    let unchecked_name =
                        ident(IdentType::Method, format!("{}_unchecked", &reg.name));
                    let iter_name =
                        ident(IdentType::Method, format!("{}_volatile_iter", &reg.name));
                    let scalar_ty = ty_gen.generate_type_ref(inner);
                    let size = *len as usize;

                    quote! {
                        #[doc = #reg_description]
                        pub fn #name(&self, idx: usize) -> Option<#scalar_ty> {
                            if idx >= #size {
                                None
                            } else {
                                Some(unsafe { self.#unchecked_name(idx)})
                            }
                        }

                        #[doc = #reg_description]
                        pub unsafe fn #unchecked_name(&self, idx: usize) -> #scalar_ty {
                            let ptr = self.0.add(#offset).cast::<#scalar_ty>();

                            ptr.add(idx).read_volatile()
                        }

                        #[doc = #reg_description]
                        pub fn #iter_name(&self) -> impl DoubleEndedIterator<Item = #scalar_ty> + '_ {
                            (0..#size).map(|i| unsafe {
                                self.#unchecked_name(i)
                            })
                        }
                    }
                } else if reg.tags.iter().any(|tag| tag == "zero-width") {
                    let ty = ty_gen.generate_type_ref(&reg.reg_type);
                    quote! {
                        #[doc = #reg_description]
                        pub fn #name(&self) -> #ty {
                            let _ = unsafe {
                                self.0.add(#offset).cast::<u8>().read_volatile()
                            };

                            unsafe { core::mem::transmute(()) }
                        }
                    }
                } else {
                    let ty = ty_gen.generate_type_ref(&reg.reg_type);
                    quote! {
                        #[doc = #reg_description]
                        pub fn #name(&self) -> #ty {
                            unsafe {
                                self.0.add(#offset).cast::<#ty>().read_volatile()
                            }
                        }
                    }
                }
            })
            .collect::<Vec<_>>();

        let set_funcs = desc
            .registers
            .iter()
            .map(|reg| {
                let can_write = reg.access == RegisterAccess::WriteOnly
                    || reg.access == RegisterAccess::ReadWrite;

                if !can_write {
                    return quote! {};
                }
                let name = ident(IdentType::Method, format!("set_{}", &reg.name));
                let offset = reg.address as usize;

                let reg_description = &reg.description;

                if let Type::Vec(len, inner) = &reg.reg_type {
                    let unchecked_name =
                        ident(IdentType::Method, format!("set_{}_unchecked", &reg.name));
                    let scalar_ty = ty_gen.generate_type_ref(inner);
                    let size = *len as usize;

                    quote! {
                        #[doc = #reg_description]
                        pub fn #name(&self, idx: usize, val: #scalar_ty) -> Option<()> {
                            if idx >= #size {
                                None
                            } else {
                                unsafe { self.#unchecked_name(idx, val) };
                                Some(())
                            }
                        }

                        #[doc = #reg_description]
                        pub unsafe fn #unchecked_name(&self, idx: usize, val: #scalar_ty) {
                            let ptr = self.0.add(#offset).cast::<#scalar_ty>();

                            ptr.add(idx).write_volatile(val);
                        }
                    }
                } else if reg.tags.iter().any(|tag| tag == "zero-width") {
                    let ty = ty_gen.generate_type_ref(&reg.reg_type);
                    quote! {
                        #[doc = #reg_description]
                        pub fn #name(&self, _val: #ty) {
                            unsafe {
                                self.0.add(#offset).cast::<u8>().write_volatile(0)
                            }
                        }
                    }
                } else {
                    let ty = ty_gen.generate_type_ref(&reg.reg_type);
                    quote! {
                        #[doc = #reg_description]
                        pub fn #name(&self, val: #ty) {
                            unsafe {
                                self.0.add(#offset).cast::<#ty>().write_volatile(val)
                            }
                        }
                    }
                }
            })
            .collect::<Vec<_>>();

        let consts = desc
            .registers
            .iter()
            .filter_map(generate_const)
            .collect::<Vec<_>>();

        let tags = generate_tag_docs(
            desc.tags.iter().map(String::as_str),
            ann.tags.iter().map(String::as_str),
        );

        let derives = &ann.derives;

        quote! {
            #[doc = #description]
            #tags
            #(#derives)*
            pub struct #name(pub *mut u8);

            impl #name {
                pub const unsafe fn new(addr: *mut u8) -> Self {
                    Self(addr)
                }

                #(#consts)*

                #(#get_funcs)*

                #(#set_funcs)*
            }
        }
    }
}

impl Default for DeviceGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Generates a constant for the given register description if applicable.
/// Returns `None` if the register type does not have a constant value.
fn generate_const(desc: &RegisterDesc) -> Option<proc_macro2::TokenStream> {
    let (const_suffix, const_value) = match &desc.reg_type {
        Type::BitVector(width) | Type::Signed(width) | Type::Unsigned(width) => {
            Some(("WIDTH", *width as usize))
        }
        Type::Index(size) => Some(("SIZE", *size as usize)),
        Type::Vec(len, _) => Some(("LEN", *len as usize)),
        _ => None,
    }?;
    let const_name = ident(
        IdentType::Raw,
        format!("{}_{}", desc.name, const_suffix).to_shouty_snake_case(),
    );
    Some(quote! {
        pub const #const_name: usize = #const_value;
    })
}
