// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use heck::{ToPascalCase, ToSnakeCase};
use proc_macro2::TokenStream;
use quote::quote;

use crate::{
    generators::{ident, types::TypeGenerator},
    parse::{DeviceDesc, RegisterAccess, Type},
};

pub struct DeviceGenerator;

impl DeviceGenerator {
    pub fn new() -> Self {
        DeviceGenerator
    }

    pub fn generate_device_type(
        &mut self,
        desc: &DeviceDesc,
        ty_gen: &mut TypeGenerator,
    ) -> proc_macro2::TokenStream {
        let device_type_name = desc.name.to_pascal_case();
        let name = ident(&device_type_name);
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

                let register_name = reg.name.to_snake_case();
                let name = ident(&register_name);
                let offset = reg.address as usize;

                let reg_description = &reg.description;

                if let Type::Vec(len, inner) = &reg.reg_type {
                    let unchecked_name = ident(format!("{}_unchecked", &register_name));
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

                let register_name = reg.name.to_snake_case();
                let name = ident(format!("set_{}", &register_name));
                let offset = reg.address as usize;

                let reg_description = &reg.description;

                if let Type::Vec(len, inner) = &reg.reg_type {
                    let unchecked_name = ident(format!("set_{}_unchecked", &register_name));
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

        let tags = self.generate_tag_docs(&desc.tags);

        quote! {
            #[doc = #description]
            #tags
            pub struct #name(pub *mut u8);

            impl #name {
                pub const unsafe fn new(addr: *mut u8) -> Self {
                    Self(addr)
                }

                #(#get_funcs)*

                #(#set_funcs)*
            }
        }
    }

    fn generate_tag_docs(&mut self, tags: &[String]) -> TokenStream {
        if tags.is_empty() {
            return TokenStream::new();
        }

        let mut toks = TokenStream::new();

        toks.extend(quote! { #[doc = ""] });
        toks.extend(quote! { #[doc = "## Tags"]});
        toks.extend(quote! { #[doc = ""] });

        for tag in tags {
            let msg = format!(" - `{}`", tag);
            toks.extend(quote! { #[doc = #msg] });
        }

        toks
    }
}

impl Default for DeviceGenerator {
    fn default() -> Self {
        Self::new()
    }
}
