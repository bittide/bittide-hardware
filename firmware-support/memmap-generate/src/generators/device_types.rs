// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use quote::quote;

use crate::{
    generators::{ident, types::generate_type_ref},
    parse::{DeviceDesc, RegisterAccess, Type},
};

pub fn generate_device_type(desc: &DeviceDesc) -> proc_macro2::TokenStream {
    let name = ident(&desc.name);
    let description = &desc.description;

    let get_funcs = desc
        .registers
        .iter()
        .map(|reg| {
            let can_read =
                reg.access == RegisterAccess::ReadOnly || reg.access == RegisterAccess::ReadWrite;
            if !can_read {
                return quote! {};
            }

            let name = ident(&reg.name);
            let offset = reg.address as usize;

            if let Type::Vec(len, inner) = &reg.reg_type {
                let unchecked_name = ident(format!("{}_unchecked", &reg.name));
                let scalar_ty = generate_type_ref(&inner);
                let size = *len as usize;

                quote! {
                    pub fn #name(&self, idx: usize) -> Option<#scalar_ty> {
                        if idx >= #size {
                            None
                        } else {
                            Some(unsafe { self.#unchecked_name(idx)})
                        }
                    }

                    pub unsafe fn #unchecked_name(&self, idx: usize) -> #scalar_ty {
                        let ptr = self.0.add(#offset).cast::<#scalar_ty>();

                        ptr.add(idx).read_volatile()
                    }
                }
            } else {
                let ty = generate_type_ref(&reg.reg_type);
                quote! {
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
            let can_write =
                reg.access == RegisterAccess::WriteOnly || reg.access == RegisterAccess::ReadWrite;

            if !can_write {
                return quote! {};
            }

            let name = ident(format!("set_{}", &reg.name));
            let offset = reg.address as usize;

            if let Type::Vec(len, inner) = &reg.reg_type {
                let unchecked_name = ident(format!("set_{}_unchecked", &reg.name));
                let scalar_ty = generate_type_ref(&inner);
                let size = *len as usize;

                quote! {
                    pub fn #name(&self, idx: usize, val: #scalar_ty) -> Option<()> {
                        if idx >= #size {
                            None
                        } else {
                            unsafe { self.#unchecked_name(idx, val) };
                            Some(())
                        }
                    }

                    pub unsafe fn #unchecked_name(&self, idx: usize, val: #scalar_ty) {
                        let ptr = self.0.add(#offset).cast::<#scalar_ty>();

                        ptr.add(idx).write_volatile(val);
                    }
                }
            } else {
                let ty = generate_type_ref(&reg.reg_type);
                quote! {
                    pub fn #name(&self, val: #ty) -> () {
                        unsafe {
                            self.0.add(#offset).cast::<#ty>().write_volatile(val)
                        }
                    }
                }
            }
        })
        .collect::<Vec<_>>();

    quote! {
        #[doc = #description]
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
