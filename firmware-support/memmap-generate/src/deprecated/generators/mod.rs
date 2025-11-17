// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::collections::{BTreeMap, BTreeSet};

use device_instances::generate_device_instances_struct;
use device_types::DeviceGenerator;
use heck::{ToPascalCase, ToShoutySnakeCase, ToSnakeCase};
use proc_macro2::{Ident, Span, TokenStream};
use types::TypeGenerator;

use crate::{
    deprecated::generators::types::PrimitiveType,
    deprecated::hal_set::{
        DeviceDescAnnotations, DeviceInstance, DeviceInstanceAnnotations, TypeDefAnnotations,
    },
    input_language::{DeviceDesc, TypeDescription, TypeName},
};

pub mod device_instances;
pub mod device_types;
pub mod types;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IdentType {
    Type,
    TypeVariable,
    Device,
    Instance,
    Module,
    Method,
    Variable,
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
        IdentType::Raw => s.to_string(),
    };

    Ident::new(&s, Span::call_site())
}

pub struct ItemReferences {
    pub custom_types: BTreeSet<TypeName>,
    pub primitives: BTreeSet<PrimitiveType>,
}

#[derive(Default)]
pub struct HalGenerator {
    ty_gen: TypeGenerator,
    device_gen: DeviceGenerator,
}

impl HalGenerator {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn generate_type_description(
        &mut self,
        ann: &TypeDefAnnotations,
        ty_desc: &TypeDescription,
    ) -> (ItemReferences, TokenStream) {
        self.clear();
        let desc = self.ty_gen.generate_type_desc(ann, ty_desc);
        let item_refs = ItemReferences {
            custom_types: self.ty_gen.referenced_custom_types().clone(),
            primitives: self.ty_gen.referenced_primitives().clone(),
        };
        (item_refs, desc)
    }

    pub fn generate_device_description(
        &mut self,
        ann: &DeviceDescAnnotations,
        dev_desc: &DeviceDesc,
    ) -> (ItemReferences, TokenStream) {
        self.clear();
        let desc = self
            .device_gen
            .generate_device_type(ann, dev_desc, &mut self.ty_gen);
        let item_refs = ItemReferences {
            custom_types: self.ty_gen.referenced_custom_types().clone(),
            primitives: self.ty_gen.referenced_primitives().clone(),
        };
        (item_refs, desc)
    }

    pub fn generate_instance_struct(
        &mut self,
        instances: &BTreeMap<String, Vec<(DeviceInstanceAnnotations, DeviceInstance)>>,
    ) -> TokenStream {
        generate_device_instances_struct(instances)
    }

    fn clear(&mut self) {
        self.device_gen.clear();
        self.ty_gen.clear();
    }
}

pub(crate) fn generate_tag_docs<'a>(
    tags: impl Iterator<Item = &'a str>,
    ann_tags: impl Iterator<Item = &'a str>,
) -> TokenStream {
    let mut tags = tags.peekable();
    let mut ann_tags = ann_tags.peekable();
    if tags.peek().is_none() && ann_tags.peek().is_none() {
        return TokenStream::new();
    }

    let mut toks = TokenStream::new();

    toks.extend(quote::quote! { #[doc = ""] });
    toks.extend(quote::quote! { #[doc = "## Tags"]});
    toks.extend(quote::quote! { #[doc = ""] });

    for tag in tags {
        let msg = format!(" - `{}`", tag);
        toks.extend(quote::quote! { #[doc = #msg] });
    }

    for tag in ann_tags {
        let msg = format!(" - `{}`", tag);
        toks.extend(quote::quote! { #[doc = #msg] });
    }

    toks
}
