// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Intermediate Representation for memory maps
//!
//! Contains the "context" (all storages) and associated types.

use std::collections::HashSet;

use crate::storage::{Handle, HandleRange, Storage};

pub use crate::input_language::{BuiltinType, RegisterAccess, SourceLocation, TypeName};

/// IR context - flat storages for all types needed to represent a memory map.
#[derive(Default)]
pub struct IrCtx {
    pub locations: Storage<SourceLocation>,
    pub paths: Storage<Path>,
    pub tags: Storage<Tag>,

    pub tree_elems: Storage<TreeElem>,
    /// Type of tree elements, elements are synchronized to [IrCtx::tree_elems].
    pub tree_elem_types: Storage<TreeElemType>,
    pub interconnect_rel_addrs: Storage<RelativeAddr>,

    pub type_names: Storage<TypeName>,
    pub type_refs: Storage<TypeRef>,
    pub type_descs: Storage<TypeDescription>,
    pub type_constructors: Storage<TypeConstructor>,
    pub type_primitives: HashSet<Handle<TypeDescription>>,
    pub type_aliases: HashSet<Handle<TypeDescription>>,
    pub type_tuples: HashSet<Handle<TypeDescription>>,

    pub type_param_types: HashSet<Handle<Identifier>>,
    pub type_param_nats: HashSet<Handle<Identifier>>,

    pub identifiers: Storage<Identifier>,

    pub device_descs: Storage<DeviceDescription>,
    pub registers: Storage<RegisterDescription>,
}

impl IrCtx {
    pub fn new() -> Self {
        Default::default()
    }
}

/// Handles to refer to elements from one memory map / HAL
#[derive(Debug, Clone, Copy)]
pub struct HalHandles {
    pub tree: Handle<TreeElem>,
    pub devices: HandleRange<DeviceDescription>,
    pub types: HandleRange<TypeDescription>,

    pub tree_elem_range: HandleRange<TreeElem>,
    pub device_reg_types_range: HandleRange<TypeRef>,
}

pub type Identifier = String;
pub type Tag = String;

/// Reference to a type, already special casing primitives.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum TypeRef {
    BitVector(Handle<TypeRef>),
    Unsigned(Handle<TypeRef>),
    Signed(Handle<TypeRef>),
    Index(Handle<TypeRef>),
    Bool,
    Float,
    Double,
    Vector(Handle<TypeRef>, Handle<TypeRef>),
    Tuple(HandleRange<TypeRef>),
    Variable(Handle<Identifier>),
    Nat(u64),
    Reference {
        name: Handle<TypeName>,
        args: HandleRange<TypeRef>,
    },
}

#[derive(Debug)]
pub struct TypeDescription {
    pub name: Handle<TypeName>,
    pub param_names: HandleRange<Identifier>,
    pub definition: TypeDefinition,
    pub type_ref_range: HandleRange<TypeRef>,
}

#[derive(Debug)]
pub enum TypeDefinition {
    DataType {
        names: HandleRange<Identifier>,
        constructors: HandleRange<TypeConstructor>,
    },
    Newtype {
        name: Handle<Identifier>,
        constructor: Handle<TypeConstructor>,
    },
    Builtin(BuiltinType),
    Synonym(Handle<TypeRef>),
}

#[derive(Debug)]
pub struct TypeConstructor {
    pub field_types: HandleRange<TypeRef>,
    pub field_names: Option<HandleRange<Identifier>>,
}

#[derive(Debug)]
pub struct DeviceDescription {
    pub name: Handle<Identifier>,
    pub description: String,
    pub registers: HandleRange<RegisterDescription>,
    pub loc: Handle<SourceLocation>,
    pub tags: HandleRange<Tag>,
    pub type_ref_range: HandleRange<TypeRef>,
}

#[derive(Debug)]
pub struct RegisterDescription {
    pub name: Handle<Identifier>,
    pub description: String,
    pub access: RegisterAccess,
    pub address: u64,
    pub size: u64,
    pub type_ref: Handle<TypeRef>,
    pub loc: Handle<SourceLocation>,
    pub tags: HandleRange<Tag>,
}

#[derive(Debug)]
pub struct TreeElem {
    pub path: Handle<Path>,
    pub tags: HandleRange<Tag>,
    pub tag_locs: HandleRange<SourceLocation>,
    pub loc: Handle<SourceLocation>,
    pub absolute_addr: u64,
}

#[derive(Debug)]
pub enum TreeElemType {
    DeviceInstance {
        device_name: Handle<Identifier>,
    },
    Interconnect {
        rel_addrs: HandleRange<RelativeAddr>,
        components: HandleRange<TreeElem>,
    },
}

pub type RelativeAddr = u64;

pub type Path = Vec<PathComp>;

#[derive(Debug)]
pub enum PathComp {
    Named {
        loc: Handle<SourceLocation>,
        name: String,
    },
    Unnamed(u64),
}
