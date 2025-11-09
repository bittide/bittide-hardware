// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Parse the input memory maps into the intermediate representation

use crate::{
    input_language::{self as input, InterconnectComponent, MemoryMapTree, SourceLocation},
    ir::types::{
        DeviceDescription, HalHandles, Identifier, IrCtx, Path, PathComp, RegisterDescription,
        TreeElem, TreeElemType, TypeConstructor, TypeDefinition, TypeDescription, TypeRef,
    },
    storage::{Handle, HandleRange},
    MemoryMapDesc,
};
use std::collections::BTreeMap;

/// Mapping from handles back to the structures in the input language.
///
/// This can be useful when dealing with handles alone is inconvenient, like
/// for performing structural equality for example.
#[derive(Default)]
pub struct IrInputMapping<'input> {
    pub type_refs: BTreeMap<Handle<TypeRef>, &'input input::TypeRef>,
    pub device_descs: BTreeMap<Handle<DeviceDescription>, &'input input::DeviceDesc>,
    pub register_descs: BTreeMap<Handle<RegisterDescription>, &'input input::RegisterDesc>,
    pub type_descs: BTreeMap<Handle<TypeDescription>, &'input input::TypeDescription>,
    pub type_constructors: BTreeMap<Handle<TypeConstructor>, &'input input::Constructor>,
    pub tree_elems: BTreeMap<Handle<TreeElem>, &'input input::MemoryMapTree>,
}

impl IrCtx {
    /// Translate a memory map in the input format into the IR.
    ///
    /// Creates mappings ([IrInputMapping]) and outputs handles for the memory map.
    pub fn add_memory_map_desc<'input>(
        &mut self,
        mapping: &mut IrInputMapping<'input>,
        desc: &'input MemoryMapDesc,
    ) -> HalHandles {
        let tree_type_range_counter = self.tree_elems.range_counter();
        let tree = self.add_toplevel_tree(mapping, desc, &desc.tree);
        let tree_elem_range = tree_type_range_counter.finish(&self.tree_elems);

        let device_reg_types_range_counter = self.type_refs.range_counter();
        let mut device_range_builder = HandleRange::build();
        for dev in desc.devices.values() {
            device_range_builder.add(self.add_device_description(mapping, desc, dev));
        }
        let device_range = device_range_builder.finish();
        let device_reg_types_range = device_reg_types_range_counter.finish(&self.type_refs);

        let mut type_desc_range_builder = HandleRange::build();
        for ty in desc.types.values() {
            type_desc_range_builder.add(self.add_type_description(mapping, ty));
        }
        let type_desc_range = type_desc_range_builder.finish();

        HalHandles {
            tree,
            devices: device_range,
            types: type_desc_range,

            tree_elem_range,
            device_reg_types_range,
        }
    }

    fn add_type_description<'input>(
        &mut self,
        mapping: &mut IrInputMapping<'input>,
        ty_desc: &'input input::TypeDescription,
    ) -> Handle<TypeDescription> {
        let name = self.type_names.push(ty_desc.name.clone());
        let mut args_builder = HandleRange::build();

        for arg in &ty_desc.type_args {
            match arg {
                input::TypeArg::Type { name } => {
                    let ident = self.identifiers.push(name.clone());
                    self.type_param_types.insert(ident);
                    args_builder.add(ident);
                }
                input::TypeArg::Number { name } => {
                    let ident = self.identifiers.push(name.clone());
                    self.type_param_nats.insert(ident);
                    args_builder.add(ident);
                }
            }
        }
        let args = args_builder.finish();

        let type_ref_range = self.type_refs.range_counter();

        let definition = match &ty_desc.definition {
            input::TypeDefinition::DataType(named_constructors) => {
                let names = self
                    .identifiers
                    .push_range(named_constructors.iter().map(|con| con.0.clone()));
                let mut to_do_and_patch = Vec::new();
                let mut con_range_builder = HandleRange::build();
                for con in named_constructors {
                    con_range_builder.add(self.add_constructor(
                        mapping,
                        &con.1,
                        &mut to_do_and_patch,
                    ));
                }
                self.add_and_patch_types(mapping, &mut to_do_and_patch);
                TypeDefinition::DataType {
                    names,
                    constructors: con_range_builder.finish(),
                }
            }
            input::TypeDefinition::Newtype(named_constructor) => {
                let con_name = self.identifiers.push(named_constructor.0.clone());
                let con = self.add_constructor(mapping, &named_constructor.1, &mut vec![]);
                TypeDefinition::Newtype {
                    name: con_name,
                    constructor: con,
                }
            }
            input::TypeDefinition::Builtin(builtin_type) => {
                TypeDefinition::Builtin(builtin_type.clone())
            }
            input::TypeDefinition::Synonym(type_ref) => {
                let type_handle = self.add_toplevel_type_ref(mapping, type_ref);
                TypeDefinition::Synonym(type_handle)
            }
        };

        let handle = self.type_descs.push(TypeDescription {
            name,
            param_names: args,
            definition,
            type_ref_range: type_ref_range.finish(&self.type_refs),
        });

        mapping.type_descs.insert(handle, ty_desc);

        match &ty_desc.definition {
            input::TypeDefinition::Builtin(_) => {
                self.type_primitives.insert(handle);
            }
            input::TypeDefinition::Synonym(_) => {
                self.type_aliases.insert(handle);
            }
            _ if ty_desc.name.module == "GHC.Tuple" => {
                self.type_tuples.insert(handle);
            }
            _ => {}
        }

        handle
    }

    fn add_constructor<'input>(
        &mut self,
        mapping: &mut IrInputMapping<'input>,
        con: &'input input::Constructor,
        to_do_and_patch: &mut Vec<(Handle<TypeRef>, &'input [input::TypeRef])>,
    ) -> Handle<TypeConstructor> {
        let handle = match con {
            input::Constructor::Nameless { fields } => {
                let mut field_builder = HandleRange::build();
                for field in fields {
                    let ty_handle = self.add_type_ref(field, to_do_and_patch);
                    field_builder.add(ty_handle);
                    mapping.type_refs.insert(ty_handle, field);
                }
                let fields = field_builder.finish();
                self.add_and_patch_types(mapping, to_do_and_patch);
                self.type_constructors.push(TypeConstructor {
                    field_types: fields,
                    field_names: None,
                })
            }
            input::Constructor::Record { fields } => {
                let mut field_ty_builder = HandleRange::build();
                let mut field_name_builder = HandleRange::build();
                for (name, field) in fields {
                    field_name_builder.add(self.identifiers.push(name.clone()));
                    let ty_handle = self.add_type_ref(field, to_do_and_patch);
                    mapping.type_refs.insert(ty_handle, field);
                    field_ty_builder.add(ty_handle);
                }
                let field_types = field_ty_builder.finish();
                let field_names = field_name_builder.finish();
                self.add_and_patch_types(mapping, to_do_and_patch);
                self.type_constructors.push(TypeConstructor {
                    field_types,
                    field_names: Some(field_names),
                })
            }
        };

        mapping.type_constructors.insert(handle, con);

        handle
    }

    fn add_and_patch_types<'input>(
        &mut self,
        mappings: &mut IrInputMapping<'input>,
        to_do_and_patch: &mut Vec<(Handle<TypeRef>, &'input [input::TypeRef])>,
    ) {
        loop {
            let Some((patch_handle, to_do)) = to_do_and_patch.pop() else {
                break;
            };

            let mut range = HandleRange::build();

            for ty_ref in to_do {
                let handle = self.add_type_ref(ty_ref, to_do_and_patch);
                range.add(handle);
                mappings.type_refs.insert(handle, ty_ref);
            }

            let range = range.finish();

            match &mut self.type_refs[patch_handle] {
                TypeRef::BitVector(handle)
                | TypeRef::Unsigned(handle)
                | TypeRef::Signed(handle)
                | TypeRef::Index(handle) => *handle = range.handles().next().unwrap(),
                TypeRef::Vector(size, type_ref) => {
                    let mut handles = range.handles();
                    *size = handles.next().unwrap();
                    *type_ref = handles.next().unwrap();
                }
                TypeRef::Tuple(handle_range) => *handle_range = range,
                TypeRef::Reference { name: _, args } => *args = range,
                other => panic!("Shouldn't need patching: {:?}", other),
            }
        }
    }

    fn add_device_description<'input>(
        &mut self,
        mapping: &mut IrInputMapping<'input>,
        desc: &'input MemoryMapDesc,
        dev_desc: &'input input::DeviceDesc,
    ) -> Handle<DeviceDescription> {
        let ident = self.identifiers.push(dev_desc.name.clone());
        let loc = self.add_location_ref(desc, &dev_desc.src_location);
        let tags = self.tags.push_range(dev_desc.tags.iter().cloned());

        let mut range_builder = HandleRange::build();
        let type_ref_start = self.type_refs.range_counter();
        for reg in &dev_desc.registers {
            range_builder.add(self.add_register_desc(mapping, desc, reg));
        }

        let range = range_builder.finish();
        let type_ref_range = type_ref_start.finish(&self.type_refs);

        let handle = self.device_descs.push(DeviceDescription {
            name: ident,
            description: dev_desc.description.clone(),
            registers: range,
            loc,
            tags,
            type_ref_range,
        });

        mapping.device_descs.insert(handle, dev_desc);

        handle
    }

    fn add_register_desc<'input>(
        &mut self,
        mapping: &mut IrInputMapping<'input>,
        desc: &'input MemoryMapDesc,
        reg: &'input input::RegisterDesc,
    ) -> Handle<RegisterDescription> {
        let ident = self.identifiers.push(reg.name.clone());
        let loc = self.add_location_ref(desc, &reg.src_location);
        let tags = self.tags.push_range(reg.tags.iter().cloned());

        let ty = self.add_toplevel_type_ref(mapping, &reg.reg_type);

        let handle = self.registers.push(RegisterDescription {
            name: ident,
            description: reg.description.clone(),
            access: reg.access,
            address: reg.address,
            size: reg.size,
            type_ref: ty,
            loc,
            tags,
        });

        mapping.register_descs.insert(handle, reg);

        handle
    }

    fn add_toplevel_type_ref<'input>(
        &mut self,
        mapping: &mut IrInputMapping<'input>,
        ty: &'input input::TypeRef,
    ) -> Handle<TypeRef> {
        let mut to_do_and_patch = Vec::new();
        let handle = self.add_type_ref(ty, &mut to_do_and_patch);
        mapping.type_refs.insert(handle, ty);

        self.add_and_patch_types(mapping, &mut to_do_and_patch);

        handle
    }

    fn add_type_ref<'input>(
        &mut self,
        ty: &'input input::TypeRef,
        to_do_and_patch: &mut Vec<(Handle<TypeRef>, &'input [input::TypeRef])>,
    ) -> Handle<TypeRef> {
        match ty {
            input::TypeRef::TypeReference { base, args } => {
                match (base.base.as_str(), base.module.as_str()) {
                    ("BitVector", "Clash.Sized.Internal.BitVector") => {
                        let handle = self
                            .type_refs
                            .push(TypeRef::BitVector(Handle::const_handle(0)));

                        to_do_and_patch.push((handle, args));
                        handle
                    }
                    ("Signed", "Clash.Sized.Internal.Signed") => {
                        let handle = self
                            .type_refs
                            .push(TypeRef::Signed(Handle::const_handle(0)));

                        to_do_and_patch.push((handle, args));
                        handle
                    }
                    ("Unsigned", "Clash.Sized.Internal.Unsigned") => {
                        let handle = self
                            .type_refs
                            .push(TypeRef::Unsigned(Handle::const_handle(0)));

                        to_do_and_patch.push((handle, args));
                        handle
                    }
                    ("Index", "Clash.Sized.Internal.Index") => {
                        let handle = self.type_refs.push(TypeRef::Index(Handle::const_handle(0)));

                        to_do_and_patch.push((handle, args));
                        handle
                    }
                    ("Float", "GHC.Types") => self.type_refs.push(TypeRef::Float),
                    ("Double", "GHC.Types") => self.type_refs.push(TypeRef::Double),
                    ("Bool", "GHC.Types") => self.type_refs.push(TypeRef::Bool),
                    ("Vec", "Clash.Sized.Vector") => {
                        let handle = self.type_refs.push(TypeRef::Vector(
                            Handle::const_handle(0),
                            Handle::const_handle(0),
                        ));

                        to_do_and_patch.push((handle, args));
                        handle
                    }
                    ("Unit", "GHC.Tuple") => {
                        self.type_refs.push(TypeRef::Tuple(HandleRange::empty()))
                    }
                    (name, "GHC.Tuple") if name.starts_with("Tuple") => {
                        let handle = self.type_refs.push(TypeRef::Tuple(HandleRange::empty()));

                        to_do_and_patch.push((handle, args));

                        handle
                    }
                    (_, _) => {
                        let name = self.type_names.push(base.clone());
                        let handle = self.type_refs.push(TypeRef::Reference {
                            name,
                            args: HandleRange::empty(),
                        });

                        to_do_and_patch.push((handle, args));

                        handle
                    }
                }
            }
            input::TypeRef::Variable(name) => {
                let ident = self.lookup_latest_identifier(name).unwrap();
                self.type_refs.push(TypeRef::Variable(ident))
            }
            input::TypeRef::Nat(n) => self.type_refs.push(TypeRef::Nat(*n)),
            input::TypeRef::Tuple(type_refs) => {
                let tup_handle = self.type_refs.push(TypeRef::Tuple(HandleRange::empty()));
                to_do_and_patch.push((tup_handle, type_refs));
                tup_handle
            }
        }
    }

    fn lookup_latest_identifier(&self, name: &str) -> Option<Handle<Identifier>> {
        self.identifiers
            .iter()
            .rev()
            .find_map(|(handle, ident)| if ident == name { Some(handle) } else { None })
    }

    fn add_toplevel_tree<'input>(
        &mut self,
        mapping: &mut IrInputMapping<'input>,
        desc: &'input MemoryMapDesc,
        tree: &'input MemoryMapTree,
    ) -> Handle<TreeElem> {
        let mut to_do_and_patch = Vec::new();
        let (top_handle, _top_ty_handle) =
            self.add_tree_elem(mapping, desc, tree, &mut to_do_and_patch);

        loop {
            let Some((handle, to_do)) = to_do_and_patch.pop() else {
                break;
            };

            let mut range = HandleRange::build();

            for thing in to_do {
                let (elem_handle, _elem_ty_handle) =
                    self.add_tree_elem(mapping, desc, &thing.tree, &mut to_do_and_patch);
                range.add(elem_handle);
            }

            let range = range.finish();
            if let TreeElemType::Interconnect { components, .. } = &mut self.tree_elem_types[handle]
            {
                *components = range;
            }
        }

        top_handle
    }

    fn add_tree_elem<'input>(
        &mut self,
        mapping: &mut IrInputMapping<'input>,
        desc: &'input MemoryMapDesc,
        tree: &'input MemoryMapTree,
        to_do_and_patch: &mut Vec<(Handle<TreeElemType>, &'input [InterconnectComponent])>,
    ) -> (Handle<TreeElem>, Handle<TreeElemType>) {
        match tree {
            MemoryMapTree::Interconnect {
                path,
                tags,
                absolute_address,
                components,
                src_location,
            } => {
                let path = self.add_path(desc, path);
                let loc = self.add_location_ref(desc, src_location);

                let tag_locs = self.locations.push_range(
                    tags.iter()
                        .map(|tag| resolve_loc_ref(desc, &tag.src_location).clone()),
                );
                let tags = self.tags.push_range(tags.iter().map(|tag| tag.tag.clone()));

                let rel_addrs = self
                    .interconnect_rel_addrs
                    .push_range(components.iter().map(|comp| comp.relative_address));

                let tree_elem = TreeElem {
                    path,
                    tags,
                    tag_locs,
                    loc,
                    absolute_addr: *absolute_address,
                };

                let tree_elem_type = TreeElemType::Interconnect {
                    rel_addrs,
                    components: HandleRange::empty(),
                };

                let handles @ (elem_handle, elem_ty_handle) =
                    self.add_tree_elem_and_type(tree_elem, tree_elem_type);

                to_do_and_patch.push((elem_ty_handle, components.as_slice()));

                mapping.tree_elems.insert(elem_handle, tree);

                handles
            }
            MemoryMapTree::DeviceInstance {
                path,
                tags,
                device_name,
                src_location,
                absolute_address,
            } => {
                let path = self.add_path(desc, path);
                let loc = self.add_location_ref(desc, src_location);

                let tag_locs = self.locations.push_range(
                    tags.iter()
                        .map(|tag| resolve_loc_ref(desc, &tag.src_location).clone()),
                );
                let tags = self.tags.push_range(tags.iter().map(|tag| tag.tag.clone()));

                let tree_elem = TreeElem {
                    path,
                    tags,
                    tag_locs,
                    loc,
                    absolute_addr: *absolute_address,
                };

                let device_name = self.identifiers.push(device_name.clone());

                let tree_elem_ty = TreeElemType::DeviceInstance { device_name };

                let handles @ (elem_handle, _elem_ty_handle) =
                    self.add_tree_elem_and_type(tree_elem, tree_elem_ty);

                mapping.tree_elems.insert(elem_handle, tree);

                handles
            }
        }
    }

    fn add_tree_elem_and_type(
        &mut self,
        elem: TreeElem,
        ty: TreeElemType,
    ) -> (Handle<TreeElem>, Handle<TreeElemType>) {
        debug_assert_eq!(self.tree_elems.len(), self.tree_elem_types.len());
        let elem = self.tree_elems.push(elem);
        let ty = self.tree_elem_types.push(ty);
        (elem, ty)
    }

    fn add_path(&mut self, desc: &MemoryMapDesc, path: &input::Path) -> Handle<Path> {
        let mut new_path = Path::with_capacity(path.len());

        for comp in path {
            match comp {
                input::PathComp::Name { src_location, name } => {
                    let loc = self.add_location_ref(desc, src_location);
                    new_path.push(PathComp::Named {
                        loc,
                        name: name.clone(),
                    })
                }
                input::PathComp::Unnamed(n) => {
                    new_path.push(PathComp::Unnamed(*n));
                }
            }
        }

        self.paths.push(new_path)
    }

    fn add_location_ref(
        &mut self,
        desc: &MemoryMapDesc,
        loc: &input::LocationRef,
    ) -> Handle<SourceLocation> {
        self.locations.push(resolve_loc_ref(desc, loc).clone())
    }
}

fn resolve_loc_ref<'input>(
    desc: &'input MemoryMapDesc,
    loc_ref: &'input input::LocationRef,
) -> &'input SourceLocation {
    match loc_ref {
        input::LocationRef::Inline(source_location) => source_location,
        input::LocationRef::Separate(i) => &desc.src_locations.as_ref().unwrap()[*i as usize],
    }
}
