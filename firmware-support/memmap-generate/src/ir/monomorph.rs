// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Monomorphization pass for memory maps.
//!
//! Not all languages targetting for HAL generation support all type
//! level features that Clash supports, namely generics and type-level naturals.
//! Monomorphization can special case type arguments based on use-sites into
//! concrete types with type-arguments substituted in-place.

use std::collections::BTreeMap;

use smallvec::SmallVec;

use crate::{
    input_language as input,
    ir::{
        input_to_ir::IrInputMapping,
        types::{IrCtx, TypeDescription, TypeName, TypeRef},
    },
    storage::{Handle, HandleRange, Storage},
};

/// Whether a type level argument should be monomorphized or not.
#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Debug)]
pub enum ArgMonomorphState {
    Monomorph,
    NoMonomorph,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
enum MonomorphTypeRefResult<'ir> {
    TypeRefVariant(Handle<TypeRefVariant>),
    TupleVariant(Handle<TupleVariant>),
    RawArg(&'ir input::TypeRef),
}

/// A pass describing what elements should be monomorphized or not.
pub trait MonomorphPass {
    fn custom_type(&mut self, ctx: &IrCtx, desc: &TypeDescription, args: &mut [ArgMonomorphState]);
    fn monomorph_tuples(&mut self) -> bool;
}

/// Storages for variant information.
///
/// Tracks monomorph variants externally to the [IrCtx].
#[derive(Default, Debug)]
pub struct MonomorphVariants {
    pub type_ref_variants: Storage<TypeRefVariant>,
    pub tuple_variant: Storage<TupleVariant>,

    pub variants_by_type: BTreeMap<Handle<TypeName>, Vec<Handle<TypeRefVariant>>>,

    pub type_refs: BTreeMap<Handle<TypeRef>, Handle<TypeRefVariant>>,
    pub tuples: BTreeMap<Handle<TypeRef>, Handle<TupleVariant>>,
}

enum TypeDescMonomorphMode<'a> {
    TopLevel,
    RecurseMonomorph {
        variable_subs: BTreeMap<Handle<TypeRef>, Handle<TypeRef>>,
        mono_type_ref_subs: &'a mut BTreeMap<Handle<TypeRef>, Handle<TypeRefVariant>>,
        mono_tuple_subs: &'a mut BTreeMap<Handle<TypeRef>, Handle<TupleVariant>>,
    },
}

#[derive(Debug, Clone)]
pub struct TypeRefVariant {
    pub original_type_desc: Handle<TypeDescription>,
    pub variable_substitutions: BTreeMap<Handle<TypeRef>, Handle<TypeRef>>,
    pub monomorph_type_ref_substitutions: BTreeMap<Handle<TypeRef>, Handle<TypeRefVariant>>,
    pub monomorph_tuple_substitutions: BTreeMap<Handle<TypeRef>, Handle<TupleVariant>>,
    pub argument_mono_values: SmallVec<[Option<Handle<TypeRef>>; 2]>,
}

#[derive(Debug, Clone)]
pub struct TupleVariant {
    pub elements: SmallVec<[Handle<TypeRef>; 2]>,
}

/// Monomorpher that keeps track of which arguments have been encountered yet.
pub struct Monomorpher<'ir> {
    ctx: &'ir IrCtx,
    mapping: &'ir IrInputMapping<'ir>,
    #[allow(clippy::type_complexity)]
    variants_by_args: BTreeMap<
        (
            Handle<TypeName>,
            SmallVec<[Option<MonomorphTypeRefResult<'ir>>; 2]>,
        ),
        Handle<TypeRefVariant>,
    >,
    tuples_by_elems: BTreeMap<SmallVec<[MonomorphTypeRefResult<'ir>; 2]>, Handle<TupleVariant>>,
    number_primitives: BTreeMap<Handle<TypeRef>, Handle<TypeRef>>,
    tuples: Vec<Handle<TypeRef>>, // hmm no idea how to deal with this :melting_face:
    vectors: Vec<Handle<TypeRef>>,
}

impl<'ir> Monomorpher<'ir> {
    pub fn new(ctx: &'ir IrCtx, mapping: &'ir IrInputMapping<'ir>) -> Self {
        Self {
            ctx,
            mapping,
            variants_by_args: BTreeMap::new(),
            tuples_by_elems: BTreeMap::new(),
            number_primitives: BTreeMap::new(),
            tuples: vec![],
            vectors: vec![],
        }
    }

    /// Add top level type references.
    ///
    /// Top-level type references do not reference variables and are the root
    /// for the discovery of all use-sites.
    pub fn monomorph_toplevel_type_refs(
        &mut self,
        variants: &mut MonomorphVariants,
        pass: &mut impl MonomorphPass,
        handles: impl DoubleEndedIterator<Item = Handle<TypeRef>>,
    ) {
        // Arguments to types are stored after that type-with-arguments.
        // If we flip the iterator we will process arguments first, reducing
        // lookups that were done already.
        for handle in handles.rev() {
            self.monomorph_type_ref(variants, pass, &mut TypeDescMonomorphMode::TopLevel, handle);
        }
    }

    /// Monomorph type descriptions
    ///
    /// Aside from top-level type references, type descriptions
    /// might also need to be monomorphized, as they can reference
    /// other types that might need to be monomorphized.
    pub fn monomorph_type_descs(
        &mut self,
        varis: &mut MonomorphVariants,
        pass: &mut impl MonomorphPass,
    ) {
        let variants_start = varis.type_ref_variants.range_counter();
        for var_handle in varis.type_ref_variants.whole_range().handles() {
            let var = &varis.type_ref_variants[var_handle];
            let desc_handle = var.original_type_desc;
            let desc = &self.ctx.type_descs[desc_handle];
            if let Some(variants) = varis.variants_by_type.get(&desc.name).cloned() {
                for variant_handle in variants {
                    let variant = &varis.type_ref_variants[variant_handle];

                    let mut mono_type_ref_subs = BTreeMap::new();
                    let mut mono_tuple_subs = BTreeMap::new();
                    let mut mode = TypeDescMonomorphMode::RecurseMonomorph {
                        variable_subs: variant.variable_substitutions.clone(),
                        mono_type_ref_subs: &mut mono_type_ref_subs,
                        mono_tuple_subs: &mut mono_tuple_subs,
                    };
                    self.monomorph_type_definition(varis, pass, &mut mode, desc_handle);
                    varis.type_ref_variants[variant_handle].monomorph_type_ref_substitutions =
                        mono_type_ref_subs;
                    varis.type_ref_variants[variant_handle].monomorph_tuple_substitutions =
                        mono_tuple_subs;
                }
            } else {
                panic!(
                    "there should not be type descriptions without a mono variant being generated"
                );
            }
        }
        let mut range = variants_start.finish(&varis.type_ref_variants);

        loop {
            let variants_start = varis.type_ref_variants.range_counter();

            for var in range.handles() {
                let variant = &varis.type_ref_variants[var];

                let mut mono_type_ref_subs = BTreeMap::new();
                let mut mono_tuple_subs = BTreeMap::new();
                let mut mode = TypeDescMonomorphMode::RecurseMonomorph {
                    variable_subs: variant.variable_substitutions.clone(),
                    mono_type_ref_subs: &mut mono_type_ref_subs,
                    mono_tuple_subs: &mut mono_tuple_subs,
                };
                self.monomorph_type_definition(varis, pass, &mut mode, variant.original_type_desc);
                varis.type_ref_variants[var].monomorph_type_ref_substitutions = mono_type_ref_subs;
                varis.type_ref_variants[var].monomorph_tuple_substitutions = mono_tuple_subs;
            }

            range = variants_start.finish(&varis.type_ref_variants);

            if range.len == 0 {
                break;
            }
        }
    }

    fn monomorph_type_definition(
        &mut self,
        varis: &mut MonomorphVariants,
        pass: &mut impl MonomorphPass,
        type_desc_mode: &mut TypeDescMonomorphMode,
        desc_handle: Handle<TypeDescription>,
    ) {
        let desc = &self.ctx.type_descs[desc_handle];
        match &desc.definition {
            super::types::TypeDefinition::DataType {
                names: _,
                constructors,
            } => {
                for con in &self.ctx.type_constructors[*constructors] {
                    for ty in con.field_types.handles() {
                        self.monomorph_type_ref(varis, pass, type_desc_mode, ty);
                    }
                }
            }
            super::types::TypeDefinition::Newtype {
                name: _,
                constructor,
            } => {
                for ty in self.ctx.type_constructors[*constructor]
                    .field_types
                    .handles()
                {
                    self.monomorph_type_ref(varis, pass, type_desc_mode, ty);
                }
            }
            super::types::TypeDefinition::Builtin(_builtin_type) => {}
            super::types::TypeDefinition::Synonym(handle) => {
                self.monomorph_type_ref(varis, pass, type_desc_mode, *handle);
            }
        }
    }

    fn monomorph_type_ref(
        &mut self,
        varis: &mut MonomorphVariants,
        pass: &mut impl MonomorphPass,
        type_desc_mode: &mut TypeDescMonomorphMode,
        handle: Handle<TypeRef>,
    ) -> MonomorphTypeRefResult<'ir> {
        let handle = match type_desc_mode {
            TypeDescMonomorphMode::TopLevel => handle,
            TypeDescMonomorphMode::RecurseMonomorph {
                variable_subs,
                mono_type_ref_subs: _,
                mono_tuple_subs: _,
            } => variable_subs.get(&handle).copied().unwrap_or(handle),
        };

        match &self.ctx.type_refs[handle] {
            TypeRef::Reference { name, args } => {
                let mut arg_result_buf = SmallVec::<[_; 2]>::with_capacity(args.len);
                let mut arg_state = SmallVec::<[_; 2]>::with_capacity(args.len);

                for arg in args.handles() {
                    arg_result_buf.push(self.monomorph_type_ref(varis, pass, type_desc_mode, arg));
                    arg_state.push(ArgMonomorphState::Monomorph);
                }
                let (desc_handle, desc) = self.lookup_type_desc(*name);

                pass.custom_type(self.ctx, desc, &mut arg_state);

                let subs = args
                    .handles()
                    .zip(arg_state.into_iter().zip(arg_result_buf));

                let variant =
                    self.instantiate_type(varis, handle, *name, desc_handle, type_desc_mode, subs);
                MonomorphTypeRefResult::TypeRefVariant(variant)
            }
            TypeRef::BitVector(arg_handle)
            | TypeRef::Unsigned(arg_handle)
            | TypeRef::Signed(arg_handle)
            | TypeRef::Index(arg_handle) => {
                // shouldn't be needed to visit the args, they can only be
                // hardcoded numbers or variables
                self.number_primitives.insert(handle, *arg_handle);
                MonomorphTypeRefResult::RawArg(self.mapping.type_refs[&handle])
            }
            TypeRef::Vector(_num, type_handle) => {
                self.monomorph_type_ref(varis, pass, type_desc_mode, *type_handle);
                self.vectors.push(handle);
                MonomorphTypeRefResult::RawArg(self.mapping.type_refs[&handle])
            }
            TypeRef::Tuple(handle_range) => {
                let mut element_refs = SmallVec::<[_; 2]>::new();

                for handle in handle_range.handles() {
                    element_refs.push(self.monomorph_type_ref(varis, pass, type_desc_mode, handle));
                }
                self.tuples.push(handle);
                if pass.monomorph_tuples() {
                    MonomorphTypeRefResult::TupleVariant(self.instantiate_tuple(
                        varis,
                        type_desc_mode,
                        handle,
                        *handle_range,
                        element_refs,
                    ))
                } else {
                    MonomorphTypeRefResult::RawArg(self.mapping.type_refs[&handle])
                }
            }
            TypeRef::Variable(_)
            | TypeRef::Bool
            | TypeRef::Float
            | TypeRef::Double
            | TypeRef::Nat(_) => MonomorphTypeRefResult::RawArg(self.mapping.type_refs[&handle]),
        }
    }

    fn lookup_type_desc(
        &self,
        name: Handle<TypeName>,
    ) -> (Handle<TypeDescription>, &'ir TypeDescription) {
        // TODO this could be better than a linear search..
        self.ctx
            .type_descs
            .iter()
            .find(|(_handle, desc)| desc.name == name)
            .unwrap()
    }

    fn instantiate_tuple(
        &mut self,
        varis: &mut MonomorphVariants,
        type_desc_mode: &mut TypeDescMonomorphMode,
        tuple_handle: Handle<TypeRef>,
        elements: HandleRange<TypeRef>,
        element_results: SmallVec<[MonomorphTypeRefResult<'ir>; 2]>,
    ) -> Handle<TupleVariant> {
        if let Some(var) = self.tuples_by_elems.get(&element_results) {
            match type_desc_mode {
                TypeDescMonomorphMode::TopLevel => {
                    varis.tuples.insert(tuple_handle, *var);
                }
                TypeDescMonomorphMode::RecurseMonomorph {
                    variable_subs: _,
                    mono_type_ref_subs: _,
                    mono_tuple_subs,
                } => {
                    mono_tuple_subs.insert(tuple_handle, *var);
                }
            }
            return *var;
        }

        let mut resolved_elems = SmallVec::new();

        for handle in elements.handles() {
            let resolved_handle = match type_desc_mode {
                TypeDescMonomorphMode::TopLevel => handle,
                TypeDescMonomorphMode::RecurseMonomorph {
                    variable_subs,
                    mono_type_ref_subs: _,
                    mono_tuple_subs: _,
                } => variable_subs.get(&handle).copied().unwrap_or(handle),
            };
            resolved_elems.push(resolved_handle);
        }

        let tuple_var = TupleVariant {
            elements: resolved_elems,
        };
        let handle = varis.tuple_variant.push(tuple_var);
        self.tuples_by_elems.insert(element_results, handle);

        match type_desc_mode {
            TypeDescMonomorphMode::TopLevel => {
                varis.tuples.insert(tuple_handle, handle);
            }
            TypeDescMonomorphMode::RecurseMonomorph {
                variable_subs: _,
                mono_type_ref_subs: _,
                mono_tuple_subs,
            } => {
                mono_tuple_subs.insert(tuple_handle, handle);
            }
        }

        handle
    }

    fn instantiate_type(
        &mut self,
        varis: &mut MonomorphVariants,
        type_handle: Handle<TypeRef>,
        type_name: Handle<TypeName>,
        desc_handle: Handle<TypeDescription>,
        type_desc_mode: &mut TypeDescMonomorphMode,
        variable_subs: impl Iterator<
            Item = (
                Handle<TypeRef>,
                (ArgMonomorphState, MonomorphTypeRefResult<'ir>),
            ),
        >,
    ) -> Handle<TypeRefVariant> {
        let desc = &self.ctx.type_descs[desc_handle];
        let with_names = desc.param_names.handles().zip(variable_subs);

        let mut memo_args = SmallVec::new();
        let mut arg_subs = SmallVec::new();
        let mut var_subs = BTreeMap::new();
        for (name, (handle, (state, res))) in with_names {
            if state == ArgMonomorphState::NoMonomorph {
                memo_args.push(None);
                arg_subs.push(None);
            } else {
                memo_args.push(Some(res));
                arg_subs.push(Some(handle));
                var_subs.insert(name, handle);
            }
        }

        if let Some(var) = self.variants_by_args.get(&(type_name, memo_args.clone())) {
            match type_desc_mode {
                TypeDescMonomorphMode::TopLevel => {
                    varis.type_refs.insert(type_handle, *var);
                }
                TypeDescMonomorphMode::RecurseMonomorph {
                    variable_subs: _,
                    mono_type_ref_subs,
                    mono_tuple_subs: _,
                } => {
                    mono_type_ref_subs.insert(type_handle, *var);
                }
            }
            return *var;
        }

        let mut subs = BTreeMap::new();

        for ty_ref_handle in desc.type_ref_range.handles() {
            if let TypeRef::Variable(n) = &self.ctx.type_refs[ty_ref_handle] {
                if let Some(sub) = var_subs.get(n) {
                    subs.insert(ty_ref_handle, *sub);
                }
            }
        }

        // no variant created for this yet
        let variant = TypeRefVariant {
            original_type_desc: desc_handle,
            variable_substitutions: subs,
            monomorph_type_ref_substitutions: Default::default(),
            monomorph_tuple_substitutions: Default::default(),
            argument_mono_values: arg_subs,
        };

        let variant_handle = varis.type_ref_variants.push(variant);
        self.variants_by_args
            .insert((type_name, memo_args), variant_handle);

        varis
            .variants_by_type
            .entry(type_name)
            .or_default()
            .push(variant_handle);

        match type_desc_mode {
            TypeDescMonomorphMode::TopLevel => {
                varis.type_refs.insert(type_handle, variant_handle);
            }
            TypeDescMonomorphMode::RecurseMonomorph {
                variable_subs: _,
                mono_type_ref_subs,
                mono_tuple_subs: _,
            } => {
                mono_type_ref_subs.insert(type_handle, variant_handle);
            }
        }

        variant_handle
    }
}

/// Example monomorphization passes
pub mod passes {
    use crate::ir::monomorph::MonomorphPass;

    /// Monomorphize only type-level naturals, leaving type arguments as they are.
    pub struct OnlyNats;

    impl MonomorphPass for OnlyNats {
        fn custom_type(
            &mut self,
            ctx: &crate::ir::types::IrCtx,
            desc: &crate::ir::types::TypeDescription,
            args: &mut [super::ArgMonomorphState],
        ) {
            for (name, state) in desc.param_names.handles().zip(args.iter_mut()) {
                if ctx.type_param_nats.contains(&name) {
                    *state = super::ArgMonomorphState::Monomorph;
                } else {
                    *state = super::ArgMonomorphState::NoMonomorph;
                }
            }
        }

        fn monomorph_tuples(&mut self) -> bool {
            false
        }
    }

    /// Monomorphize type-level naturals and type arguments.
    pub struct All;

    impl MonomorphPass for All {
        fn custom_type(
            &mut self,
            _ctx: &crate::ir::types::IrCtx,
            _desc: &crate::ir::types::TypeDescription,
            args: &mut [super::ArgMonomorphState],
        ) {
            for arg in args {
                *arg = super::ArgMonomorphState::Monomorph;
            }
        }

        fn monomorph_tuples(&mut self) -> bool {
            true
        }
    }

    /// Don't monomorphize anything.
    pub struct Nothing;

    impl MonomorphPass for Nothing {
        fn custom_type(
            &mut self,
            _ctx: &crate::ir::types::IrCtx,
            _desc: &crate::ir::types::TypeDescription,
            args: &mut [super::ArgMonomorphState],
        ) {
            for arg in args {
                *arg = super::ArgMonomorphState::NoMonomorph;
            }
        }

        fn monomorph_tuples(&mut self) -> bool {
            false
        }
    }
}
