// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
//! Helper macros for defining and implementing traits for numeric types

macro_rules! newtype_copyable_impl {
    (
        @noassign
        type: [
            name: $tyname:ident,
            outerparams: [$($($oparams:tt)+)?],
            appliedparams: [$($($aparams:tt)+)?],
            where: [$($($where:tt)+)?],
        ],
        $newtrait:ident::$newfn:ident = $oldtrait:ident::$oldfn:ident
    ) => {
        subst_macros::repeat_parallel_subst! {
            groups: [
                [group
                    [sub [GEN] = [$(<$($oparams)+>)?]]
                    [sub [LHSTY] = [$tyname$(<$($aparams)+>)?]]
                    [sub [RHSTY] = [$tyname$(<$($aparams)+>)?]]
                    [sub [SELFPRE] = []]
                    [sub [RHSPRE] = []]
                    [sub [NEWTRAIT] = [$newtrait]]
                ]
                [group
                    [sub [GEN] = [<'a$(, $($oparams)+)?>]]
                    [sub [LHSTY] = [$tyname$(<$($aparams)+>)?]]
                    [sub [RHSTY] = [&'a $tyname$(<$($aparams)+>)?]]
                    [sub [SELFPRE] = []]
                    [sub [RHSPRE] = [&]]
                    [sub [NEWTRAIT] = [$newtrait]]
                ]
                [group
                    [sub [GEN] = [<'a$(, $($oparams)+)?>]]
                    [sub [LHSTY] = [&'a $tyname$(<$($aparams)+>)?]]
                    [sub [RHSTY] = [$tyname$(<$($aparams)+>)?]]
                    [sub [SELFPRE] = [&]]
                    [sub [RHSPRE] = []]
                    [sub [NEWTRAIT] = [$newtrait]]
                ]
                [group
                    [sub [GEN] = [<'a, 'b$(, $($oparams)+)?>]]
                    [sub [LHSTY] = [&'a $tyname$(<$($aparams)+>)?]]
                    [sub [RHSTY] = [&'b $tyname$(<$($aparams)+>)?]]
                    [sub [SELFPRE] = [&]]
                    [sub [RHSPRE] = [&]]
                    [sub [NEWTRAIT] = [$newtrait]]
                ]
            ],
            callback: NONE,
            in: {
                impl GEN $oldtrait<RHSTY> for LHSTY $(
                where
                    $($where)+
                )? {
                    type Output = $tyname$(<$($aparams)+>)?;

                    #[allow(unused_unsafe)]
                    fn $oldfn(self, rhs: RHSTY) -> Self::Output {
                        let result = (SELFPRE self.0).$newfn(RHSPRE rhs.0);
                        let result = if check_bounds(&result) {
                            result
                        } else {
                            apply_mask(result)
                        };
                        unsafe { $tyname::new_unchecked(result) }
                    }
                }
            }
        }
    };
    (
        @noassign @strict
        type: [
            name: $tyname:ident,
            outerparams: [$($($oparams:tt)+)?],
            appliedparams: [$($($aparams:tt)+)?],
            where: [$($($where:tt)+)?],
        ],
        $newtrait:ident::$newfn:ident = $oldtrait:ident::$oldfn:ident
    ) => {
        subst_macros::repeat_parallel_subst! {
            groups: [
                [group
                    [sub [GEN] = [$(<$($oparams)+>)?]]
                    [sub [LHSTY] = [$tyname$(<$($aparams)+>)?]]
                    [sub [RHSTY] = [$tyname$(<$($aparams)+>)?]]
                    [sub [SELFPRE] = []]
                    [sub [RHSPRE] = []]
                    [sub [NEWTRAIT] = [$newtrait]]
                ]
                [group
                    [sub [GEN] = [<'a$(, $($oparams)+)?>]]
                    [sub [LHSTY] = [$tyname$(<$($aparams)+>)?]]
                    [sub [RHSTY] = [&'a $tyname$(<$($aparams)+>)?]]
                    [sub [SELFPRE] = []]
                    [sub [RHSPRE] = [&]]
                    [sub [NEWTRAIT] = [$newtrait]]
                ]
                [group
                    [sub [GEN] = [<'a$(, $($oparams)+)?>]]
                    [sub [LHSTY] = [&'a $tyname$(<$($aparams)+>)?]]
                    [sub [RHSTY] = [$tyname$(<$($aparams)+>)?]]
                    [sub [SELFPRE] = [&]]
                    [sub [RHSPRE] = []]
                    [sub [NEWTRAIT] = [$newtrait]]
                ]
                [group
                    [sub [GEN] = [<'a, 'b$(, $($oparams)+)?>]]
                    [sub [LHSTY] = [&'a $tyname$(<$($aparams)+>)?]]
                    [sub [RHSTY] = [&'b $tyname$(<$($aparams)+>)?]]
                    [sub [SELFPRE] = [&]]
                    [sub [RHSPRE] = [&]]
                    [sub [NEWTRAIT] = [$newtrait]]
                ]
            ],
            callback: NONE,
            in: {
                impl GEN $oldtrait<RHSTY> for LHSTY $(
                where
                    $($where)+
                )? {
                    type Output = $tyname$(<$($aparams)+>)?;

                    #[allow(unused_unsafe)]
                    fn $oldfn(self, rhs: RHSTY) -> Self::Output {
                        unsafe {
                            $tyname::new_unchecked(masked((SELFPRE self.0).$newfn(RHSPRE rhs.0)))
                        }
                    }
                }
            }
        }
    };
    (
        @assign
        type: [
            name: $tyname:ident,
            outerparams: [$($($oparams:tt)+)?],
            appliedparams: [$($($aparams:tt)+)?],
            where: [$($($where:tt)+)?],
        ],
        $newtrait:ident::$newfn:ident = $oldtrait:ident::$oldfn:ident
    ) => {
        subst_macros::repeat_parallel_subst! {
            groups: [
                [group
                    [sub [GEN] = [$(<$($oparams)+>)?]]
                    [sub [LHSTY] = [$tyname$(<$($aparams)+>)?]]
                    [sub [RHSTY] = [$tyname$(<$($aparams)+>)?]]
                    [sub [SELFPRE] = []]
                    [sub [RHSPRE] = []]
                    // Look for and remove any `Output = ...>`
                    [sub [, Output = $$output:ty>] = [>]]
                    [sub [NEWTRAIT] = [$newtrait]]
                ]
                [group
                    [sub [GEN] = [<'a$(, $($oparams)+)?>]]
                    [sub [LHSTY] = [$tyname$(<$($aparams)+>)?]]
                    [sub [RHSTY] = [&'a $tyname$(<$($aparams)+>)?]]
                    [sub [SELFPRE] = []]
                    [sub [RHSPRE] = [&]]
                    // Look for and remove any `Output = ...>`
                    [sub [, Output = $$output:ty>] = [>]]
                    [sub [NEWTRAIT] = [$newtrait]]
                ]
            ],
            callback: NONE,
            in: {
                impl GEN $oldtrait<RHSTY> for LHSTY $(
                where
                    $($where)+
                )? {
                    #[allow(unused_unsafe)]
                    fn $oldfn(&mut self, rhs: RHSTY) {
                        (SELFPRE self.0).$newfn(RHSPRE rhs.0);
                        if !check_bounds(&self.0) {
                            apply_mask_to(&mut self.0);
                        }
                    }
                }
            }
        }
    };
    (
        @assign @strict
        type: [
            name: $tyname:ident,
            outerparams: [$($($oparams:tt)+)?],
            appliedparams: [$($($aparams:tt)+)?],
            where: [$($($where:tt)+)?],
        ],
        $newtrait:ident::$newfn:ident = $oldtrait:ident::$oldfn:ident
    ) => {
        subst_macros::repeat_parallel_subst! {
            groups: [
                [group
                    [sub [GEN] = [$(<$($oparams)+>)?]]
                    [sub [LHSTY] = [$tyname$(<$($aparams)+>)?]]
                    [sub [RHSTY] = [$tyname$(<$($aparams)+>)?]]
                    [sub [SELFPRE] = []]
                    [sub [RHSPRE] = []]
                    // Look for and remove any `Output = ...>`
                    [sub [, Output = $$output:ty>] = [>]]
                    [sub [NEWTRAIT] = [$newtrait]]
                ]
                [group
                    [sub [GEN] = [<'a$(, $($oparams)+)?>]]
                    [sub [LHSTY] = [$tyname$(<$($aparams)+>)?]]
                    [sub [RHSTY] = [&'a $tyname$(<$($aparams)+>)?]]
                    [sub [SELFPRE] = []]
                    [sub [RHSPRE] = [&]]
                    // Look for and remove any `Output = ...>`
                    [sub [, Output = $$output:ty>] = [>]]
                    [sub [NEWTRAIT] = [$newtrait]]
                ]
            ],
            callback: NONE,
            in: {
                impl GEN $oldtrait<RHSTY> for LHSTY $(
                where
                    $($where)+
                )? {
                    #[allow(unused_unsafe)]
                    fn $oldfn(&mut self, rhs: RHSTY) {
                        (SELFPRE self.0).$newfn(RHSPRE rhs.0);
                        masked_inplace(&mut self.0);
                    }
                }
            }
        }
    };
}

pub(crate) use newtype_copyable_impl;

/// Generates "copyable" implementations of a trait for a type
///
/// For the purposes of this macro, a "copyable" implementation must meet all of the following
/// criteria:
/// - The trait must be a `const trait`
/// - The trait must have a single type parameter (referred to here as `Rhs`)
/// - The trait must contain only an associated type `Output` and a single method with one of the
///   following forms:
///     - If in a `@noassign` branch: `fn(self, other: Rhs) -> Self::Output`
///     - If in a `@assign` branch: `fn(&mut self, other: Rhs)`
/// - It makes sense for ALL OF the following implementations to exist:
///     - If in a `@noassign` branch:
///         - `impl Trait<T> for T`
///         - `impl Trait<&T> for T`
///         - `impl Trait<T> for &T`
///         - `impl<'a, 'b> Trait<&'b T> for &'a T`
///     - If in a `@assign` branch:
///         - `impl Trait<T> for T`
///         - `impl Trait<&T> for T`
/// - It makes sense for the body of the trait method to be identical in each `impl`
/// - The body of the trait method must be `const`
///
/// #### Substitutions
///
/// This macro invokes [`repeat_parallel_subst!`](subst_macros::repeat_parallel_subst) with the
/// following hard-coded substitutions, as well as any other ones passed in by the caller:
/// - `LHSTY` is substituted with two different things depending on which `impl` is being done:
///     - `$lhsty`, e.g. `usize`
///     - `&'a $lhsty`, e.g. `&'a bool`
/// - `RHSTY` is substituted with three different things depending on which `impl` is being done:
///     - `$rhsty`, e.g. `i16`
///     - `&'a $rhsty`, e.g. `&'a u128`
///     - `&'b $rhsty`, e.g. `&'b char`
///
/// Because this macro emits `impl const` implementations, you may need to enable additional
/// compiler features in order to invoke it.
///
/// If this makes sense, then you are able to generate impls as follows:
/// ```ignore
/// #![feature(const_ops, const_trait_impl, macro_metavar_expr)]
///
/// # use clash_num::macros::copyable_op_impl;
/// use core::ops::Add;
///
/// struct Foo(usize);
///
/// copyable_op_impl! {
///     @noassign
///     gen = [],
///     Lhs = Foo,
///     Rhs = Foo,
///     where = [],
///     Add::add(self, rhs): {
///         Foo(self.0 + rhs.0)
///     }
/// }
/// ```
///
/// # Compiler features
///
/// This macro requires the `macro_metavar_expr` and `const_trait_impl` compiler features to be
/// enabled.
#[cfg_attr(test, macro_export)]
macro_rules! copyable_op_impl {
    (
        $(#[$($meta:tt)+])*
        @noassign
        $(extrasub = [
            pre: [$($presub:tt)*],
            pst: [$($pstsub:tt)*],
        ],)?
        gen = [$($($gen:tt)+)?],
        $(traitgen = [$($tgen:tt)+],)?
        Lhs = $lhsty:ty,
        Rhs = $rhsty:ty,
        where = [$($($where:tt)+)?],
        $trait:ident::$fn:ident(
            $self:ident,
            $rhs:ident$(,)?
        ): { $($body:tt)* }
    ) => {
        subst_macros::repeat_parallel_subst! {
            groups: [
                [group
                    $($($presub)*)?
                    [sub [GEN] = [<$($($gen)+)?>]]
                    [sub [LHSTY] = [$lhsty]]
                    [sub [RHSTY] = [$rhsty]]
                    [sub [LDEREF] = []]
                    [sub [RDEREF] = []]
                    $($($pstsub)*)?
                ]
                [group
                    $($($presub)*)?
                    [sub [GEN] = [<'a$(, $($gen)+)?>]]
                    [sub [LHSTY] = [$lhsty]]
                    [sub [RHSTY] = [&'a $rhsty]]
                    [sub [LDEREF] = []]
                    [sub [RDEREF] = [*]]
                    $($($pstsub)*)?
                ]
                [group
                    $($($presub)*)?
                    [sub [GEN] = [<'a$(, $($gen)+)?>]]
                    [sub [LHSTY] = [&'a $lhsty]]
                    [sub [RHSTY] = [$rhsty]]
                    [sub [LDEREF] = [*]]
                    [sub [RDEREF] = []]
                    $($($pstsub)*)?
                ]
                [group
                    $($($presub)*)?
                    [sub [GEN] = [<'a, 'b$(, $($gen)+)?>]]
                    [sub [LHSTY] = [&'a $lhsty]]
                    [sub [RHSTY] = [&'b $rhsty]]
                    [sub [LDEREF] = [*]]
                    [sub [RDEREF] = [*]]
                    $($($pstsub)*)?
                ]
            ],
            callback: NONE,
            in: {
                $(#[$($meta)+])*
                impl GEN const $trait<$($($tgen)+, )?RHSTY> for LHSTY $(
                where
                    $($where)+
                )?{
                    type Output = $lhsty;

                    fn $fn($self, $rhs: RHSTY) -> Self::Output { $($body)* }
                }
            }
        }
    };
    (
        $(#[$($meta:tt)+])*
        @assign
        $(extrasub = [
            pre: [$($presub:tt)*],
            pst: [$($pstsub:tt)*],
        ],)?
        gen = [$($($gen:tt)+)?],
        $(traitgen = [$($tgen:tt)+],)?
        Lhs = $lhsty:ty,
        Rhs = $rhsty:ty,
        where = [$($($where:tt)+)?],
        $trait:ident::$fn:ident(
            $self:ident,
            $rhs:ident$(,)?
        ): { $($body:tt)* }
    ) => {
        subst_macros::repeat_parallel_subst! {
            groups: [
                [group
                    $($($presub)*)?
                    [sub [GEN] = [$(<$($gen)+>)?]]
                    [sub [LHSTY] = [$lhsty]]
                    [sub [RHSTY] = [$rhsty]]
                    [sub [LDEREF] = [*]]
                    [sub [RDEREF] = []]
                    $($($pstsub)*)?
                ]
                [group
                    $($($presub)*)?
                    [sub [GEN] = [<'a$(, $($gen)+)?>]]
                    [sub [LHSTY] = [$lhsty]]
                    [sub [RHSTY] = [&'a $rhsty]]
                    [sub [LDEREF] = [*]]
                    [sub [RDEREF] = [*]]
                    $($($pstsub)*)?
                ]
            ],
            callback: NONE,
            in: {
                $(#[$($meta)+])*
                impl GEN const $trait<$($($tgen)+, )?RHSTY> for LHSTY $(
                where
                    $($where)+
                )?{
                    fn $fn(&mut $self, $rhs: RHSTY) { $($body)* }
                }
            }
        }
    };
}

pub(crate) use copyable_op_impl;
