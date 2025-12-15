// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::numeric_size;

#[cfg(debug_assertions)]
use const_panic::concat_panic;

pub const trait HasUnsignedBacker: Sized {
    type Type: Copy + const PartialEq + const Eq;
    const MIN: Self::Type;
    const MAX: Self::Type;

    type Mask: Copy + const PartialEq + const Eq;
    const MASK: Self::Mask;
    fn apply_mask_to(val: &mut Self::Type);
    fn apply_mask(val: Self::Type) -> Self::Type {
        let mut val = val;
        Self::apply_mask_to(&mut val);
        val
    }

    fn bounds_check(val: &Self::Type) -> bool;
}

pub struct UnsignedLutInner<const N: u8, const M: u8>;

pub type UnsignedLut<const N: u8> = UnsignedLutInner<N, { numeric_size(N) }>;
pub type UnsignedBackerOf<const N: u8> = <UnsignedLut<N> as HasUnsignedBacker>::Type;

subst_macros::repeat_parallel_subst! {
    groups: [
        [group [sub [TYPE] = [u8]] [sub [BITS] = [8]] [sub [DIV8] = [1]]]
        [group [sub [TYPE] = [u16]] [sub [BITS] = [16]] [sub [DIV8] = [2]]]
        [group [sub [TYPE] = [u32]] [sub [BITS] = [32]] [sub [DIV8] = [4]]]
        [group [sub [TYPE] = [u64]] [sub [BITS] = [64]] [sub [DIV8] = [8]]]
        [group [sub [TYPE] = [u128]] [sub [BITS] = [128]] [sub [DIV8] = [16]]]
    ],
    callback: NONE,
    in: {
        impl<const N: u8> const HasUnsignedBacker for UnsignedLutInner<N, DIV8> {
            type Type = TYPE;
            const MIN: Self::Type = 0;
            const MAX: Self::Type = !(!0 << N);

            type Mask = TYPE;
            const MASK: Self::Mask = {
                if N == BITS {
                    !0
                } else {
                    !(!0 << N)
                }
            };
            fn apply_mask_to(val: &mut Self::Type) {
                *val &= Self::MASK;
            }

            fn bounds_check(val: &Self::Type) -> bool {
                Self::MASK.ge(val)
            }
        }
    }
}

const fn unsigned_check_bounds<const N: u8>(val: &UnsignedBackerOf<N>) -> bool
where
    UnsignedLut<N>: const HasUnsignedBacker,
{
    <UnsignedLut<N> as HasUnsignedBacker>::bounds_check(val)
}

#[repr(transparent)]
#[derive(Copy, Clone)]
/// Unsigned integer type representing `N` bits.
pub struct Unsigned<const N: u8>(pub(super) UnsignedBackerOf<N>)
where
    UnsignedLut<N>: const HasUnsignedBacker;

impl<const N: u8> Unsigned<N>
where
    UnsignedLut<N>: const HasUnsignedBacker,
{
    pub const fn new(val: UnsignedBackerOf<N>) -> Option<Self> {
        if unsigned_check_bounds(&val) {
            Some(Unsigned(val))
        } else {
            None
        }
    }

    #[doc(hidden)]
    pub const unsafe fn new_unchecked(val: UnsignedBackerOf<N>) -> Self {
        Unsigned(val)
    }
}

#[cfg(debug_assertions)]
pub(super) const fn masked<const N: u8>(val: UnsignedBackerOf<N>) -> UnsignedBackerOf<N>
where
    UnsignedLut<N>: const HasUnsignedBacker,
{
    if unsigned_check_bounds(&val) {
        val
    } else {
        concat_panic!("Output overflowed maximum value for type Unsigned<", N, ">");
    }
}

#[cfg(not(debug_assertions))]
pub(super) const fn masked<const N: u8>(val: UnsignedBackerOf<N>) -> UnsignedBackerOf<N>
where
    UnsignedLut<N>: const HasUnsignedBacker,
{
    if unsigned_check_bounds(&val) {
        val
    } else {
        <UnsignedLut<N> as HasUnsignedBacker>::apply_mask(val)
    }
}

#[cfg(debug_assertions)]
pub(super) const fn masked_inplace<const N: u8>(val: &mut UnsignedBackerOf<N>)
where
    UnsignedLut<N>: const HasUnsignedBacker,
{
    if !unsigned_check_bounds(val) {
        concat_panic!("Output overflowed maximum value for type Unsigned<", N, ">");
    }
}

#[cfg(not(debug_assertions))]
pub(super) const fn masked_inplace<const N: u8>(val: &mut UnsignedBackerOf<N>)
where
    UnsignedLut<N>: const HasUnsignedBacker,
{
    if !unsigned_check_bounds(val) {
        <UnsignedLut<N> as HasUnsignedBacker>::apply_mask_to(val);
    }
}

subst_macros::repeat_parallel_subst! {
    groups: [
        [group [sub [TRAIT] = [Add]] [sub [TRAITFN] = [add]]]
        [group [sub [TRAIT] = [BitAnd]] [sub [TRAITFN] = [bitand]]]
        [group [sub [TRAIT] = [BitOr]] [sub [TRAITFN] = [bitor]]]
        [group [sub [TRAIT] = [BitXor]] [sub [TRAITFN] = [bitxor]]]
        [group [sub [TRAIT] = [Div]] [sub [TRAITFN] = [div]]]
        [group [sub [TRAIT] = [Mul]] [sub [TRAITFN] = [mul]]]
        [group [sub [TRAIT] = [Rem]] [sub [TRAITFN] = [rem]]]
        [group [sub [TRAIT] = [Sub]] [sub [TRAITFN] = [sub]]]
    ],
    callback: [
        macro: subst_macros::repeat_parallel_subst,
        prefix: [
            @callback
            groups: [
                [group
                    [sub [GEN] = []]
                    [sub [LHSTY] = [Unsigned<N>]]
                    [sub [RHSTY] = [Unsigned<N>]]
                    [sub [LHSINNER] = [UnsignedBackerOf<N>]]
                    [sub [RHSINNER] = [UnsignedBackerOf<N>]]
                    [sub [LHSPRE] = []]
                    [sub [RHSPRE] = []]
                ]
                [group
                    [sub [GEN] = ['a, ]]
                    [sub [LHSTY] = [Unsigned<N>]]
                    [sub [RHSTY] = [&'a Unsigned<N>]]
                    [sub [LHSINNER] = [UnsignedBackerOf<N>]]
                    [sub [RHSINNER] = [&'a UnsignedBackerOf<N>]]
                    [sub [LHSPRE] = []]
                    [sub [RHSPRE] = [&]]
                ]
                [group
                    [sub [GEN] = ['a, ]]
                    [sub [LHSTY] = [&'a Unsigned<N>]]
                    [sub [RHSTY] = [Unsigned<N>]]
                    [sub [LHSINNER] = [&'a UnsignedBackerOf<N>]]
                    [sub [RHSINNER] = [UnsignedBackerOf<N>]]
                    [sub [LHSPRE] = [&]]
                    [sub [RHSPRE] = []]
                ]
                [group
                    [sub [GEN] = ['a, 'b, ]]
                    [sub [LHSTY] = [&'a Unsigned<N>]]
                    [sub [RHSTY] = [&'b Unsigned<N>]]
                    [sub [LHSINNER] = [&'a UnsignedBackerOf<N>]]
                    [sub [RHSINNER] = [&'b UnsignedBackerOf<N>]]
                    [sub [LHSPRE] = [&]]
                    [sub [RHSPRE] = [&]]
                ]
            ],
            callback: NONE,
        ],
        suffix: [],
    ],
    in: {
        impl<GEN const N: u8> const core::ops::TRAIT<RHSTY> for LHSTY
        where
            UnsignedLut<N>: const HasUnsignedBacker,
            LHSINNER: const core::ops::TRAIT<RHSINNER, Output = UnsignedBackerOf<N>>,
        {
            type Output = Unsigned<N>;

            fn TRAITFN(self, rhs: RHSTY) -> Self::Output {
                Unsigned(masked((LHSPRE self.0).TRAITFN(RHSPRE rhs.0)))
            }
        }
    }
}

subst_macros::repeat_parallel_subst! {
    groups: [
        [group [sub [TRAIT] = [AddAssign]] [sub [TRAITFN] = [add_assign]]]
        [group [sub [TRAIT] = [BitAndAssign]] [sub [TRAITFN] = [bitand_assign]]]
        [group [sub [TRAIT] = [BitOrAssign]] [sub [TRAITFN] = [bitor_assign]]]
        [group [sub [TRAIT] = [BitXorAssign]] [sub [TRAITFN] = [bitxor_assign]]]
        [group [sub [TRAIT] = [DivAssign]] [sub [TRAITFN] = [div_assign]]]
        [group [sub [TRAIT] = [MulAssign]] [sub [TRAITFN] = [mul_assign]]]
        [group [sub [TRAIT] = [RemAssign]] [sub [TRAITFN] = [rem_assign]]]
        [group [sub [TRAIT] = [SubAssign]] [sub [TRAITFN] = [sub_assign]]]
    ],
    callback: [
        macro: subst_macros::repeat_parallel_subst,
        prefix: [
            @callback
            groups: [
                [group
                    [sub [GEN] = []]
                    [sub [LHSTY] = [Unsigned<N>]]
                    [sub [RHSTY] = [Unsigned<N>]]
                    [sub [LHSINNER] = [UnsignedBackerOf<N>]]
                    [sub [RHSINNER] = [UnsignedBackerOf<N>]]
                    [sub [LHSPRE] = []]
                    [sub [RHSPRE] = []]
                ]
                [group
                    [sub [GEN] = ['a, ]]
                    [sub [LHSTY] = [Unsigned<N>]]
                    [sub [RHSTY] = [&'a Unsigned<N>]]
                    [sub [LHSINNER] = [UnsignedBackerOf<N>]]
                    [sub [RHSINNER] = [&'a UnsignedBackerOf<N>]]
                    [sub [LHSPRE] = []]
                    [sub [RHSPRE] = [&]]
                ]
            ],
            callback: NONE,
        ],
        suffix: [],
    ],
    in: {
        impl<GEN const N: u8> const core::ops::TRAIT<RHSTY> for LHSTY
        where
            UnsignedLut<N>: const HasUnsignedBacker,
            LHSINNER: const core::ops::TRAIT<RHSINNER>,
        {
            fn TRAITFN(&mut self, rhs: RHSTY) {
                self.0.TRAITFN(RHSPRE rhs.0);
                masked_inplace(&mut self.0);
            }
        }
    }
}
