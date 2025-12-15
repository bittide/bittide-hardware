// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::numeric_size;

#[cfg(debug_assertions)]
use const_panic::concat_panic;

pub const trait HasSignedBacker<const N: u8>: Sized {
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

    fn is_pos(val: &Self::Type) -> bool;
    fn is_neg(val: &Self::Type) -> bool;
}

pub struct SignedLutInner<const N: u8, const M: u8>;

pub type SignedLut<const N: u8> = SignedLutInner<N, { numeric_size(N) }>;
pub type SignedBackerOf<const N: u8> = <SignedLut<N> as HasSignedBacker<N>>::Type;

impl<const N: u8> const HasSignedBacker<N> for SignedLutInner<N, 1> {
    type Type = u8;
    const MIN: Self::Type = 0b1 << (N - 1);
    const MAX: Self::Type = !(!0 << (N - 1));

    type Mask = u8;
    const MASK: Self::Mask = { if N == 8 { !0 } else { !(!0 << N) } };

    fn apply_mask_to(val: &mut Self::Type) {
        *val &= Self::MASK;
    }

    fn bounds_check(val: &Self::Type) -> bool {
        Self::MASK.ge(val)
    }

    fn is_pos(val: &Self::Type) -> bool {
        val & Self::MIN == 0
    }

    fn is_neg(val: &Self::Type) -> bool {
        val & Self::MIN != 0
    }
}

impl<const N: u8> const HasSignedBacker<N> for SignedLutInner<N, 2> {
    type Type = u16;
    const MIN: Self::Type = 0b1 << (N - 1);
    const MAX: Self::Type = !(!0 << (N - 1));

    type Mask = u16;
    const MASK: Self::Mask = { if N == 16 { !0 } else { !(!0 << N) } };

    fn apply_mask_to(val: &mut Self::Type) {
        *val &= Self::MASK;
    }

    fn bounds_check(val: &Self::Type) -> bool {
        Self::MASK.ge(val)
    }

    fn is_pos(val: &Self::Type) -> bool {
        val & Self::MIN == 0
    }

    fn is_neg(val: &Self::Type) -> bool {
        val & Self::MIN != 0
    }
}

impl<const N: u8> const HasSignedBacker<N> for SignedLutInner<N, 4> {
    type Type = u32;
    const MIN: Self::Type = 0b1 << (N - 1);
    const MAX: Self::Type = !(!0 << (N - 1));

    type Mask = u32;
    const MASK: Self::Mask = !(!0 << N);

    fn apply_mask_to(val: &mut Self::Type) {
        *val &= Self::MASK;
    }

    fn bounds_check(val: &Self::Type) -> bool {
        Self::MASK.ge(val)
    }

    fn is_pos(val: &Self::Type) -> bool {
        val & Self::MIN == 0
    }

    fn is_neg(val: &Self::Type) -> bool {
        val & Self::MIN != 0
    }
}

impl<const N: u8> const HasSignedBacker<N> for SignedLutInner<N, 8> {
    type Type = u64;
    const MIN: Self::Type = 0b1 << (N - 1);
    const MAX: Self::Type = !(!0 << (N - 1));

    type Mask = u64;
    const MASK: Self::Mask = !(!0 << N);

    fn apply_mask_to(val: &mut Self::Type) {
        *val &= Self::MASK;
    }

    fn bounds_check(val: &Self::Type) -> bool {
        Self::MASK.ge(val)
    }

    fn is_pos(val: &Self::Type) -> bool {
        val & Self::MIN == 0
    }

    fn is_neg(val: &Self::Type) -> bool {
        val & Self::MIN != 0
    }
}

impl<const N: u8> const HasSignedBacker<N> for SignedLutInner<N, 16> {
    type Type = u128;
    const MIN: Self::Type = 0b1 << (N - 1);
    const MAX: Self::Type = !(!0 << (N - 1));

    type Mask = u128;
    const MASK: Self::Mask = !(!0 << N);

    fn apply_mask_to(val: &mut Self::Type) {
        *val &= Self::MASK;
    }

    fn bounds_check(val: &Self::Type) -> bool {
        Self::MASK.ge(val)
    }

    fn is_pos(val: &Self::Type) -> bool {
        val & Self::MIN == 0
    }

    fn is_neg(val: &Self::Type) -> bool {
        val & Self::MIN != 0
    }
}

#[repr(transparent)]
#[derive(Copy, Clone)]
/// Signed integer type representing `N` bits.
pub struct Signed<const N: u8>(pub(super) SignedBackerOf<N>)
where
    SignedLut<N>: const HasSignedBacker<N>;

impl<const N: u8> Signed<N>
where
    SignedLut<N>: const HasSignedBacker<N>,
{
    pub const fn new(val: SignedBackerOf<N>) -> Option<Self> {
        if signed_check_bounds(&val) {
            Some(Signed(val))
        } else {
            None
        }
    }

    #[doc(hidden)]
    pub const unsafe fn new_unchecked(val: SignedBackerOf<N>) -> Self {
        Signed(val)
    }
}

const fn signed_check_bounds<const N: u8>(val: &SignedBackerOf<N>) -> bool
where
    SignedLut<N>: const HasSignedBacker<N>,
{
    <SignedLut<N> as HasSignedBacker<N>>::bounds_check(val)
}

#[cfg(debug_assertions)]
const fn masked<const N: u8>(val: SignedBackerOf<N>) -> SignedBackerOf<N>
where
    SignedLut<N>: const HasSignedBacker<N>,
{
    if signed_check_bounds(&val) {
        val
    } else {
        concat_panic!("Output overflowed maximum value for type Signed<", N, ">");
    }
}

#[cfg(not(debug_assertions))]
const fn masked<const N: u8>(val: SignedBackerOf<N>) -> SignedBackerOf<N>
where
    SignedLut<N>: const HasSignedBacker<N>,
{
    if signed_check_bounds(&val) {
        val
    } else {
        <SignedLut<N> as HasSignedBacker<N>>::apply_mask(val)
    }
}

#[cfg(debug_assertions)]
const fn masked_inplace<const N: u8>(val: &mut SignedBackerOf<N>)
where
    SignedLut<N>: const HasSignedBacker<N>,
{
    if !signed_check_bounds(val) {
        concat_panic!("Output overflowed maximum value for type Signed<", N, ">");
    }
}

#[cfg(not(debug_assertions))]
const fn masked_inplace<const N: u8>(val: &mut SignedBackerOf<N>)
where
    SignedLut<N>: const HasSignedBacker<N>,
{
    if !signed_check_bounds(val) {
        <SignedLut<N> as HasSignedBacker<N>>::apply_mask_to(val);
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
                    [sub [LHSTY] = [Signed<N>]]
                    [sub [RHSTY] = [Signed<N>]]
                    [sub [LHSINNER] = [SignedBackerOf<N>]]
                    [sub [RHSINNER] = [SignedBackerOf<N>]]
                    [sub [LHSPRE] = []]
                    [sub [RHSPRE] = []]
                ]
                [group
                    [sub [GEN] = ['a, ]]
                    [sub [LHSTY] = [Signed<N>]]
                    [sub [RHSTY] = [&'a Signed<N>]]
                    [sub [LHSINNER] = [SignedBackerOf<N>]]
                    [sub [RHSINNER] = [&'a SignedBackerOf<N>]]
                    [sub [LHSPRE] = []]
                    [sub [RHSPRE] = [&]]
                ]
                [group
                    [sub [GEN] = ['a, ]]
                    [sub [LHSTY] = [&'a Signed<N>]]
                    [sub [RHSTY] = [Signed<N>]]
                    [sub [LHSINNER] = [&'a SignedBackerOf<N>]]
                    [sub [RHSINNER] = [SignedBackerOf<N>]]
                    [sub [LHSPRE] = [&]]
                    [sub [RHSPRE] = []]
                ]
                [group
                    [sub [GEN] = ['a, 'b, ]]
                    [sub [LHSTY] = [&'a Signed<N>]]
                    [sub [RHSTY] = [&'b Signed<N>]]
                    [sub [LHSINNER] = [&'a SignedBackerOf<N>]]
                    [sub [RHSINNER] = [&'b SignedBackerOf<N>]]
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
            SignedLut<N>: const HasSignedBacker<N>,
            LHSINNER: const core::ops::TRAIT<RHSINNER, Output = SignedBackerOf<N>>,
        {
            type Output = Signed<N>;

            fn TRAITFN(self, rhs: RHSTY) -> Self::Output {
                Signed(masked((LHSPRE self.0).TRAITFN(RHSPRE rhs.0)))
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
                    [sub [LHSTY] = [Signed<N>]]
                    [sub [RHSTY] = [Signed<N>]]
                    [sub [LHSINNER] = [SignedBackerOf<N>]]
                    [sub [RHSINNER] = [SignedBackerOf<N>]]
                    [sub [LHSPRE] = []]
                    [sub [RHSPRE] = []]
                ]
                [group
                    [sub [GEN] = ['a, ]]
                    [sub [LHSTY] = [Signed<N>]]
                    [sub [RHSTY] = [&'a Signed<N>]]
                    [sub [LHSINNER] = [SignedBackerOf<N>]]
                    [sub [RHSINNER] = [&'a SignedBackerOf<N>]]
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
            SignedLut<N>: const HasSignedBacker<N>,
            LHSINNER: const core::ops::TRAIT<RHSINNER>,
        {
            fn TRAITFN(&mut self, rhs: RHSTY) {
                self.0.TRAITFN(RHSPRE rhs.0);
                masked_inplace(&mut self.0);
            }
        }
    }
}
