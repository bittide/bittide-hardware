// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
/// Guarantees the presence of `.saturating_add()` as implemented on the numeric types.
pub const trait SaturatingAdd<Rhs = Self> {
    type Output;

    fn saturating_add(self, rhs: Rhs) -> Self::Output;
}

/// Guarantees the presence of `.saturating_sub()` as implemented on the numeric types.
pub const trait SaturatingSub<Rhs = Self> {
    type Output;

    fn saturating_sub(self, rhs: Rhs) -> Self::Output;
}

pub const trait SaturatingAddAssign<Rhs = Self> {
    fn saturating_add_assign(&mut self, rhs: Rhs);
}

pub const trait SaturatingSubAssign<Rhs = Self> {
    fn saturating_sub_assign(&mut self, rhs: Rhs);
}

subst_macros::repeat_parallel_subst! {
    groups: [
        [group [sub [TYPE] = [u8]]]
        [group [sub [TYPE] = [u16]]]
        [group [sub [TYPE] = [u32]]]
        [group [sub [TYPE] = [u64]]]
        [group [sub [TYPE] = [u128]]]
        [group [sub [TYPE] = [usize]]]
    ],
    callback: [
        macro: subst_macros::repeat_parallel_subst,
        prefix: [
            @callback
            groups: [
                [group
                    [sub [TRAIT] = [SaturatingAdd]]
                    [sub [TRAITASSIGN] = [SaturatingAddAssign]]
                    [sub [FN] = [saturating_add]]
                    [sub [FNASSIGN] = [saturating_add_assign]]
                ]
                [group
                    [sub [TRAIT] = [SaturatingSub]]
                    [sub [TRAITASSIGN] = [SaturatingSubAssign]]
                    [sub [FN] = [saturating_sub]]
                    [sub [FNASSIGN] = [saturating_sub_assign]]
                ]
            ],
            callback: NONE,
        ],
        suffix: [],
    ],
    in: {
        crate::macros::copyable_op_impl! {
            @noassign
            gen = [],
            Lhs = TYPE,
            Rhs = TYPE,
            where = [],
            TRAIT::FN(self, rhs): {
                TYPE::FN(LDEREF self, RDEREF rhs)
            }
        }

        crate::macros::copyable_op_impl! {
            @assign
            gen = [],
            Lhs = TYPE,
            Rhs = TYPE,
            where = [],
            TRAITASSIGN::FNASSIGN(self, rhs): {
                *self = TYPE::FN(LDEREF self, RDEREF rhs);
            }
        }
    }
}

pub const trait InnerTy {
    type Inner;
}

pub type GetInnerTy<T> = <T as InnerTy>::Inner;

// impl<const N: u16> InnerTy for crate::bitvec::BitVec<N>
// where
//     crate::bitvec::BitVecLut<N>: const crate::bitvec::HasBVBacker,
// {
//     type Inner = crate::bitvec::BVBackerOf<N>;
// }

// impl<'a, const N: u16> InnerTy for &'a crate::bitvec::BitVec<N>
// where
//     crate::bitvec::BitVecLut<N>: const crate::bitvec::HasBVBacker,
// {
//     type Inner = &'a crate::bitvec::BVBackerOf<N>;
// }

impl<const N: u128> InnerTy for crate::index::Index<N>
where
    crate::index::IndexLut<N>: const crate::index::HasIndexBacker,
{
    type Inner = crate::index::IndexBackerOf<N>;
}

impl<'a, const N: u128> InnerTy for &'a crate::index::Index<N>
where
    crate::index::IndexLut<N>: const crate::index::HasIndexBacker,
{
    type Inner = &'a crate::index::IndexBackerOf<N>;
}
