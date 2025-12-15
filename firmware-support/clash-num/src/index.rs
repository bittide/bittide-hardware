// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::{
    index_bit_size,
    traits::{GetInnerTy, SaturatingAdd, SaturatingAddAssign, SaturatingSub, SaturatingSubAssign},
};
use core::ops::{Add, AddAssign, Sub, SubAssign};

#[cfg(debug_assertions)]
use const_panic::concat_panic;

/// Trait used to implement the trait LUT for [`Index<N>`] backing types
pub const trait HasIndexBacker: Sized {
    /// The inner type for a given size of [`Index<N>`]
    type Type: Copy + const PartialOrd;
    /// Minimum allowable value for this size
    const MIN: Self::Type;
    /// Maximum allowable value for this size
    const MAX: Self::Type;

    /// Runs masking behaviour on a value in-place
    ///
    /// Has the following behaviours:
    /// - Debug mode: panics on overflow
    /// - Release mode: wraps on overflow
    fn apply_mask_to(val: &mut Self::Type);
    /// Runs masking behaviour on a value and returns the result
    ///
    /// Has the following behaviours:
    /// - Debug mode: panics on overflow
    /// - Release mode: wraps on overflow
    fn apply_mask(val: Self::Type) -> Self::Type {
        let mut val = val;
        Self::apply_mask_to(&mut val);
        val
    }

    /// Checks that the inner value is within `MIN..MAX`
    fn bounds_check(val: &Self::Type) -> bool;
}

/// Marker type used to perform lookups for backing types and helper methods/functions
pub struct IndexLutInner<const M: u128, const N: u8>;

/// Performs a lookup into the trait LUT
pub type IndexLut<const N: u128> = IndexLutInner<N, { index_bit_size(N) }>;
/// Retrieves the inner type for a [`Index<N>`]
pub type IndexBackerOf<const N: u128> = <IndexLut<N> as HasIndexBacker>::Type;

impl<const N: u128> const HasIndexBacker for IndexLutInner<N, 128> {
    type Type = u128;
    const MIN: u128 = 0;
    const MAX: u128 = N - 1;

    fn apply_mask_to(val: &mut u128) {
        *val -= Self::MAX;
    }

    fn bounds_check(val: &Self::Type) -> bool {
        val.lt(&N)
    }
}

impl<const N: u128> const HasIndexBacker for IndexLutInner<N, 64> {
    type Type = u64;
    const MIN: u64 = 0;
    const MAX: u64 = (N - 1) as u64;

    fn apply_mask_to(val: &mut u64) {
        *val -= Self::MAX;
    }

    fn bounds_check(val: &Self::Type) -> bool {
        if N == u64::MAX as u128 + 1 {
            true
        } else {
            val.lt(&(N as u64))
        }
    }
}

impl<const N: u128> const HasIndexBacker for IndexLutInner<N, 32> {
    type Type = u32;
    const MIN: u32 = 0;
    const MAX: u32 = (N - 1) as u32;

    fn apply_mask_to(val: &mut u32) {
        *val -= Self::MAX;
    }

    fn bounds_check(val: &Self::Type) -> bool {
        if N == u32::MAX as u128 + 1 {
            true
        } else {
            val.lt(&(N as u32))
        }
    }
}

impl<const N: u128> const HasIndexBacker for IndexLutInner<N, 16> {
    type Type = u16;
    const MIN: u16 = 0;
    const MAX: u16 = (N - 1) as u16;

    fn apply_mask_to(val: &mut u16) {
        *val -= Self::MAX;
    }

    fn bounds_check(val: &Self::Type) -> bool {
        if N == u16::MAX as u128 + 1 {
            true
        } else {
            val.lt(&(N as u16))
        }
    }
}

impl<const N: u128> const HasIndexBacker for IndexLutInner<N, 8> {
    type Type = u8;
    const MIN: u8 = 0;
    const MAX: u8 = (N - 1) as u8;

    fn apply_mask_to(val: &mut u8) {
        *val -= Self::MAX;
    }

    fn bounds_check(val: &Self::Type) -> bool {
        if N == u8::MAX as u128 + 1 {
            true
        } else {
            val.lt(&(N as u8))
        }
    }
}

pub const fn check_bounds<const N: u128>(val: &IndexBackerOf<N>) -> bool
where
    IndexLut<N>: const HasIndexBacker,
{
    <IndexLut<N> as HasIndexBacker>::bounds_check(val)
}

pub const fn apply_mask<const N: u128>(val: IndexBackerOf<N>) -> IndexBackerOf<N>
where
    IndexLut<N>: const HasIndexBacker,
{
    <IndexLut<N> as HasIndexBacker>::apply_mask(val)
}

pub const fn apply_mask_to<const N: u128>(val: &mut IndexBackerOf<N>)
where
    IndexLut<N>: const HasIndexBacker,
{
    <IndexLut<N> as HasIndexBacker>::apply_mask_to(val);
}

#[cfg(debug_assertions)]
pub const fn masked<const N: u128>(val: IndexBackerOf<N>) -> IndexBackerOf<N>
where
    IndexLut<N>: const HasIndexBacker,
{
    if check_bounds(&val) {
        val
    } else {
        concat_panic!("Value overflowed maximum for type Index<", N, ">");
    }
}

#[cfg(not(debug_assertions))]
pub const fn masked<const N: u128>(val: IndexBackerOf<N>) -> IndexBackerOf<N>
where
    IndexLut<N>: const HasIndexBacker,
{
    if check_bounds(&val) {
        val
    } else {
        <IndexLut<N> as HasIndexBacker>::apply_mask(val)
    }
}

#[cfg(debug_assertions)]
pub const fn masked_inplace<const N: u128>(val: &mut IndexBackerOf<N>)
where
    IndexLut<N>: const HasIndexBacker,
{
    if !check_bounds(val) {
        concat_panic!("Value overflowed maximum for type Index<", N, ">");
    }
}

#[cfg(not(debug_assertions))]
pub const fn masked_inplace<const N: u128>(val: &mut IndexBackerOf<N>)
where
    IndexLut<N>: const HasIndexBacker,
{
    if !check_bounds(val) {
        <IndexLut<N> as HasIndexBacker>::apply_mask_to(val);
    }
}

/// Unsigned integer type bound at type-level to a range of `0..N`
#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct Index<const N: u128>(pub(crate) IndexBackerOf<N>)
where
    IndexLut<N>: const HasIndexBacker;

impl<const N: u128> Index<N>
where
    IndexLut<N>: const HasIndexBacker,
{
    pub const MIN: Self = Index(<IndexLut<N> as HasIndexBacker>::MIN);
    pub const MAX: Self = Index(<IndexLut<N> as HasIndexBacker>::MAX);

    pub const fn new(val: IndexBackerOf<N>) -> Option<Self> {
        if check_bounds(&val) {
            Some(Index(val))
        } else {
            None
        }
    }

    #[doc(hidden)]
    #[must_use]
    pub const unsafe fn new_unchecked(val: IndexBackerOf<N>) -> Self {
        Index(val)
    }

    pub const fn into_underlying(self) -> IndexBackerOf<N> {
        self.0
    }
}

subst_macros::repeat_parallel_subst! {
    groups: [
        [group [sub [TRAIT] = [core::fmt::Debug]] [sub [FMTSTR] = ["Index<{N}>({:?})"]]]
        [group [sub [TRAIT] = [core::fmt::Display]] [sub [FMTSTR] = ["{}"]]]
        [group [sub [TRAIT] = [core::fmt::LowerHex]] [sub [FMTSTR] = ["{:x}"]]]
        [group [sub [TRAIT] = [core::fmt::UpperHex]] [sub [FMTSTR] = ["{:X}"]]]
        [group [sub [TRAIT] = [core::fmt::Binary]] [sub [FMTSTR] = ["{:b}"]]]
        [group [sub [TRAIT] = [core::fmt::LowerExp]] [sub [FMTSTR] = ["{:e}"]]]
        [group [sub [TRAIT] = [core::fmt::UpperExp]] [sub [FMTSTR] = ["{:E}"]]]
        [group [sub [TRAIT] = [core::fmt::Octal]] [sub [FMTSTR] = ["{:o}"]]]
        [group [sub [TRAIT] = [core::fmt::Pointer]] [sub [FMTSTR] = ["{:p}"]]]
    ],
    callback: NONE,
    in: {
        impl<const N: u128> TRAIT for Index<N>
        where
            IndexLut<N>: const HasIndexBacker,
            IndexBackerOf<N>: TRAIT,
        {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                write!(f, FMTSTR, self.0)
            }
        }
    }
}

#[cfg(feature = "ufmt")]
subst_macros::repeat_parallel_subst! {
    groups: [
        [group
            [sub [TRAIT] = [ufmt::uDebug]]
            [sub [FNNAME] = [fmt]]
            [sub [WRITE] = ["Index<{}>({:?})", N]]
            [sub [EXTARG] = []]
        ]
        [group
            [sub [TRAIT] = [ufmt::uDisplay]]
            [sub [FNNAME] = [fmt]]
            [sub [WRITE] = ["{}"]]
            [sub [EXTARG] = []]
        ]
        [group
            [sub [TRAIT] = [ufmt::uDisplayHex]]
            [sub [FNNAME] = [fmt_hex]]
            [sub [WRITE] = ["{:x}"]]
            [sub [EXTARG] = [options: ufmt::HexOptions]]
        ]
    ],
    callback: NONE,
    in: {
        impl<const N: u128> TRAIT for Index<N>
        where
            IndexLut<N>: const HasIndexBacker,
            IndexBackerOf<N>: TRAIT,
        {
            fn FNNAME<W>(&self, w: &mut ufmt::Formatter<'_, W>, EXTARG) -> Result<(), W::Error>
            where
                W: ufmt::uWrite + ?Sized,
            {
                ufmt::uwrite!(w, WRITE, self.0)
            }
        }
    }
}

impl<const N: u128> Eq for Index<N>
where
    IndexLut<N>: const HasIndexBacker,
    IndexBackerOf<N>: Eq,
{
}

impl<const N: u128> PartialEq for Index<N>
where
    IndexLut<N>: const HasIndexBacker,
    IndexBackerOf<N>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }

    #[allow(clippy::partialeq_ne_impl)]
    fn ne(&self, other: &Self) -> bool {
        self.0.ne(&other.0)
    }
}

impl<const N: u128> Ord for Index<N>
where
    IndexLut<N>: const HasIndexBacker,
    IndexBackerOf<N>: Ord,
{
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.0.cmp(&other.0)
    }

    fn max(self, other: Self) -> Self
    where
        Self: Sized,
    {
        Index(self.0.max(other.0))
    }

    fn min(self, other: Self) -> Self
    where
        Self: Sized,
    {
        Index(self.0.min(other.0))
    }

    fn clamp(self, min: Self, max: Self) -> Self
    where
        Self: Sized,
    {
        Index(self.0.clamp(min.0, max.0))
    }
}

impl<const N: u128> PartialOrd for Index<N>
where
    IndexLut<N>: const HasIndexBacker,
    IndexBackerOf<N>: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }

    fn lt(&self, other: &Self) -> bool {
        self.0.lt(&other.0)
    }

    fn le(&self, other: &Self) -> bool {
        self.0.le(&other.0)
    }

    fn gt(&self, other: &Self) -> bool {
        self.0.gt(&other.0)
    }

    fn ge(&self, other: &Self) -> bool {
        self.0.ge(&other.0)
    }
}

// Wrapping impls
subst_macros::repeat_parallel_subst! {
    groups: [
        [group [sub [TRAIT] = [Add]] [sub [FN] = [add]] [sub [KIND] = [noassign]]]
        [group [sub [TRAIT] = [Sub]] [sub [FN] = [sub]] [sub [KIND] = [noassign]]]
        [group [sub [TRAIT] = [AddAssign]] [sub [FN] = [add_assign]] [sub [KIND] = [assign]]]
        [group [sub [TRAIT] = [SubAssign]] [sub [FN] = [sub_assign]] [sub [KIND] = [assign]]]
    ],
    callback: NONE,
    in: {
        crate::macros::newtype_copyable_impl! {
            @KIND @strict
            type: [
                name: Index,
                outerparams: [const N: u128],
                appliedparams: [N],
                where: [
                    IndexLut<N>: const HasIndexBacker,
                    IndexBackerOf<N>: const TRAIT<GetInnerTy<RHSTY>, Output = IndexBackerOf<N>>,
                ],
            ],
            TRAIT::FN = TRAIT::FN
        }
    }
}

const _: () = {
    const fn apply_mask<const N: u128>(_: IndexBackerOf<N>) -> IndexBackerOf<N>
    where
        IndexLut<N>: const HasIndexBacker,
    {
        <IndexLut<N> as HasIndexBacker>::MAX
    }

    const fn apply_mask_to<const N: u128>(val: &mut IndexBackerOf<N>)
    where
        IndexLut<N>: const HasIndexBacker,
    {
        *val = <IndexLut<N> as HasIndexBacker>::MAX;
    }

    subst_macros::repeat_parallel_subst! {
        groups: [
            [group
                [sub [TRAIT] = [SaturatingAdd]]
                [sub [FN] = [saturating_add]]
                [sub [KIND] = [noassign]]
            ]
            [group
                [sub [TRAIT] = [SaturatingSub]]
                [sub [FN] = [saturating_sub]]
                [sub [KIND] = [noassign]]
            ]
            [group
                [sub [TRAIT] = [SaturatingAddAssign]]
                [sub [FN] = [saturating_add_assign]]
                [sub [KIND] = [assign]]
            ]
            [group
                [sub [TRAIT] = [SaturatingSubAssign]]
                [sub [FN] = [saturating_sub_assign]]
                [sub [KIND] = [assign]]
            ]
        ],
        callback: NONE,
        in: {
            crate::macros::newtype_copyable_impl! {
                @KIND
                type: [
                    name: Index,
                    outerparams: [const N: u128],
                    appliedparams: [N],
                    where: [
                        IndexLut<N>: const HasIndexBacker,
                        IndexBackerOf<N>: const TRAIT<GetInnerTy<RHSTY>, Output = IndexBackerOf<N>>,
                    ],
                ],
                TRAIT::FN = TRAIT::FN
            }
        }
    }
};

#[cfg(test)]
mod test {
    use super::*;
    use rand::{distr::Uniform, prelude::*};

    #[test]
    fn test_sizes() {
        let pairs = [
            (1, 8),
            (256, 8),
            (257, 16),
            (65_536, 16),
            (65_537, 32),
            (4_294_967_296, 32),
            (4_294_967_297, 64),
            (18_446_744_073_709_551_616, 64),
            (18_446_744_073_709_551_617, 128),
            (340_282_366_920_938_463_463_374_607_431_768_211_455, 128),
        ];
        for (lim, backing) in pairs {
            let check = index_bit_size(lim);
            if check != backing {
                panic!("Unexpected bit size for 0..{lim}! Expected {backing}, found {check}");
            }
        }
    }

    #[cfg(debug_assertions)]
    const TEST_ITERS: usize = 250_000;
    #[cfg(not(debug_assertions))]
    const TEST_ITERS: usize = 25_000_000;

    subst_macros::repeat_parallel_subst! {
        groups: [
            [group [sub [TY] = [u8]]]
            [group [sub [TY] = [u16]]]
            [group [sub [TY] = [u32]]]
            [group [sub [TY] = [u64]]]
            [group [sub [TY] = [u128]]]
        ],
        callback: NONE,
        in: {
            paste::paste! {
                const [<TY:upper _LIM_U128>]: u128 = {{
                    const MOD: u128 = if TY::MAX as u128 != u8::MAX as u128 {
                        TY::MAX as u128 / 2 - 3
                    } else {
                        u8::MAX as u128 - 3
                    };
                    const BASE: u128 = if TY::MAX as u128 != u8::MAX as u128 { MOD + 3 } else { 0 };
                    let tmp = get_random_const::random!(TY) as u128 + 1;
                    let tmp = tmp % MOD;
                    BASE + 3 + tmp
                }
                };
                const [<TY:upper _LIM>]: TY = [<TY:upper _LIM_U128>] as TY - 1;
            }
        }
    }

    #[test]
    fn test_add() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        subst_macros::repeat_parallel_subst! {
            groups: [
                [group [sub [TY] = [u8]]]
                [group [sub [TY] = [u16]]]
                [group [sub [TY] = [u32]]]
                [group [sub [TY] = [u64]]]
                [group [sub [TY] = [u128]]]
            ],
            callback: NONE,
            in: {
                paste::paste! {
                    let dist = Uniform::try_from(0..=[<TY:upper _LIM>] - 2).unwrap();
                    for _ in 0..TEST_ITERS {
                        let lhs = dist.sample(&mut rng);
                        let rhs = rng.random_range(0..[<TY:upper _LIM>] - lhs);
                        let lhsi = Index::<[<TY:upper _LIM_U128>]>::new(lhs).unwrap();
                        let rhsi = Index::<[<TY:upper _LIM_U128>]>::new(rhs).unwrap();
                        let result = lhsi + rhsi;
                        let expect = Index::<[<TY:upper _LIM_U128>]>::new(lhs + rhs).unwrap();
                        if result != expect {
                            println!(
                                "type: Index<{}> (0..{})",
                                [<TY:upper _LIM_U128>],
                                [<TY:upper _LIM>],
                            );
                            println!("a = {lhs}");
                            println!("b = {rhs}");
                            println!("op: a + b");
                            println!("  result: {result}");
                            println!("expected: {expect}");
                            panic!("Test failed!");
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_sub() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        subst_macros::repeat_parallel_subst! {
            groups: [
                [group [sub [TY] = [u8]]]
                [group [sub [TY] = [u16]]]
                [group [sub [TY] = [u32]]]
                [group [sub [TY] = [u64]]]
                [group [sub [TY] = [u128]]]
            ],
            callback: NONE,
            in: {
                paste::paste! {
                    let dist = Uniform::try_from(1..=[<TY:upper _LIM>]).unwrap();
                    for _ in 0..TEST_ITERS {
                        let lhs = dist.sample(&mut rng);
                    let rhs = rng.random_range(0..lhs);
                        let lhsi = Index::<[<TY:upper _LIM_U128>]>::new(lhs).unwrap();
                        let rhsi = Index::<[<TY:upper _LIM_U128>]>::new(rhs).unwrap();
                        let result = lhsi - rhsi;
                        let expect = Index::<[<TY:upper _LIM_U128>]>::new(lhs - rhs).unwrap();
                        if result != expect {
                            println!(
                                "type: Index<{}> (0..{})",
                                [<TY:upper _LIM_U128>],
                                [<TY:upper _LIM>],
                            );
                            println!("a = {lhs}");
                            println!("b = {rhs}");
                            println!("op: a - b");
                            println!("  result: {result}");
                            println!("expected: {expect}");
                            panic!("Test failed!");
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_add_assign() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        subst_macros::repeat_parallel_subst! {
            groups: [
                [group [sub [TY] = [u8]]]
                [group [sub [TY] = [u16]]]
                [group [sub [TY] = [u32]]]
                [group [sub [TY] = [u64]]]
                [group [sub [TY] = [u128]]]
            ],
            callback: NONE,
            in: {
                paste::paste! {
                    let dist = Uniform::try_from(0..=[<TY:upper _LIM>] - 2).unwrap();
                    for _ in 0..TEST_ITERS {
                        let lhs = dist.sample(&mut rng);
                        let rhs = rng.random_range(0..[<TY:upper _LIM>] - lhs);
                        let lhsi = Index::<[<TY:upper _LIM_U128>]>::new(lhs).unwrap();
                        let rhsi = Index::<[<TY:upper _LIM_U128>]>::new(rhs).unwrap();
                        let result = lhsi + rhsi;
                        let expect = Index::<[<TY:upper _LIM_U128>]>::new(lhs + rhs).unwrap();
                        if result != expect {
                            println!(
                                "type: Index<{}> (0..{})",
                                [<TY:upper _LIM_U128>],
                                [<TY:upper _LIM>],
                            );
                            println!("a = {lhs}");
                            println!("b = {rhs}");
                            println!("op: a + b");
                            println!("  result: {result}");
                            println!("expected: {expect}");
                            panic!("Test failed!");
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_sub_assign() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        subst_macros::repeat_parallel_subst! {
            groups: [
                [group [sub [TY] = [u8]]]
                [group [sub [TY] = [u16]]]
                [group [sub [TY] = [u32]]]
                [group [sub [TY] = [u64]]]
                [group [sub [TY] = [u128]]]
            ],
            callback: NONE,
            in: {
                paste::paste! {
                    let dist = Uniform::try_from(1..=[<TY:upper _LIM>]).unwrap();
                    for _ in 0..TEST_ITERS {
                        let lhs = dist.sample(&mut rng);
                    let rhs = rng.random_range(0..lhs);
                        let lhsi = Index::<[<TY:upper _LIM_U128>]>::new(lhs).unwrap();
                        let rhsi = Index::<[<TY:upper _LIM_U128>]>::new(rhs).unwrap();
                        let result = lhsi - rhsi;
                        let expect = Index::<[<TY:upper _LIM_U128>]>::new(lhs - rhs).unwrap();
                        if result != expect {
                            println!(
                                "type: Index<{}> (0..{})",
                                [<TY:upper _LIM_U128>],
                                [<TY:upper _LIM>],
                            );
                            println!("a = {lhs}");
                            println!("b = {rhs}");
                            println!("op: a - b");
                            println!("  result: {result}");
                            println!("expected: {expect}");
                            panic!("Test failed!");
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_sat_add() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        subst_macros::repeat_parallel_subst! {
            groups: [
                [group [sub [TY] = [u8]]]
                [group [sub [TY] = [u16]]]
                [group [sub [TY] = [u32]]]
                [group [sub [TY] = [u64]]]
                [group [sub [TY] = [u128]]]
            ],
            callback: NONE,
            in: {
                paste::paste! {
                    let dist = Uniform::try_from(0..=[<TY:upper _LIM>]).unwrap();
                    for _ in 0..TEST_ITERS {
                        let lhs = dist.sample(&mut rng);
                        let rhs = rng.random_range(0..[<TY:upper _LIM>]);
                        let lhsi = Index::<[<TY:upper _LIM_U128>]>::new(lhs).unwrap();
                        let rhsi = Index::<[<TY:upper _LIM_U128>]>::new(rhs).unwrap();
                        let result = lhsi.saturating_add(rhsi);
                        let expect = Index::<[<TY:upper _LIM_U128>]>::new(
                            lhs
                                .saturating_add(rhs)
                                .clamp(0, [<TY:upper _LIM>])
                        )
                        .unwrap();
                        if result != expect {
                            println!(
                                "type: Index<{}> (0..{})",
                                [<TY:upper _LIM_U128>],
                                [<TY:upper _LIM>],
                            );
                            println!("a = {lhs}");
                            println!("b = {rhs}");
                            println!("op: a.saturating_add(b)");
                            println!("  result: {result}");
                            println!("expected: {expect}");
                            panic!("Test failed!");
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_sat_sub() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        subst_macros::repeat_parallel_subst! {
            groups: [
                [group [sub [TY] = [u8]]]
                [group [sub [TY] = [u16]]]
                [group [sub [TY] = [u32]]]
                [group [sub [TY] = [u64]]]
                [group [sub [TY] = [u128]]]
            ],
            callback: NONE,
            in: {
                paste::paste! {
                    let dist = Uniform::try_from(0..=[<TY:upper _LIM>]).unwrap();
                    for _ in 0..TEST_ITERS {
                        let lhs = dist.sample(&mut rng);
                    let rhs = rng.random_range(0..=[<TY:upper _LIM>]);
                        let lhsi = Index::<[<TY:upper _LIM_U128>]>::new(lhs).unwrap();
                        let rhsi = Index::<[<TY:upper _LIM_U128>]>::new(rhs).unwrap();
                        let result = lhsi.saturating_sub(rhsi);
                        let expect =
                            Index::<[<TY:upper _LIM_U128>]>::new(lhs.saturating_sub(rhs)).unwrap();
                        if result != expect {
                            println!(
                                "type: Index<{}> (0..{})",
                                [<TY:upper _LIM_U128>],
                                [<TY:upper _LIM>],
                            );
                            println!("a = {lhs}");
                            println!("b = {rhs}");
                            println!("op: a - b");
                            println!("  result: {result}");
                            println!("expected: {expect}");
                            panic!("Test failed!");
                        }
                    }
                }
            }
        }
    }

    #[cfg(debug_assertions)]
    const N_OTHER: u128 = get_random_const::random!(u128) % (u128::MAX - 1);

    #[cfg_attr(debug_assertions, test)]
    #[cfg_attr(debug_assertions, should_panic)]
    #[cfg(debug_assertions)]
    fn test_new_panic() {
        let _ = Index::<N_OTHER>::new(N_OTHER + 1).unwrap();
    }

    #[cfg_attr(debug_assertions, test)]
    #[cfg_attr(debug_assertions, should_panic)]
    #[cfg(debug_assertions)]
    fn test_add_panic() {
        let a = Index::<N_OTHER>::MAX;
        let b = Index::<N_OTHER>::MAX;
        let _ = a + b;
    }

    #[cfg_attr(debug_assertions, test)]
    #[cfg_attr(debug_assertions, should_panic)]
    #[cfg(debug_assertions)]
    fn test_sub_panic() {
        let a = Index::<N_OTHER>::MIN;
        let b = Index::<N_OTHER>::MAX;
        let _ = a - b;
    }
}
