// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
//! Provides and implements the functionality for a [`BitVec<N>`] type

use crate::{ConstCheck, True};
use const_panic::concat_panic;
use core::{
    cmp::Ordering,
    ops::{BitAndAssign, Not},
};

pub const fn bv_size(n: usize) -> usize {
    if n == 0 {
        panic!("Cannot have a bitvector represent 0 bits!");
    } else {
        n.div_ceil(8)
    }
}

/// A fixed-width vector of `N` bits
///
/// # Backing types
///
/// This type makes use of various backing types depending on the size `N` of the input.
/// Specifically, these are the following ranges of `N` and the backing types they correspond to:
/// - `1..=8`: `u8`
/// - `9..=16`: `u16`
/// - `17..=32`: `u32`
/// - `33..=64`: `u64`
/// - `65..=128`: `u128`
/// - `129..=65535`: `[u8; N.div_ceil(8) as usize]`
///
/// For example, a `BitVec<7>` is equivalent to a `u8`, but a `BitVec<510>` is equivalent to a
/// `[u8; 64]`.
///
/// # How to make generic implementations on this type
///
/// In order to make `impl`s on this type that are generic over `N`, you must add the following
/// `where` clause:
/// ```no_run
/// #![feature(const_trait_impl, generic_const_exprs)]
/// #![allow(incomplete_features)]
///
/// use clash_num::bitvec::{bv_size, BitVec};
///
/// pub trait Marker {}
///
/// impl<const N: usize> Marker for BitVec<N>
/// where
///     [(); bv_size(N)]:,
/// {
/// }
/// ```
///
/// # How to implement functions/methods on this type
///
/// Generally, the way to implement new functionality for this type is to create a wrapper trait
/// for the functionality you desire and then implement it for each of the following types:
/// - `u8`
/// - `u16`
/// - `u32`
/// - `u64`
/// - `u128`
/// - `[u8; N]` (with `const N: usize`)
/// - `usize` (optional)
///
/// For instance, the implementation of `BitAnd` on this type is done by creating a `BitVecBitAnd`
/// trait and implementing it for all of the listed types, and then implementing `BitAnd` in terms
/// of that wrapper trait:
/// ```ignore
/// #![feature(const_trait_impl, generic_const_exprs)]
/// # use clash_num::bitvec::{BitVec, BitVecLut, BVBackerOf, HasBVBacker};
///
/// pub trait BitVecBitAnd<Rhs = Self> {
///     type Output;
///
///     fn bitvec_bitand(self, rhs: Rhs) -> Self::Output;
/// }
///
/// impl BitVecBitAnd<u8> for u8 {
///     type Output = <u8 as BitAnd<u8>>::Output;
///
///     fn bitvec_bitand(self, rhs: u8) -> Self::Output {
///         self.bitand(rhs)
///     }
/// }
///
/// // other `impl`s elided for brevity
///
/// impl<const N: u16> core::ops::BitAnd<BitVec<N>> for BitVec<N>
/// where
///     BitVecLut<N>: const HasBVBacker,
///     BVBackerOf<N>: BitVecBitAnd<BVBackerOf<N>, Output = BVBackerOf<N>>,
/// {
///     type Output = BitVec<N>;
///
///     fn bitand(self, rhs: BitVec<N>) -> Self::Output {
///         BitVec(apply_mask(self.0.bitvec_bitand(rhs.0)))
///     }
/// }
///
/// impl<'a, const N: u16> core::ops::BitAnd<&'a BitVec<N>> for BitVec<N>
/// where
///     BitVecLut<N>: const HasBVBacker,
///     BVBackerOf<N>: BitVecBitAnd<&'a BVBackerOf<N>, Output = BVBackerOf<N>>,
/// {
///     type Output = BitVec<N>;
///
///     fn bitand(self, rhs: BitVec<N>) -> Self::Output {
///         BitVec(apply_mask(self.0.bitvec_bitand(&rhs.0)))
///     }
/// }
///
/// // other `impl`s elided for brevity
/// ```
#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct BitVec<const N: usize>(pub(super) [u8; bv_size(N)])
where
    [(); bv_size(N)]:;

impl<const N: usize> BitVec<N>
where
    [(); bv_size(N)]:,
{
    const MIN: [u8; bv_size(N)] = [0; _];
    const MAX: [u8; bv_size(N)] = {
        let mut out = [!0; _];
        *out.last_mut().unwrap() = Self::MASK;
        out
    };
    const MASK: u8 = {
        if N.is_multiple_of(8) {
            !0
        } else {
            !(!0 << (N % 8))
        }
    };
    const fn apply_mask_to(val: &mut [u8; bv_size(N)]) {
        unsafe {
            val.last_mut().unwrap_unchecked().bitand_assign(Self::MASK);
        }
    }
    const fn apply_mask(val: [u8; bv_size(N)]) -> [u8; bv_size(N)] {
        let mut val = val;
        Self::apply_mask_to(&mut val);
        val
    }
    const fn bounds_check(val: &[u8; bv_size(N)]) -> bool {
        unsafe { Self::MASK.ge(val.last().unwrap_unchecked()) }
    }

    /// Make an instance of `BitVec<N>` given an input of its backing type.
    ///
    /// # Example
    /// ```
    /// # #![feature(generic_const_exprs)]
    /// # #![allow(incomplete_features)]
    /// # use clash_num::bitvec::BitVec;
    /// let a = BitVec::<8>::try_from(b'A').unwrap();
    /// let b = BitVec::<8>::try_from(0b0010_0000u8).unwrap();
    /// let c = BitVec::<8>::try_from(b'a').unwrap();
    /// assert_eq!(a | b, c);
    /// ```
    ///
    /// Returns `None` if the input is out of bounds for a `BitVec<N>`. For instance:
    /// ```
    /// # #![feature(generic_const_exprs)]
    /// # #![allow(incomplete_features)]
    /// # use clash_num::bitvec::BitVec;
    /// assert!(BitVec::<3>::try_from(0b1111u8).is_err());
    /// assert!(BitVec::<3>::try_from(0b111u8).is_ok());
    /// ```
    pub const fn new(val: [u8; bv_size(N)]) -> Option<Self> {
        if Self::bounds_check(&val) {
            Some(BitVec(val))
        } else {
            None
        }
    }

    /// Create an instance of `BitVec<N>` with `N` 0s.
    ///
    /// # Example
    /// ```
    /// # #![feature(generic_const_exprs)]
    /// # #![allow(incomplete_features)]
    /// # use clash_num::bitvec::BitVec;
    /// assert_eq!(BitVec::<1024>::min(), BitVec::new([0; 128]).unwrap());
    /// ```
    pub const fn min() -> Self {
        BitVec(Self::MIN)
    }

    /// Creates an instance of `BitVec<N>` with `N` 1s.
    ///
    /// # Example
    /// ```
    /// # #![feature(generic_const_exprs)]
    /// # #![allow(incomplete_features)]
    /// # use clash_num::bitvec::BitVec;
    /// assert_eq!(BitVec::<17>::max(), BitVec::try_from(0x1_ffffu32).unwrap());
    /// ```
    pub const fn max() -> Self {
        BitVec(Self::MAX)
    }

    /// Creates an instance of `BitVec<N>` with `M` 1s.
    ///
    /// # Example
    /// ```
    /// # #![feature(generic_const_exprs)]
    /// # #![allow(incomplete_features)]
    /// # use clash_num::bitvec::BitVec;
    /// assert_eq!(BitVec::<36>::const_ones::<10>(), BitVec::try_from(0x3ffu16).unwrap());
    /// ```
    pub const fn const_ones<const M: usize>() -> Self
    where
        ConstCheck<{ M <= N }>: True,
    {
        let mut out = [0; _];
        let nbytes = M / 8;
        let mut i = 0;
        while i < nbytes {
            out[i] = !0;
            i += 1;
        }
        let extra = M % 8;
        if extra != 0 {
            out[i] = !0 >> (8 - extra);
        }
        BitVec(out)
    }

    /// Creates an instance of `BitVec<N>` with a dynamically provided number of 1s.
    ///
    /// Produces `None` if `n > N`.
    ///
    /// # Example
    /// ```
    /// # #![feature(generic_const_exprs)]
    /// # #![allow(incomplete_features)]
    /// # use clash_num::bitvec::BitVec;
    /// for ones in 0..=32 {
    ///     assert_eq!(BitVec::<32>::ones(ones).unwrap().trailing_ones(), ones);
    /// }
    /// ```
    pub const fn ones(n: usize) -> Option<Self> {
        if n <= N {
            let mut out = [0; bv_size(N)];
            if n > 0 {
                let nbytes = n / 8;
                let mut i = 0;
                while i < nbytes {
                    out[i] = !0;
                    i += 1;
                }
                if i < bv_size(N) {
                    let extra = n % 8;
                    if extra != 0 {
                        out[i] = !0 >> (8 - extra);
                    }
                }
            }
            Some(BitVec(out))
        } else {
            None
        }
    }

    #[doc(hidden)]
    pub const unsafe fn new_unchecked(val: [u8; bv_size(N)]) -> Self {
        BitVec(val)
    }

    pub const fn trailing_ones(&self) -> usize {
        let mut count = 0;
        let mut i = 0;
        while i < bv_size(N) {
            let val = self.0[i];
            if val == !0 {
                count += 8;
                i += 1;
            } else {
                return count + val.trailing_ones() as usize;
            }
        }
        count
    }

    pub const fn trailing_zeros(&self) -> usize {
        let mut count = 0;
        let mut i = 0;
        while i < bv_size(N) {
            let val = self.0[i];
            if val == 0 {
                count += 8;
                i += 1;
            } else {
                return count + val.trailing_zeros() as usize;
            }
        }
        count
    }

    pub const fn leading_ones(&self) -> usize {
        let mut count = 0;
        let mut idx = Some(const { bv_size(N) - 1 });
        while let Some(i) = idx {
            let val = self.0[i];
            if val == !0 {
                count += 8;
                idx = i.checked_sub(1);
            } else {
                count += val.leading_ones() as usize;
                break;
            }
        }
        count
    }

    pub const fn leading_zeros(&self) -> usize {
        let mut count = 0;
        let mut idx = Some(const { bv_size(N) - 1 });
        while let Some(i) = idx {
            let val = self.0[i];
            if val == 0 {
                count += 8;
                idx = i.checked_sub(1);
            } else {
                count += val.leading_zeros() as usize;
                break;
            }
        }
        count
    }

    /// Gives the inner value of a `BitVec<N>` instance.
    ///
    /// # Example
    /// ```
    /// # #![feature(generic_const_exprs)]
    /// # #![allow(incomplete_features)]
    /// # use clash_num::bitvec::BitVec;
    /// assert_eq!(u32::from(BitVec::<24>::max()), 0xff_ffffu32);
    /// ```
    pub const fn into_inner(self) -> [u8; bv_size(N)] {
        self.0
    }
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
    callback: NONE,
    in: {
        impl<const N: usize> const TryFrom<TYPE> for BitVec<N>
        where
            [(); bv_size(N)]:,
        {
            type Error = ();

            fn try_from(other: TYPE) -> Result<Self, Self::Error> {
                if bv_size(N) > core::mem::size_of::<TYPE>() {
                    let mut out = [0; _];
                    unsafe {
                        out.as_mut_ptr()
                            .cast::<TYPE>()
                            .write_unaligned(other.to_le());
                    }
                    Ok(BitVec(BitVec::apply_mask(out)))
                } else {
                    if (TYPE::BITS - other.leading_zeros()) as usize > N {
                        Err(())
                    } else {
                        let mut out = [0; _];
                        out[..].copy_from_slice(&other.to_le_bytes()[..bv_size(N)]);
                        Ok(BitVec(BitVec::apply_mask(out)))
                    }
                }
            }
        }

        impl<const N: usize> const From<BitVec<N>> for TYPE
        where
            [(); bv_size(N)]:,
            ConstCheck<{ N <= TYPE::BITS as usize }>: True,
        {
            fn from(other: BitVec<N>) -> Self {
                let mut out = [0; _];
                out[0..bv_size(N)].copy_from_slice(&other.0);
                TYPE::from_le_bytes(out)
            }
        }
    }
}

const fn bv_cmp<const N: usize>(lhs: &[u8; N], rhs: &[u8; N]) -> Ordering {
    let mut i = const { N - 1 };
    while i > 0 {
        match lhs[i].cmp(&rhs[i]) {
            Ordering::Equal => i -= 1,
            other => return other,
        }
    }
    lhs[0].cmp(&rhs[0])
}

#[allow(clippy::non_canonical_partial_ord_impl)]
impl<const N: usize> const PartialOrd for BitVec<N>
where
    [(); bv_size(N)]:,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(bv_cmp(&self.0, &other.0))
    }
}

impl<const N: usize> const Ord for BitVec<N>
where
    [(); bv_size(N)]:,
{
    fn cmp(&self, other: &Self) -> Ordering {
        bv_cmp(&self.0, &other.0)
    }
}

impl<const N: usize> const PartialEq for BitVec<N>
where
    [(); bv_size(N)]:,
{
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<const N: usize> const Eq for BitVec<N> where [(); bv_size(N)]: {}

impl<const N: usize> const Not for BitVec<N>
where
    [(); bv_size(N)]:,
{
    type Output = Self;

    fn not(self) -> Self::Output {
        let mut out = self.0;
        let mut i = 0;
        while i < const { N.div_ceil(8) } {
            out[i] = !out[i];
            i += 1;
        }
        BitVec(BitVec::apply_mask(out))
    }
}

// Trivial non-assigning operator impls
subst_macros::repeat_parallel_subst! {
    groups: [
        [group [sub [TRAIT] = [core::ops::BitAnd]] [sub [FN] = [bitand]] [sub [OP] = [&]]]
        [group [sub [TRAIT] = [core::ops::BitOr]] [sub [FN] = [bitor]] [sub [OP] = [|]]]
        [group [sub [TRAIT] = [core::ops::BitXor]] [sub [FN] = [bitxor]] [sub [OP] = [^]]]
    ],
    callback: [
        macro: subst_macros::repeat_parallel_subst,
        prefix: [
            @callback
            groups: [
                [group
                    [sub [GEN] = [const N: usize]]
                    [sub [LHSTY] = [BitVec<N>]]
                    [sub [RHSTY] = [BitVec<N>]]
                ]
                [group
                    [sub [GEN] = ['a, const N: usize]]
                    [sub [LHSTY] = [BitVec<N>]]
                    [sub [RHSTY] = [&'a BitVec<N>]]
                ]
                [group
                    [sub [GEN] = ['a, const N: usize]]
                    [sub [LHSTY] = [&'a BitVec<N>]]
                    [sub [RHSTY] = [BitVec<N>]]
                ]
                [group
                    [sub [GEN] = ['a, 'b, const N: usize]]
                    [sub [LHSTY] = [&'a BitVec<N>]]
                    [sub [RHSTY] = [&'b BitVec<N>]]
                ]
            ],
            callback: NONE,
        ],
        suffix: [],
    ],
    in: {
        impl<GEN> TRAIT<RHSTY> for LHSTY
        where
            [(); bv_size(N)]:,
        {
            type Output = BitVec<N>;

            fn FN(self, rhs: RHSTY) -> Self::Output {
                let mut out = [0; _];
                let mut i = 0;
                while i < bv_size(N) {
                    out[i] = self.0[i] OP rhs.0[i];
                    i += 1;
                }
                BitVec(BitVec::apply_mask(out))
            }
        }
    }
}

// Trivial assigning operator impls
subst_macros::repeat_parallel_subst! {
    groups: [
        [group [sub [TRAIT] = [core::ops::BitAndAssign]] [sub [FN] = [bitand_assign]] [sub [OP] = [&=]]]
        [group [sub [TRAIT] = [core::ops::BitOrAssign]] [sub [FN] = [bitor_assign]] [sub [OP] = [|=]]]
        [group [sub [TRAIT] = [core::ops::BitXorAssign]] [sub [FN] = [bitxor_assign]] [sub [OP] = [^=]]]
    ],
    callback: [
        macro: subst_macros::repeat_parallel_subst,
        prefix: [
            @callback
            groups: [
                [group
                    [sub [GEN] = [const N: usize]]
                    [sub [LHSTY] = [BitVec<N>]]
                    [sub [RHSTY] = [BitVec<N>]]
                ]
                [group
                    [sub [GEN] = ['a, const N: usize]]
                    [sub [LHSTY] = [BitVec<N>]]
                    [sub [RHSTY] = [&'a BitVec<N>]]
                ]
            ],
            callback: NONE,
        ],
        suffix: [],
    ],
    in: {
        impl<GEN> TRAIT<RHSTY> for LHSTY
        where
            [(); bv_size(N)]:,
        {
            fn FN(&mut self, rhs: RHSTY) {
                let mut i = 0;
                while i < bv_size(N) {
                    self.0[i] OP rhs.0[i];
                    i += 1;
                }
                BitVec::apply_mask_to(&mut self.0);
            }
        }
    }
}

const trait ShiftCheck<const N: usize> {
    fn check_shift(&self) -> bool;
    /// Should only be called if `shift_check` returns `true`.
    fn shift_parts(&self) -> (usize, u8);
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
    callback: NONE,
    in: {
        #[allow(clippy::unnecessary_cast)]
        impl<const N: usize> const ShiftCheck<N> for TYPE {
            fn check_shift(&self) -> bool {
                if N <= TYPE::BITS as usize {
                    *self < N as TYPE
                } else {
                    (*self as usize) < N
                }
            }

            fn shift_parts(&self) -> (usize, u8) {
                let n = *self as usize;
                (n / 8, (n % 8) as u8)
            }
        }

        #[allow(clippy::unnecessary_cast)]
        impl<const N: usize> const ShiftCheck<N> for &TYPE {
            fn check_shift(&self) -> bool {
                if N <= TYPE::BITS as usize {
                    **self < N as TYPE
                } else {
                    (**self as usize) < N
                }
            }

            fn shift_parts(&self) -> (usize, u8) {
                let n = **self as usize;
                (n / 8, (n % 8) as u8)
            }
        }
    }
}

const fn check_shift<const N: usize>(arr: &[u8; bv_size(N)]) -> bool
where
    [(); bv_size(N)]:,
{
    if N <= usize::BITS as usize {
        let mut bytes = [0; size_of::<usize>()];
        bytes[..bv_size(N)].copy_from_slice(&arr[..]);
        usize::from_le_bytes(bytes) < N
    } else {
        let mut i = const { bv_size(N) - 1 };
        while i > size_of::<usize>() {
            if arr[i] != 0 {
                return false;
            } else {
                i -= 1;
            }
        }
        let mut bytes = [0; size_of::<usize>()];
        bytes[..].copy_from_slice(&arr[0..size_of::<usize>()]);
        usize::from_le_bytes(bytes) < N
    }
}

const fn get_loffset<const N: usize>(arr: &[u8; N]) -> usize {
    if const { N <= size_of::<usize>() } {
        let mut bytes = [0; _];
        bytes[0..N].copy_from_slice(arr.as_slice());
        usize::from_le_bytes(bytes) >> 3
    } else {
        #[cfg(target_endian = "little")]
        unsafe {
            const IDX: usize = size_of::<usize>();
            const SFT: usize = usize::BITS as usize - 3;
            let n = arr.as_ptr().cast::<usize>().read_unaligned() >> 3;
            n | (((arr[IDX] as usize) & 0x7) << SFT)
        }
        #[cfg(target_endian = "big")]
        unsafe {
            const IDX: usize = size_of::<usize>() + 1;
            const SFT: usize = usize::BITS as usize - 3;
            let n = arr.as_ptr().cast::<usize>().read_unaligned().swap_bytes() >> 3;
            n | (((arr[IDX] as usize) & 0x7) << SFT)
        }
    }
}

impl<const N: usize> const ShiftCheck<N> for BitVec<N>
where
    [(); bv_size(N)]:,
{
    fn check_shift(&self) -> bool {
        check_shift(&self.0)
    }

    fn shift_parts(&self) -> (usize, u8) {
        (get_loffset(&self.0), self.0[0] & 0x7)
    }
}

impl<const N: usize> const ShiftCheck<N> for &BitVec<N>
where
    [(); bv_size(N)]:,
{
    fn check_shift(&self) -> bool {
        check_shift(&self.0)
    }

    fn shift_parts(&self) -> (usize, u8) {
        (get_loffset(&self.0), self.0[0] & 0x7)
    }
}

const fn shl_inner<const N: usize>(lhs: &[u8; N], loffset: usize, lshift: u8) -> [u8; N] {
    let mut out = [0; _];
    if lshift != 0 {
        // Have to do carry maths
        let mut carry = 0;
        let mut i = 0;
        #[cfg(target_endian = "little")]
        while i + loffset < N {
            [out[i + loffset], carry] = (((lhs[i] as u16) << lshift) | carry as u16).to_ne_bytes();
            i += 1;
        }
        #[cfg(target_endian = "big")]
        while i + loffset < N {
            [carry, out[i + loffset]] = (((lhs[i] as u16) << lshift) | carry as u16).to_ne_bytes();
            i += 1;
        }
    } else {
        // Don't have to do carry maths
        let mut i = 0;
        while i + loffset < N {
            out[i + loffset] = lhs[i];
            i += 1;
        }
    }
    out
}

impl<T, const N: usize> core::ops::Shl<T> for BitVec<N>
where
    [(); bv_size(N)]:,
    T: ShiftCheck<N>,
{
    type Output = BitVec<N>;

    fn shl(self, rhs: T) -> Self::Output {
        if rhs.check_shift() {
            let (loffset, lshift) = rhs.shift_parts();
            BitVec(BitVec::apply_mask(shl_inner(&self.0, loffset, lshift)))
        } else if cfg!(debug_assertions) {
            concat_panic!(
                "Attempt to shift left with overflow on type BitVec<",
                N,
                ">"
            );
        } else {
            BitVec([0; _])
        }
    }
}

impl<T, const N: usize> core::ops::Shl<T> for &BitVec<N>
where
    [(); bv_size(N)]:,
    T: ShiftCheck<N>,
{
    type Output = BitVec<N>;

    fn shl(self, rhs: T) -> Self::Output {
        if rhs.check_shift() {
            let (loffset, lshift) = rhs.shift_parts();
            BitVec(BitVec::apply_mask(shl_inner(&self.0, loffset, lshift)))
        } else if cfg!(debug_assertions) {
            concat_panic!(
                "Attempt to shift left with overflow on type BitVec<",
                N,
                ">"
            );
        } else {
            BitVec([0; _])
        }
    }
}

const fn shl_assign_inner<const N: usize>(lhs: &mut [u8; N], loffset: usize, lshift: u8) {
    if lshift != 0 {
        // Have to do carry maths
        let mut carry = lhs[const { N - 1 } - loffset] << lshift;
        let loffsetp1 = loffset + 1;
        let mut idx = const { N - 1 }.checked_sub(loffsetp1);
        #[cfg(target_endian = "little")]
        while let Some(i) = idx {
            [carry, lhs[i + loffsetp1]] =
                (((lhs[i] as u16) << lshift) | u16::from_ne_bytes([0, carry])).to_ne_bytes();
            idx = i.checked_sub(1);
        }
        #[cfg(target_endian = "big")]
        while let Some(i) = idx {
            [lhs[i + loffsetp1], carry] =
                (((lhs[i] as u16) << lshift) | u16::from_ne_bytes([carry, 0])).to_ne_bytes();
            idx = i.checked_sub(1);
        }
        lhs[loffset] = lhs[0] << lshift;
    } else if loffset != 0 {
        // Don't have to do carry maths
        let mut idx = const { N - 1 }.checked_sub(loffset);
        while let Some(i) = idx {
            lhs[i + loffset] = lhs[i];
            idx = i.checked_sub(1);
        }
    } // No `else`, since it would be a no-op.
    // Zero out the lower order words in `self` that should be filled in with zeros from the
    // shifting.
    if loffset > 0 {
        let mut i = 0;
        while i < loffset {
            lhs[i] = 0;
            i += 1;
        }
    }
}

impl<T, const N: usize> core::ops::ShlAssign<T> for BitVec<N>
where
    [(); bv_size(N)]:,
    T: ShiftCheck<N>,
{
    fn shl_assign(&mut self, rhs: T) {
        if rhs.check_shift() {
            let (loffset, lshift) = rhs.shift_parts();
            shl_assign_inner(&mut self.0, loffset, lshift);
            BitVec::apply_mask_to(&mut self.0);
        } else if cfg!(debug_assertions) {
            concat_panic!(
                "Attempt to shift left with overflow on type BitVec<",
                N,
                ">"
            );
        } else {
            self.0 = [0; _];
        }
    }
}

const fn shr_inner<const N: usize>(lhs: &[u8; N], loffset: usize, rshift: u8) -> [u8; N] {
    let mut out = [0; _];
    if rshift != 0 {
        // Have to do carry maths
        let mut i = 0;
        let mut carry = lhs[loffset] >> rshift;
        let loffsetp1 = loffset + 1;
        let lastchange = const { N - 1 }.saturating_sub(loffset);
        #[cfg(target_endian = "little")]
        while i < lastchange {
            let tmp = u16::from_ne_bytes([0, lhs[i + loffsetp1]]) >> rshift;
            [out[i], carry] = (tmp | carry as u16).to_ne_bytes();
            i += 1;
        }
        #[cfg(target_endian = "big")]
        while i < lastchange {
            let tmp = u16::from_ne_bytes([lhs[i + loffset], 0]) >> rshift;
            [carry, out[i]] = (tmp | carry as u16).to_ne_bytes();
            i += 1;
        }
        out[lastchange] = carry;
    } else {
        // Don't have to do carry maths
        let mut i = 0;
        while i + loffset < N {
            out[i] = lhs[i + loffset];
            i += 1;
        }
    }
    out
}

impl<T, const N: usize> core::ops::Shr<T> for BitVec<N>
where
    [(); bv_size(N)]:,
    T: ShiftCheck<N>,
{
    type Output = BitVec<N>;

    fn shr(self, rhs: T) -> Self::Output {
        if rhs.check_shift() {
            let (loffset, rshift) = rhs.shift_parts();
            BitVec(shr_inner(&self.0, loffset, rshift))
        } else if cfg!(debug_assertions) {
            concat_panic!(
                "Attempt to shift right with overflow on type BitVec<",
                N,
                ">"
            );
        } else {
            BitVec([0; _])
        }
    }
}

impl<T, const N: usize> core::ops::Shr<T> for &BitVec<N>
where
    [(); bv_size(N)]:,
    T: ShiftCheck<N>,
{
    type Output = BitVec<N>;

    fn shr(self, rhs: T) -> Self::Output {
        if rhs.check_shift() {
            let (loffset, rshift) = rhs.shift_parts();
            BitVec(BitVec::apply_mask(shr_inner(&self.0, loffset, rshift)))
        } else if cfg!(debug_assertions) {
            concat_panic!(
                "Attempt to shift right with overflow on type BitVec<",
                N,
                ">"
            );
        } else {
            BitVec([0; _])
        }
    }
}

const fn shr_assign_inner<const N: usize>(lhs: &mut [u8; N], loffset: usize, rshift: u8) {
    if rshift != 0 {
        // Have to do carry maths
        let mut i = 0;
        let mut carry = lhs[loffset] >> rshift;
        let loffsetp1 = loffset + 1;
        let lastchange = const { N - 1 }.saturating_sub(loffset);
        #[cfg(target_endian = "little")]
        while i < lastchange {
            let tmp = u16::from_ne_bytes([0, lhs[i + loffsetp1]]) >> rshift;
            [lhs[i], carry] = (tmp | carry as u16).to_ne_bytes();
            i += 1;
        }
        #[cfg(target_endian = "big")]
        while i < lastchange {
            let tmp = u16::from_ne_bytes([lhs[i + loffsetp1], 0]) >> rshift;
            [carry, lhs[i]] = (tmp | carry as u16).to_ne_bytes();
            i += 1;
        }
        lhs[lastchange] = carry;
    } else if loffset != 0 {
        // Don't have to do carry maths
        let mut i = 0;
        while i + loffset < N {
            lhs[i] = lhs[i + loffset];
            i += 1;
        }
    } // No `else`, since it would be a no-op.
    // Zero out the higher order words in `self` that should be filled in with zeros from
    // the shifting.
    if loffset > 0 {
        let mut i = N - loffset;
        while i < N {
            lhs[i] = 0;
            i += 1;
        }
    }
}

impl<T, const N: usize> core::ops::ShrAssign<T> for BitVec<N>
where
    [(); bv_size(N)]:,
    T: ShiftCheck<N>,
{
    fn shr_assign(&mut self, rhs: T) {
        if rhs.check_shift() {
            let (loffset, rshift) = rhs.shift_parts();
            shr_assign_inner(&mut self.0, loffset, rshift);
            BitVec::apply_mask_to(&mut self.0);
        } else if cfg!(debug_assertions) {
            concat_panic!(
                "Attempt to shift right with overflow on type BitVec<",
                N,
                ">"
            );
        } else {
            self.0 = [0; _];
        }
    }
}

const unsafe fn byte_to_char_upper(b: u8) -> char {
    match b {
        0x0..=0x9 => (b'0' + b) as char,
        0xa..=0xf => (b'A' + b - 0xa) as char,
        _ => unsafe { core::hint::unreachable_unchecked() },
    }
}

const unsafe fn byte_to_char_lower(b: u8) -> char {
    match b {
        0x0..=0x9 => (b'0' + b) as char,
        0xa..=0xf => (b'a' + b - 0xa) as char,
        _ => unsafe { core::hint::unreachable_unchecked() },
    }
}

fn write_array_upper<const N: usize>(
    f: &mut core::fmt::Formatter<'_>,
    arr: &[u8; bv_size(N)],
) -> core::fmt::Result
where
    [(); bv_size(N)]:,
{
    let msbyte = arr[const { bv_size(N) - 1 }];
    if BitVec::<N>::MASK.count_ones() > 4 {
        write!(f, "{}", unsafe { byte_to_char_upper(msbyte >> 4) })?;
    }
    write!(f, "{}", unsafe { byte_to_char_upper(msbyte & 0xf) })?;
    for byte in arr[..const { bv_size(N) - 1 }].iter().rev() {
        write!(f, "{}", unsafe { byte_to_char_upper(byte >> 4) })?;
        write!(f, "{}", unsafe { byte_to_char_upper(byte & 0xf) })?;
    }
    Ok(())
}

impl<const N: usize> core::fmt::Debug for BitVec<N>
where
    [(); bv_size(N)]:,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "BitVec<{N}>(0x")?;
        write_array_upper(f, &self.0)?;
        write!(f, ")")
    }
}

impl<const N: usize> core::fmt::Display for BitVec<N>
where
    [(); bv_size(N)]:,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write_array_upper(f, &self.0)
    }
}

impl<const N: usize> core::fmt::LowerHex for BitVec<N>
where
    [(); bv_size(N)]:,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "0x")?;
        let msbyte = self.0[const { bv_size(N) - 1 }];
        if BitVec::<N>::MASK.count_ones() > 4 {
            write!(f, "{}", unsafe { byte_to_char_lower(msbyte >> 4) })?;
        }
        write!(f, "{}", unsafe { byte_to_char_lower(msbyte & 0xf) })?;
        for byte in self.0[..const { bv_size(N) - 1 }].iter().rev() {
            write!(f, "{}", unsafe { byte_to_char_lower(byte >> 4) })?;
            write!(f, "{}", unsafe { byte_to_char_lower(byte & 0xf) })?;
        }
        Ok(())
    }
}

impl<const N: usize> core::fmt::UpperHex for BitVec<N>
where
    [(); bv_size(N)]:,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "0x")?;
        write_array_upper(f, &self.0)?;
        Ok(())
    }
}

#[cfg(feature = "ufmt")]
const _: () = {
    fn uwrite_array_upper<const N: usize, W>(
        f: &mut ufmt::Formatter<'_, W>,
        arr: &[u8; bv_size(N)],
    ) -> Result<(), W::Error>
    where
        [(); bv_size(N)]:,
        W: ufmt::uWrite + ?Sized,
    {
        let msbyte = arr[const { bv_size(N) - 1 }];
        if BitVec::<N>::MASK.count_ones() > 4 {
            ufmt::uwrite!(f, "{}", unsafe { byte_to_char_upper(msbyte >> 4) })?;
        }
        ufmt::uwrite!(f, "{}", unsafe { byte_to_char_upper(msbyte & 0xf) })?;
        for byte in arr[..const { bv_size(N) - 1 }].iter().rev() {
            ufmt::uwrite!(f, "{}", unsafe { byte_to_char_upper(byte >> 4) })?;
            ufmt::uwrite!(f, "{}", unsafe { byte_to_char_upper(byte & 0xf) })?;
        }
        Ok(())
    }

    impl<const N: usize> ufmt::uDebug for BitVec<N>
    where
        [(); bv_size(N)]:,
    {
        fn fmt<W>(&self, f: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
        where
            W: ufmt::uWrite + ?Sized,
        {
            ufmt::uwrite!(f, "BitVec<{}>(0x", N)?;
            uwrite_array_upper(f, &self.0)?;
            ufmt::uwrite!(f, ")")
        }
    }

    impl<const N: usize> ufmt::uDisplay for BitVec<N>
    where
        [(); bv_size(N)]:,
    {
        fn fmt<W>(&self, f: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
        where
            W: ufmt::uWrite + ?Sized,
        {
            uwrite_array_upper(f, &self.0)
        }
    }

    impl<const N: usize> ufmt::uDisplayHex for BitVec<N>
    where
        [(); bv_size(N)]:,
    {
        fn fmt_hex<W>(
            &self,
            f: &mut ufmt::Formatter<'_, W>,
            options: ufmt::HexOptions,
        ) -> Result<(), W::Error>
        where
            W: ufmt::uWrite + ?Sized,
        {
            let pad_before = options.ox_prefix && options.pad_char == b' ';
            let pad = options
                .pad_length
                .saturating_sub(if options.ox_prefix { 2 } else { 0 })
                .saturating_sub(const { N.div_ceil(4) });
            if pad_before {
                for _ in 0..pad {
                    ufmt::uwrite!(f, "{}", options.pad_char as char)?;
                }
                match [options.ox_prefix, options.upper_case] {
                    [true, true] => ufmt::uwrite!(f, "0X")?,
                    [true, false] => ufmt::uwrite!(f, "0x")?,
                    _ => {}
                }
            } else {
                for _ in 0..pad {
                    ufmt::uwrite!(f, "{}", options.pad_char as char)?;
                }
                match [options.ox_prefix, options.upper_case] {
                    [true, true] => ufmt::uwrite!(f, "0X")?,
                    [true, false] => ufmt::uwrite!(f, "0x")?,
                    _ => {}
                }
            }
            let to_char = if options.upper_case {
                byte_to_char_upper
            } else {
                byte_to_char_lower
            };
            let msbyte = self.0[const { bv_size(N) - 1 }];
            if BitVec::<N>::MASK.count_ones() > 4 {
                ufmt::uwrite!(f, "{}", unsafe { to_char(msbyte >> 4) })?;
            }
            ufmt::uwrite!(f, "{}", unsafe { to_char(msbyte & 0xf) })?;
            for byte in self.0[..const { bv_size(N) - 1 }].iter().rev() {
                ufmt::uwrite!(f, "{}", unsafe { to_char(byte >> 4) })?;
                ufmt::uwrite!(f, "{}", unsafe { to_char(byte & 0xf) })?;
            }
            Ok(())
        }
    }
};

#[cfg(test)]
mod test {
    use super::*;
    use rand::{distr::Uniform, prelude::*};
    use std::fmt::Write;

    #[cfg(debug_assertions)]
    const FMT_TEST_ITERS: usize = 1_000_000;
    #[cfg(not(debug_assertions))]
    const FMT_TEST_ITERS: usize = 10_000_000;

    #[test]
    fn test_fmt() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        let mut buf_lhs = String::with_capacity(34);
        let mut buf_rhs = String::with_capacity(34);
        for _ in 0..FMT_TEST_ITERS {
            let num = rng.random::<u128>();
            write!(buf_lhs, "0x{num:032X}").unwrap();
            write!(buf_rhs, "0x{}", BitVec::<128>::try_from(num).unwrap()).unwrap();
            if buf_lhs != buf_rhs {
                println!("n = 0x{num:032X} ({num})");
                println!("op: format");
                println!("  result: {buf_rhs}");
                println!("expected: {buf_lhs}");
                panic!("Test failed!");
            }
            buf_lhs.clear();
            buf_rhs.clear();
        }
    }

    #[cfg(debug_assertions)]
    const TEST_ITERS: usize = 1_000_000;
    #[cfg(not(debug_assertions))]
    const TEST_ITERS: usize = 100_000_000;

    #[test]
    fn test_bitvec_bitand() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        for _ in 0..TEST_ITERS {
            let lhs = rng.random::<u128>();
            let rhs = rng.random::<u128>();
            let lhs_bv = BitVec::<128>::try_from(lhs).unwrap();
            let rhs_bv = BitVec::<128>::try_from(rhs).unwrap();
            let result = lhs_bv & rhs_bv;
            let expect = BitVec::<128>::try_from(lhs & rhs).unwrap();
            if result != expect {
                println!("a = 0x{lhs:032x}");
                println!("b = 0x{rhs:032x}");
                println!("op: a & b");
                println!("  result: {result}");
                println!("expected: {expect}");
                panic!("Test failed");
            }
        }
    }

    #[test]
    fn test_bitand_assign() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        for _ in 0..TEST_ITERS {
            let lhs = rng.random::<u128>();
            let rhs = rng.random::<u128>();
            let mut lhs_bv = BitVec::<128>::try_from(lhs).unwrap();
            let rhs_bv = BitVec::<128>::try_from(rhs).unwrap();
            lhs_bv &= rhs_bv;
            let expect = BitVec::<128>::try_from(lhs & rhs).unwrap();
            if lhs_bv != expect {
                println!("a = 0x{lhs:032x}");
                println!("b = 0x{rhs:032x}");
                println!("op: a &= b");
                println!("  result: {lhs_bv}");
                println!("expected: {expect}");
                panic!("Test failed");
            }
        }
    }

    #[test]
    fn test_bitor() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        for _ in 0..TEST_ITERS {
            let lhs = rng.random::<u128>();
            let rhs = rng.random::<u128>();
            let lhs_bv = BitVec::<128>::try_from(lhs).unwrap();
            let rhs_bv = BitVec::<128>::try_from(rhs).unwrap();
            let result = lhs_bv | rhs_bv;
            let expect = BitVec::<128>::try_from(lhs | rhs).unwrap();
            if result != expect {
                println!("a = 0x{lhs:032x}");
                println!("b = 0x{rhs:032x}");
                println!("op: a | b");
                println!("  result: {result}");
                println!("expected: {expect}");
                panic!("Test failed");
            }
        }
    }

    #[test]
    fn test_bitor_assign() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        for _ in 0..TEST_ITERS {
            let lhs = rng.random::<u128>();
            let rhs = rng.random::<u128>();
            let mut lhs_bv = BitVec::<128>::try_from(lhs).unwrap();
            let rhs_bv = BitVec::<128>::try_from(rhs).unwrap();
            lhs_bv |= rhs_bv;
            let expect = BitVec::<128>::try_from(lhs | rhs).unwrap();
            if lhs_bv != expect {
                println!("a = 0x{lhs:032x}");
                println!("b = 0x{rhs:032x}");
                println!("op: a |= b");
                println!("  result: {lhs_bv}");
                println!("expected: {expect}");
                panic!("Test failed");
            }
        }
    }

    #[test]
    fn test_bitxor() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        for _ in 0..TEST_ITERS {
            let lhs = rng.random::<u128>();
            let rhs = rng.random::<u128>();
            let lhs_bv = BitVec::<128>::try_from(lhs).unwrap();
            let rhs_bv = BitVec::<128>::try_from(rhs).unwrap();
            let result = lhs_bv ^ rhs_bv;
            let expect = BitVec::<128>::try_from(lhs ^ rhs).unwrap();
            if result != expect {
                println!("a = 0x{lhs:032x}");
                println!("b = 0x{rhs:032x}");
                println!("op: a ^ b");
                println!("  result: {result}");
                println!("expected: {expect}");
                panic!("Test failed");
            }
        }
    }

    #[test]
    fn test_bitxor_assign() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        for _ in 0..TEST_ITERS {
            let lhs = rng.random::<u128>();
            let rhs = rng.random::<u128>();
            let mut lhs_bv = BitVec::<128>::try_from(lhs).unwrap();
            let rhs_bv = BitVec::<128>::try_from(rhs).unwrap();
            lhs_bv ^= rhs_bv;
            let expect = BitVec::<128>::try_from(lhs ^ rhs).unwrap();
            if lhs_bv != expect {
                println!("a = 0x{lhs:032x}");
                println!("b = 0x{rhs:032x}");
                println!("op: a ^= b");
                println!("  result: {lhs_bv}");
                println!("expected: {expect}");
                panic!("Test failed");
            }
        }
    }

    #[test]
    fn test_bitvec_shl() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        let dist = Uniform::try_from(0..u128::BITS).unwrap();
        for _ in 0..TEST_ITERS {
            let lhs = rng.random::<u128>();
            let rhs = dist.sample(&mut rng);
            let result = BitVec::<128>::try_from(lhs).unwrap() << rhs;
            let expect = BitVec::<128>::try_from(lhs << rhs).unwrap();
            if result != expect {
                println!("a = 0x{lhs:032x}");
                println!("b = 0x{rhs:02x} ({rhs})");
                println!("op: a << b");
                println!("  result: 0x{result}");
                println!("expected: 0x{expect}");
                panic!("Test failed!");
            }
        }
    }

    #[test]
    fn test_bitvec_shl_assign() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        let dist = Uniform::try_from(0..u128::BITS).unwrap();
        for _ in 0..TEST_ITERS {
            let lhs = rng.random::<u128>();
            let rhs = dist.sample(&mut rng);
            let mut lhs_bv = BitVec::<128>::try_from(lhs).unwrap();
            lhs_bv <<= rhs;
            let expect = BitVec::<128>::try_from(lhs << rhs).unwrap();
            if lhs_bv != expect {
                println!("a = 0x{lhs:032x}");
                println!("b = 0x{rhs:02x} ({rhs})");
                println!("op: a <<= b");
                println!("  result: 0x{lhs_bv}");
                println!("expected: 0x{expect}");
                panic!("Test failed!");
            }
        }
    }

    #[test]
    fn test_bitvec_shr() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        let dist = Uniform::try_from(0..u128::BITS).unwrap();
        for _ in 0..TEST_ITERS {
            let lhs = rng.random::<u128>();
            let rhs = dist.sample(&mut rng);
            let result = BitVec::<128>::try_from(lhs).unwrap() >> rhs;
            let expect = BitVec::<128>::try_from(lhs >> rhs).unwrap();
            if result != expect {
                println!("a = 0x{lhs:032x}");
                println!("b = 0x{rhs:02x} ({rhs})");
                println!("op: a >> b");
                println!("  result: 0x{result}");
                println!("expected: 0x{expect}");
                panic!("Test failed!");
            }
        }
    }

    #[test]
    fn test_bitvec_shr_assign() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        let dist = Uniform::try_from(0..u128::BITS).unwrap();
        for _ in 0..TEST_ITERS {
            let lhs = rng.random::<u128>();
            let rhs = dist.sample(&mut rng);
            let mut lhs_bv = BitVec::<128>::try_from(lhs).unwrap();
            lhs_bv >>= rhs;
            let expect = BitVec::<128>::try_from(lhs >> rhs).unwrap();
            if lhs_bv != expect {
                println!("a = 0x{lhs:032x}");
                println!("b = 0x{rhs:02x} ({rhs})");
                println!("op: a >>= b");
                println!("  result: 0x{lhs_bv}");
                println!("expected: 0x{expect}");
                panic!("Test failed!");
            }
        }
    }

    #[cfg(debug_assertions)]
    const ARRAY_TEST_ITERS: usize = 10_000;
    #[cfg(not(debug_assertions))]
    const ARRAY_TEST_ITERS: usize = 100_000;

    const ARRAY_TEST_BITS: usize = 512;
    const _: () = {
        assert!(
            ARRAY_TEST_BITS <= u16::MAX as usize,
            "Array test bits should be <= 65535"
        );
        assert!(
            ARRAY_TEST_BITS % 8 == 0,
            "Array test bits must be a byte multiple"
        );
    };
    const ARRAY_TEST_LEN: usize = ARRAY_TEST_BITS as usize / 8;

    #[test]
    fn test_bitvec_shl_array() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        let dist = Uniform::try_from(256..ARRAY_TEST_BITS).unwrap();
        for _ in 0..ARRAY_TEST_ITERS {
            let lhs = BitVec::<ARRAY_TEST_BITS>::apply_mask(rng.random::<[u8; ARRAY_TEST_LEN]>());
            let lhs_bv = BitVec::new(lhs).unwrap();
            let rhs_num = dist.sample(&mut rng);
            let rhs_bv = {
                let mut out = [0; ARRAY_TEST_LEN];
                for (i, val) in rhs_num.to_le_bytes().into_iter().enumerate() {
                    out[i] = val;
                }
                BitVec::<ARRAY_TEST_BITS>::new(out).unwrap()
            };
            let expect = {
                // Shift by `rhs_num` is equal to multiplying by `2.pow(rhs_num)`. This number is
                // equal to a 1 followed by `rhs_num - 1` 0s.
                let mut rhs_array = [0; ARRAY_TEST_LEN];
                // The 1 is in word...
                let bit_word = rhs_num as usize / 8;
                // The 1 is at position...
                let bit_pos = rhs_num % 8;
                rhs_array[bit_word] = 1 << bit_pos;
                let mut out = [0u8; ARRAY_TEST_LEN];
                // We only need to multiply in the word that has the 1.
                let mut carry = 0;
                let mut tmp = lhs;
                for i in 0..ARRAY_TEST_LEN {
                    (tmp[i], carry) = tmp[i].carrying_mul(rhs_array[bit_word], carry);
                }
                for i in 0..ARRAY_TEST_LEN - bit_word {
                    out[i + bit_word] = tmp[i];
                }
                BitVec::<ARRAY_TEST_BITS>::new(out).unwrap()
            };
            let result = lhs_bv << rhs_bv;
            if result != expect {
                println!("a = {lhs_bv}");
                println!("b = {rhs_bv}");
                println!("op: a << b");
                println!("  result: {result}");
                println!("expected: {expect}");
                panic!("Test failed!");
            }
        }
    }

    #[test]
    fn test_bitvec_shl_assign_array() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        let dist = Uniform::try_from(256..ARRAY_TEST_BITS).unwrap();
        for _ in 0..ARRAY_TEST_ITERS {
            let lhs = BitVec::<ARRAY_TEST_BITS>::apply_mask(rng.random::<[u8; ARRAY_TEST_LEN]>());
            let rhs_num = dist.sample(&mut rng);
            let lhs_bv = BitVec::new(lhs).unwrap();
            let rhs_bv = {
                let mut out = [0; ARRAY_TEST_LEN];
                for (i, val) in rhs_num.to_le_bytes().into_iter().enumerate() {
                    out[i] = val;
                }
                BitVec::<ARRAY_TEST_BITS>::new(out).unwrap()
            };
            let expect = {
                // Shift by `rhs_num` is equal to multiplying by `2.pow(rhs_num)`. This number is
                // equal to a 1 followed by `rhs_num - 1` 0s.
                let mut rhs_array = [0; ARRAY_TEST_LEN];
                // The 1 is in word...
                let bit_word = rhs_num as usize / 8;
                // The 1 is at position...
                let bit_pos = rhs_num % 8;
                rhs_array[bit_word] = 1 << bit_pos;
                let mut out = [0u8; ARRAY_TEST_LEN];
                // We only need to multiply in the word that has the 1.
                let mut carry = 0;
                let mut tmp = lhs;
                for i in 0..ARRAY_TEST_LEN {
                    (tmp[i], carry) = tmp[i].carrying_mul(rhs_array[bit_word], carry);
                }
                for i in 0..ARRAY_TEST_LEN - bit_word {
                    out[i + bit_word] = tmp[i];
                }
                BitVec::<ARRAY_TEST_BITS>::new(out).unwrap()
            };
            let mut result = BitVec::<ARRAY_TEST_BITS>::new(lhs).unwrap();
            result <<= rhs_bv;
            if result != expect {
                println!("a = {lhs_bv}");
                println!("b = {rhs_bv}");
                println!("op: a << b");
                println!("  result: {result}");
                println!("expected: {expect}");
                panic!("Test failed!");
            }
        }
    }

    #[test]
    fn test_bitvec_shr_array() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        let dist = Uniform::try_from(256..ARRAY_TEST_BITS).unwrap();
        for _ in 0..ARRAY_TEST_ITERS {
            let rhs_num = dist.sample(&mut rng);
            let lhs_bv = BitVec::new(BitVec::<ARRAY_TEST_BITS>::apply_mask(
                rng.random::<[u8; ARRAY_TEST_LEN]>(),
            ))
            .unwrap()
                & BitVec::<ARRAY_TEST_BITS>::ones(ARRAY_TEST_BITS - rhs_num).unwrap();
            let rhs_bv = {
                let mut out = [0; ARRAY_TEST_LEN];
                for (i, val) in rhs_num.to_le_bytes().into_iter().enumerate() {
                    out[i] = val;
                }
                BitVec::<ARRAY_TEST_BITS>::new(out).unwrap()
            };
            let result = (lhs_bv << rhs_bv) >> rhs_bv;
            if result != lhs_bv {
                println!("a = {lhs_bv}");
                println!("b = {rhs_bv}");
                println!("op: a >> b");
                println!("  result: {result}");
                println!("expected: {lhs_bv}");
                panic!("Test failed!");
            }
        }
    }

    #[test]
    fn test_bitvec_shr_assign_array() {
        let mut rng = rand::rngs::StdRng::from_os_rng();
        let dist = Uniform::try_from(256..ARRAY_TEST_BITS).unwrap();
        for _ in 0..ARRAY_TEST_ITERS {
            let rhs_num = dist.sample(&mut rng);
            let lhs_bv = BitVec::new(BitVec::<ARRAY_TEST_BITS>::apply_mask(
                rng.random::<[u8; ARRAY_TEST_LEN]>(),
            ))
            .unwrap()
                & BitVec::<ARRAY_TEST_BITS>::ones(ARRAY_TEST_BITS - rhs_num).unwrap();
            let rhs_bv = {
                let mut out = [0; ARRAY_TEST_LEN];
                for (i, val) in rhs_num.to_le_bytes().into_iter().enumerate() {
                    out[i] = val;
                }
                BitVec::<ARRAY_TEST_BITS>::new(out).unwrap()
            };
            let mut result = lhs_bv;
            result <<= rhs_bv;
            result >>= rhs_bv;
            if result != lhs_bv {
                println!("a = {lhs_bv}");
                println!("b = {rhs_bv}");
                println!("op: a >>= b");
                println!("  result: {result}");
                println!("expected: {lhs_bv}");
                panic!("Test failed!");
            }
        }
    }

    #[cfg_attr(debug_assertions, test)]
    #[cfg_attr(debug_assertions, should_panic)]
    #[cfg(debug_assertions)]
    fn test_mask_panic() {
        let _ = BitVec::<7>::try_from(u8::MAX).unwrap();
    }
}
