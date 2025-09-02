// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![allow(incomplete_features)]

use core::ops::{Add, AddAssign, Rem, RemAssign, Sub, SubAssign};
use ufmt::derive::uDebug;

pub struct IM<const N: u128>;

pub trait IndexTyInner: Sized {
    type Inner: Copy + core::fmt::Debug + Into<u128> + TryFrom<u128>;

    fn from_u128(x: u128) -> Option<Self::Inner>;
    fn to_u128(val: Self::Inner) -> u128;

    fn saturating_add(a: Self::Inner, b: Self::Inner) -> Self::Inner;
    fn saturating_sub(a: Self::Inner, b: Self::Inner) -> Self::Inner;
    fn saturating_mul(a: Self::Inner, b: Self::Inner) -> Self::Inner;

    fn checked_add(a: Self::Inner, b: Self::Inner) -> Option<Self::Inner>;
    fn checked_sub(a: Self::Inner, b: Self::Inner) -> Option<Self::Inner>;
    fn checked_mul(a: Self::Inner, b: Self::Inner) -> Option<Self::Inner>;
}

pub type IndexTI<const N: u128> = <IM<N> as IndexTyInner>::Inner;

struct Check<const B: bool>;

trait True {}

impl True for Check<true> {}

impl<const N: u128> IndexTyInner for IM<N> {
    default type Inner = u128;

    default fn from_u128(x: u128) -> Option<Self::Inner> {
        Self::Inner::try_from(x).ok()
    }

    default fn to_u128(val: Self::Inner) -> u128 {
        val.into()
    }

    fn saturating_add(a: Self::Inner, b: Self::Inner) -> Self::Inner {
        a.saturating_add(b)
    }

    fn saturating_sub(a: Self::Inner, b: Self::Inner) -> Self::Inner {
        todo!()
    }

    fn saturating_mul(a: Self::Inner, b: Self::Inner) -> Self::Inner {
        todo!()
    }

    fn checked_add(a: Self::Inner, b: Self::Inner) -> Option<Self::Inner> {
        todo!()
    }

    fn checked_sub(a: Self::Inner, b: Self::Inner) -> Option<Self::Inner> {
        todo!()
    }

    fn checked_mul(a: Self::Inner, b: Self::Inner) -> Option<Self::Inner> {
        todo!()
    }
}

impl<const N: u128> IndexTyInner for IM<N>
where
    Check<{ N < u64::MAX as u128 }>: True,
{
    default type Inner = u64;

    fn from_u128(x: u128) -> Option<Self::Inner> {
        Self::Inner::try_from(x).ok()
    }

    fn to_u128(val: Self::Inner) -> u128 {
        todo!()
    }

    fn saturating_add(a: Self::Inner, b: Self::Inner) -> Self::Inner {
        a.saturating_add(b)
    }

    fn saturating_sub(a: Self::Inner, b: Self::Inner) -> Self::Inner {
        todo!()
    }

    fn saturating_mul(a: Self::Inner, b: Self::Inner) -> Self::Inner {
        todo!()
    }

    fn checked_add(a: Self::Inner, b: Self::Inner) -> Option<Self::Inner> {
        todo!()
    }

    fn checked_sub(a: Self::Inner, b: Self::Inner) -> Option<Self::Inner> {
        todo!()
    }

    fn checked_mul(a: Self::Inner, b: Self::Inner) -> Option<Self::Inner> {
        todo!()
    }
}

impl<const N: u128> IndexTyInner for IM<N>
where
    Check<{ N < u64::MAX as u128 }>: True,
    Check<{ N < u32::MAX as u128 }>: True,
{
    default type Inner = u32;

    fn from_u128(x: u128) -> Option<Self::Inner> {
        todo!()
    }

    fn to_u128(val: Self::Inner) -> u128 {
        todo!()
    }

    fn saturating_add(a: Self::Inner, b: Self::Inner) -> Self::Inner {
        todo!()
    }

    fn saturating_sub(a: Self::Inner, b: Self::Inner) -> Self::Inner {
        todo!()
    }

    fn saturating_mul(a: Self::Inner, b: Self::Inner) -> Self::Inner {
        todo!()
    }

    fn checked_add(a: Self::Inner, b: Self::Inner) -> Option<Self::Inner> {
        todo!()
    }

    fn checked_sub(a: Self::Inner, b: Self::Inner) -> Option<Self::Inner> {
        todo!()
    }

    fn checked_mul(a: Self::Inner, b: Self::Inner) -> Option<Self::Inner> {
        todo!()
    }
}

impl<const N: u128> IndexTyInner for IM<N>
where
    Check<{ N < u64::MAX as u128 }>: True,
    Check<{ N < u32::MAX as u128 }>: True,
    Check<{ N < u16::MAX as u128 }>: True,
{
    default type Inner = u16;

    fn from_u128(x: u128) -> Option<Self::Inner> {
        todo!()
    }

    fn to_u128(val: Self::Inner) -> u128 {
        todo!()
    }

    fn saturating_add(a: Self::Inner, b: Self::Inner) -> Self::Inner {
        todo!()
    }

    fn saturating_sub(a: Self::Inner, b: Self::Inner) -> Self::Inner {
        todo!()
    }

    fn saturating_mul(a: Self::Inner, b: Self::Inner) -> Self::Inner {
        todo!()
    }

    fn checked_add(a: Self::Inner, b: Self::Inner) -> Option<Self::Inner> {
        todo!()
    }

    fn checked_sub(a: Self::Inner, b: Self::Inner) -> Option<Self::Inner> {
        todo!()
    }

    fn checked_mul(a: Self::Inner, b: Self::Inner) -> Option<Self::Inner> {
        todo!()
    }
}

impl<const N: u128> IndexTyInner for IM<N>
where
    Check<{ N < u64::MAX as u128 }>: True,
    Check<{ N < u32::MAX as u128 }>: True,
    Check<{ N < u16::MAX as u128 }>: True,
    Check<{ N < u8::MAX as u128 }>: True,
{
    type Inner = u8;

    fn from_u128(x: u128) -> Option<Self::Inner> {
        todo!()
    }

    fn to_u128(val: Self::Inner) -> u128 {
        todo!()
    }

    fn saturating_add(a: Self::Inner, b: Self::Inner) -> Self::Inner {
        todo!()
    }

    fn saturating_sub(a: Self::Inner, b: Self::Inner) -> Self::Inner {
        todo!()
    }

    fn saturating_mul(a: Self::Inner, b: Self::Inner) -> Self::Inner {
        todo!()
    }

    fn checked_add(a: Self::Inner, b: Self::Inner) -> Option<Self::Inner> {
        todo!()
    }

    fn checked_sub(a: Self::Inner, b: Self::Inner) -> Option<Self::Inner> {
        todo!()
    }

    fn checked_mul(a: Self::Inner, b: Self::Inner) -> Option<Self::Inner> {
        todo!()
    }
}

pub struct IndexTy<const N: u128, T> {
    idx: T,
}

/// Arbitrarily-bounded unsigned integer
///
/// Use `Index![N]` for types or `index!(v, n = N)` for constructing values.
///
/// Given an upper bound `N`, an `Index<N, ty>` number has a range of: [0..N-1].
///
/// Due to limits of Rust's const generics and evaluation, the underlying number
/// type cannot be automatically computed by the type system.
///
/// Any generated code should automatically pick the best underlying number
/// type, but manually written Rust code could do this wrong and make things
/// confusing.
///
/// This type implements addition, subtraction and reminder operations. Other
/// operations are not supported, so when a coder tries to do, for example,
/// multiplication, it will not compile. This is by design, the exact behaviour
/// should be detailed at the call site when such "uncommon" operations are
/// needed. ("uncommon" as indices are usually only incremented or decremented)
pub type Index<const N: u128> = IndexTy<N, IndexTI<N>>;

impl<const N: u128> Index<N> {
    pub fn new(val: IndexTI<N>) -> Option<Self> {
        if val.into() < N {
            Some(IndexTy { idx: val })
        } else {
            None
        }
    }

    pub fn min_val() -> Self {
        Self {
            idx: 0.try_into().ok().unwrap(),
        }
    }

    pub fn max_val() -> Self {
        Self {
            idx: (N - 1).try_into().ok().unwrap(),
        }
    }

    #[doc(hidden)]
    pub unsafe fn new_unchecked(val: IndexTI<N>) -> Self {
        IndexTy { idx: val }
    }

    pub fn into_underlying(self) -> IndexTI<N> {
        self.idx
    }
}

impl<const N: u128> Index<N> where IM<N>: IndexTyInner<Inner = u8> {
    pub fn saturating_add(self, rhs: Self) -> Self {
        let res = self.idx.saturating_add(rhs.idx);
        Self{ idx: res.min(Self::max_val().idx) }
    }

    pub fn checked_add(self, rhs: Self) -> Option<Self> {
        let res = self.0.checked_add(rhs.0)?;
        if res.into() >= N {
            None
        } else {
            Some(Self(res))
        }
    }

    #[inline]
    pub fn wrapping_add(self, rhs: Self) -> Self {
        let until_end = N as $t - self.0;
        let res = if rhs.0 >= until_end {
            rhs.0 - until_end
        } else {
            self.0 + rhs.0
        };
        Self(res)
    }
}

/*
impl<const N: u128> TryFrom<IndexTI<N>> for Index<N> {
    type Error = ();

    fn try_from(val: IndexTI<N>) -> Result<Self, Self::Error> {
        Self::new(val).ok_or(())
    }
}

impl<const N: u128> From<Index<N>> for IndexTI<N> {
    fn from(val: Index<N>) -> Self {
        val.into_underlying()
    }
}
*/

impl<const N: u128> core::fmt::Display for Index<N> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        use core::fmt::Debug;
        self.idx.fmt(f)
    }
}

/*
macro_rules! impl_add {
    ($t:ty) => {
        impl<const N: u128> IndexTy<N, $t> {
            pub fn saturating_add(self, rhs: Self) -> Self {
                let res = self.0.saturating_add(rhs.0);
                Self(res.min(Self::MAX.0))
            }

            pub fn checked_add(self, rhs: Self) -> Option<Self> {
                let res = self.0.checked_add(rhs.0)?;
                if res >= N as $t {
                    None
                } else {
                    Some(Self(res))
                }
            }

            #[inline]
            pub fn wrapping_add(self, rhs: Self) -> Self {
                let until_end = N as $t - self.0;
                let res = if rhs.0 >= until_end {
                    rhs.0 - until_end
                } else {
                    self.0 + rhs.0
                };
                Self(res)
            }
        }

        impl<const N: u128> Add for IndexTy<N, $t> {
            type Output = Self;

            fn add(self, rhs: Self) -> Self::Output {
                let res = self.0 + rhs.0;
                debug_assert!(res < N as $t, "Addition overflowed");
                if cfg!(debug_assertions) {
                    Self(res)
                } else {
                    self.wrapping_add(rhs)
                }
            }
        }

        impl<const N: u128> Add<&IndexTy<N, $t>> for IndexTy<N, $t> {
            type Output = Self;

            fn add(self, rhs: &Self) -> Self::Output {
                self + *rhs
            }
        }

        impl<const N: u128> AddAssign for IndexTy<N, $t> {
            fn add_assign(&mut self, rhs: Self) {
                *self = *self + rhs;
            }
        }

        impl<const N: u128> AddAssign<&IndexTy<N, $t>> for IndexTy<N, $t> {
            fn add_assign(&mut self, rhs: &Self) {
                *self = *self + rhs;
            }
        }

        impl<const N: u128> Add<$t> for IndexTy<N, $t> {
            type Output = Self;

            fn add(self, rhs: $t) -> Self::Output {
                self + Self::new(rhs).expect("Right hand side operand of index addition cannot be turned into an index value")
            }
        }

        impl<const N: u128> Add<&$t> for IndexTy<N, $t> {
            type Output = Self;

            fn add(self, rhs: &$t) -> Self::Output {
                self + *rhs
            }
        }
    };
}

impl_add!(u8);
impl_add!(u16);
impl_add!(u32);
impl_add!(u64);
impl_add!(u128);

macro_rules! impl_sub {
    ($t:ty) => {
        impl<const N: u128> IndexTy<N, $t> {
            pub fn saturating_sub(self, rhs: Self) -> Self {
                let res = self.0.saturating_sub(rhs.0);
                Self(res)
            }

            pub fn checked_sub(self, rhs: Self) -> Option<Self> {
                if rhs.0 > self.0 {
                    None
                } else {
                    Some(Self(self.0 - rhs.0))
                }
            }

            #[inline]
            pub fn wrapping_sub(self, rhs: Self) -> Self {
                if rhs.0 > self.0 {
                    Self(N as $t - (rhs.0 - self.0))
                } else {
                    Self(self.0 - rhs.0)
                }
            }
        }

        impl<const N: u128> Sub for IndexTy<N, $t> {
            type Output = Self;

            fn sub(self, rhs: Self) -> Self::Output {
                let res = self.0 - rhs.0;
                debug_assert!(res < N as $t);
                if cfg!(debug_assertions) {
                    Self(res)
                } else {
                    self.wrapping_sub(rhs)
                }
            }
        }

        impl<const N: u128> Sub<&IndexTy<N, $t>> for IndexTy<N, $t> {
            type Output = Self;

            fn sub(self, rhs: &Self) -> Self::Output {
                self - *rhs
            }
        }

        impl<const N: u128> SubAssign for IndexTy<N, $t> {
            fn sub_assign(&mut self, rhs: Self) {
                *self = *self - rhs;
            }
        }

        impl<const N: u128> SubAssign<&IndexTy<N, $t>> for IndexTy<N, $t> {
            fn sub_assign(&mut self, rhs: &Self) {
                *self = *self - rhs;
            }
        }

        impl<const N: u128> Sub<$t> for IndexTy<N, $t> {
            type Output = Self;

            fn sub(self, rhs: $t) -> Self::Output {
                self - Self::new(rhs).expect("Right hand side operand of index subtraction cannot be turned into an index value")
            }
        }

        impl<const N: u128> Sub<&$t> for IndexTy<N, $t> {
            type Output = Self;

            fn sub(self, rhs: &$t) -> Self::Output {
                self - *rhs
            }
        }
    };
}

impl_sub!(u8);
impl_sub!(u16);
impl_sub!(u32);
impl_sub!(u64);
impl_sub!(u128);

macro_rules! impl_rem {
    ($t:ty) => {
        impl<const N: u128> IndexTy<N, $t> {
            pub fn checked_rem(self, rhs: Self) -> Option<Self> {
                let res = self.0.checked_rem(rhs.0)?;
                Some(Self(res))
            }

            pub fn restricting_rem<const M: u128>(self) -> IndexTy<M, $t> {
                let val = self.0 % (M as $t);
                IndexTy(val)
            }
        }

        impl<const N: u128> Rem for IndexTy<N, $t> {
            type Output = Self;

            fn rem(self, rhs: Self) -> Self::Output {
                let res = self.0 % rhs.0;
                debug_assert!(res < N as $t);
                Self(res)
            }
        }

        impl<const N: u128> Rem<&IndexTy<N, $t>> for IndexTy<N, $t> {
            type Output = Self;

            fn rem(self, rhs: &Self) -> Self::Output {
                let res = self.0 % rhs.0;
                debug_assert!(res < N as $t);
                Self(res)
            }
        }

        impl<const N: u128> RemAssign for IndexTy<N, $t> {
            fn rem_assign(&mut self, rhs: Self) {
                *self = *self % rhs;
            }
        }

        impl<const N: u128> RemAssign<&IndexTy<N, $t>> for IndexTy<N, $t> {
            fn rem_assign(&mut self, rhs: &Self) {
                *self = *self % rhs;
            }
        }

        impl<const N: u128> Rem<$t> for IndexTy<N, $t> {
            type Output = Self;

            fn rem(self, rhs: $t) -> Self::Output {
                self % Self::new(rhs).expect("Right hand side operand of index reminder cannot be turned into an index value")
            }
        }

        impl<const N: u128> Rem<&$t> for IndexTy<N, $t> {
            type Output = Self;

            fn rem(self, rhs: &$t) -> Self::Output {
                self % *rhs
            }
        }
    };
}

impl_rem!(u8);
impl_rem!(u16);
impl_rem!(u32);
impl_rem!(u64);
impl_rem!(u128);

#[cfg(test)]
mod tests {
    use super::*;
    use crate as bittide_hal;
    use bittide_macros::*;

    #[test]
    fn new_works_with_bounds() {
        let x = IndexTy::<8, u8>::new(7);
        assert_eq!(x, Some(IndexTy(7)));

        let x = IndexTy::<8, u8>::new(12);
        assert_eq!(x, None);
    }

    #[test]
    #[should_panic = "Addition overflowed"]
    fn addition_overflow_panics_two_indices() {
        let a = index!(4, n = 10);
        let b = index!(8, n = 10);

        let _ = a + b;
    }

    #[test]
    #[should_panic = "Addition overflowed"]
    fn addition_overflow_panics_non_index() {
        let a = index!(4, n = 10);

        let _ = a + 8;
    }

    #[test]
    fn addition_in_bounds_works() {
        let a = index!(4, n = 10);
        let b = index!(3, n = 10);

        let c = a + b;
        assert_eq!(c.into_underlying(), 7);

        assert_eq!(a + 4, index!(8, n = 10));

        assert_eq!(a.checked_add(index!(8, n = 10)), None);
    }

    #[test]
    fn saturated_add_works() {
        let a = index!(5, n = 10);
        let b = index!(6, n = 10);

        assert_eq!(a.saturating_add(b), <Index![10]>::MAX);
    }

    #[test]
    fn wrapped_add_works() {
        let a = index!(9, n = 10);
        let b = index!(1, n = 10);

        assert_eq!(a.wrapping_add(b), index!(0, n = 10));
    }

    #[test]
    fn checked_sub_works() {
        let a = index!(4, n = 10);
        let b = index!(3, n = 10);

        assert_eq!(a - b, index!(1, n = 10));
        assert_eq!(a.checked_sub(b), Some(index!(1, n = 10)));

        assert_eq!(b.checked_sub(a), None);
    }

    #[test]
    fn saturated_sub_works() {
        let a = index!(4, n = 10);
        let b = index!(3, n = 10);

        assert_eq!(a.saturating_sub(b), index!(1, n = 10));
        assert_eq!(b.saturating_sub(a), index!(0, n = 10));
    }

    #[test]
    fn wrapping_sub_works() {
        let a = index!(3, n = 10);
        let b = index!(4, n = 10);

        assert_eq!(a.wrapping_sub(b), index!(9, n = 10));

        assert_eq!(
            index!(0, n = 10).wrapping_sub(index!(1, n = 10)),
            index!(9, n = 10)
        );
    }

    #[test]
    fn rem_works() {
        let a = index!(15, n = 20);
        let b = index!(10, n = 20);

        assert_eq!(a % b, index!(5, n = 20));
    }

    #[test]
    fn restricting_rem_works() {
        let a = index!(15, n = 20);

        assert_eq!(a.restricting_rem::<10>(), index!(5, n = 10));
    }
}

*/
