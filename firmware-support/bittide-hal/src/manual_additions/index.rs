// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::ops::{Add, AddAssign, Rem, RemAssign, Sub, SubAssign};
use ufmt::derive::uDebug;

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
#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, Debug, uDebug)]
pub struct IndexTy<const N: u128, T>(T);

impl<const N: u128, T> IndexTy<N, T> {
    pub fn into_underlying(self) -> T {
        self.0
    }

    #[doc(hidden)]
    pub const unsafe fn new_unchecked(n: T) -> Self {
        Self(n)
    }
}

macro_rules! impl_general_stuff {
    ($t:ty) => {
        impl<const N: u128> IndexTy<N, $t> {
            pub const MIN: Self = Self(0);
            pub const MAX: Self = Self((N - 1) as $t);

            pub const fn min_value() -> Self {
                Self::MIN
            }

            pub const fn max_value() -> Self {
                Self::MAX
            }

            pub fn new(val: $t) -> Option<Self> {
                if val >= N as $t {
                    None
                } else {
                    Some(Self(val))
                }
            }
        }

        impl<const N: u128> TryFrom<$t> for IndexTy<N, $t> {
            type Error = ();

            fn try_from(val: $t) -> Result<Self, Self::Error> {
                Self::new(val).ok_or(())
            }
        }

        impl<const N: u128> From<IndexTy<N, $t>> for $t {
            fn from(val: IndexTy<N, $t>) -> $t {
                val.into_underlying()
            }
        }

        impl<const N: u128> core::fmt::Display for IndexTy<N, $t> {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                self.0.fmt(f)
            }
        }
    };
}

impl_general_stuff!(u8);
impl_general_stuff!(u16);
impl_general_stuff!(u32);
impl_general_stuff!(u64);
impl_general_stuff!(u128);

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
