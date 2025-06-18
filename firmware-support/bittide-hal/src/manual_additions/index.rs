// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Rem, RemAssign, Sub, SubAssign};
use ufmt::derive::uDebug;

/// Arbitrarily-bounded unsigned integer
///
/// Given an upper bound `N`, an `Index<N, ty>` number has a range of: [0..N-1].
///
/// Due to limits of Rust's const generics and evaluation, the underlying number
/// type cannot be automatically computed by the type system.
///
/// Any generated code should automatically pick the best underlying number
/// type, but manually written Rust code could do this wrong and make things
/// confusing.
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
            const MIN: Self = Self(0);
            const MAX: Self = Self(N as $t);

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
                Self(res.min(N as $t))
            }

            pub fn checked_add(self, rhs: Self) -> Option<Self> {
                let res = self.0.checked_add(rhs.0)?;
                if res >= N as $t {
                    None
                } else {
                    Some(Self(res))
                }
            }
        }

        impl<const N: u128> Add for IndexTy<N, $t> {
            type Output = Self;

            fn add(self, rhs: Self) -> Self::Output {
                let res = self.0 + rhs.0;
                debug_assert!(res < N as $t, "Addition overflowed");
                Self(res)
            }
        }

        impl<const N: u128> Add<&IndexTy<N, $t>> for IndexTy<N, $t> {
            type Output = Self;

            fn add(self, rhs: &Self) -> Self::Output {
                let res = self.0 + rhs.0;
                debug_assert!(res < N as $t, "Addition overflowed");
                Self(res)
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
                let res = self.0 + rhs;
                debug_assert!(res < N as $t, "Addition overflowed");
                Self(res)
            }
        }

        impl<const N: u128> Add<&$t> for IndexTy<N, $t> {
            type Output = Self;

            fn add(self, rhs: &$t) -> Self::Output {
                let res = self.0 + rhs;
                debug_assert!(res < N as $t, "Addition overflowed");
                Self(res)
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
        }

        impl<const N: u128> Sub for IndexTy<N, $t> {
            type Output = Self;

            fn sub(self, rhs: Self) -> Self::Output {
                let res = self.0 - rhs.0;
                debug_assert!(res < N as $t);
                Self(res)
            }
        }

        impl<const N: u128> Sub<&IndexTy<N, $t>> for IndexTy<N, $t> {
            type Output = Self;

            fn sub(self, rhs: &Self) -> Self::Output {
                let res = self.0 - rhs.0;
                debug_assert!(res < N as $t);
                Self(res)
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
                let res = self.0 - rhs;
                debug_assert!(res < N as $t);
                Self(res)
            }
        }

        impl<const N: u128> Sub<&$t> for IndexTy<N, $t> {
            type Output = Self;

            fn sub(self, rhs: &$t) -> Self::Output {
                let res = self.0 - rhs;
                debug_assert!(res < N as $t);
                Self(res)
            }
        }
    };
}

impl_sub!(u8);
impl_sub!(u16);
impl_sub!(u32);
impl_sub!(u64);
impl_sub!(u128);

macro_rules! impl_mul {
    ($t:ty) => {
        impl<const N: u128> IndexTy<N, $t> {
            pub fn saturating_mul(self, rhs: Self) -> Self {
                let res = self.0.saturating_mul(rhs.0);
                Self(res.min(N as $t))
            }
        }

        impl<const N: u128> Mul for IndexTy<N, $t> {
            type Output = Self;

            fn mul(self, rhs: Self) -> Self::Output {
                let res = self.0 - rhs.0;
                debug_assert!(res < N as $t, "Multiplication overflowed");
                Self(res)
            }
        }

        impl<const N: u128> Mul<&IndexTy<N, $t>> for IndexTy<N, $t> {
            type Output = Self;

            fn mul(self, rhs: &Self) -> Self::Output {
                let res = self.0 - rhs.0;
                debug_assert!(res < N as $t, "Multiplication overflowed");
                Self(res)
            }
        }

        impl<const N: u128> MulAssign for IndexTy<N, $t> {
            fn mul_assign(&mut self, rhs: Self) {
                *self = *self * rhs;
            }
        }

        impl<const N: u128> MulAssign<&IndexTy<N, $t>> for IndexTy<N, $t> {
            fn mul_assign(&mut self, rhs: &Self) {
                *self = *self * rhs;
            }
        }
    };
}

impl_mul!(u8);
impl_mul!(u16);
impl_mul!(u32);
impl_mul!(u64);
impl_mul!(u128);

macro_rules! impl_div {
    ($t:ty) => {
        impl<const N: u128> Div for IndexTy<N, $t> {
            type Output = Self;

            fn div(self, rhs: Self) -> Self::Output {
                let res = self.0 / rhs.0;
                debug_assert!(res < N as $t);
                Self(res)
            }
        }

        impl<const N: u128> Div<&IndexTy<N, $t>> for IndexTy<N, $t> {
            type Output = Self;

            fn div(self, rhs: &Self) -> Self::Output {
                let res = self.0 / rhs.0;
                debug_assert!(res < N as $t);
                Self(res)
            }
        }

        impl<const N: u128> DivAssign for IndexTy<N, $t> {
            fn div_assign(&mut self, rhs: Self) {
                *self = *self / rhs;
            }
        }

        impl<const N: u128> DivAssign<&IndexTy<N, $t>> for IndexTy<N, $t> {
            fn div_assign(&mut self, rhs: &Self) {
                *self = *self / rhs;
            }
        }

        impl<const N: u128> Div<$t> for IndexTy<N, $t> {
            type Output = Self;

            fn div(self, rhs: $t) -> Self::Output {
                let res = self.0 / rhs;
                debug_assert!(res < N as $t);
                Self(res)
            }
        }

        impl<const N: u128> Div<&$t> for IndexTy<N, $t> {
            type Output = Self;

            fn div(self, rhs: &$t) -> Self::Output {
                let res = self.0 / rhs;
                debug_assert!(res < N as $t);
                Self(res)
            }
        }
    };
}

impl_div!(u8);
impl_div!(u16);
impl_div!(u32);
impl_div!(u64);
impl_div!(u128);

macro_rules! impl_rem {
    ($t:ty) => {
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
                let res = self.0 % rhs;
                debug_assert!(res < N as $t);
                Self(res)
            }
        }

        impl<const N: u128> Rem<&$t> for IndexTy<N, $t> {
            type Output = Self;

            fn rem(self, rhs: &$t) -> Self::Output {
                let res = self.0 % rhs;
                debug_assert!(res < N as $t);
                Self(res)
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

    #[test]
    fn new_works_with_bounds() {
        let x = IndexTy::<8, u8>::new(7);
        assert_eq!(x, Some(IndexTy(7)));

        let x = IndexTy::<8, u8>::new(12);
        assert_eq!(x, None);
    }

    #[test]
    #[should_panic = "Addition overflowed"]
    fn addition_overflow_panics() {
        let a = IndexTy::<10, u8>::new(4).unwrap();
        let b = IndexTy::<10, u8>::new(8).unwrap();

        let _ = a + b;
    }

    #[test]
    fn addition_in_bounds_works() {
        let a = IndexTy::<10, u8>::new(4).unwrap();
        let b = IndexTy::<10, u8>::new(3).unwrap();

        let c = a + b;
        assert_eq!(c.into_underlying(), 7);

        assert_eq!(a + 4, IndexTy::<10, u8>::new(8).unwrap());
    }
}
