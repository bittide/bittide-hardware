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
pub struct Index<const N: u128, T>(T);

impl<const N: u128, T> Index<N, T> {
    pub fn into_underlying(self) -> T {
        self.0
    }
}

macro_rules! impl_general_stuff {
    ($t:ty) => {
        impl<const N: u128> Index<N, $t> {
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

        impl<const N: u128> TryFrom<$t> for Index<N, $t> {
            type Error = ();

            fn try_from(val: $t) -> Result<Self, Self::Error> {
                Self::new(val).ok_or(())
            }
        }

        impl<const N: u128> From<Index<N, $t>> for $t {
            fn from(val: Index<N, $t>) -> $t {
                val.into_underlying()
            }
        }

        impl<const N: u128> core::fmt::Display for Index<N, $t> {
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
        impl<const N: u128> Index<N, $t> {
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

        impl<const N: u128> Add for Index<N, $t> {
            type Output = Self;

            fn add(self, rhs: Self) -> Self::Output {
                let res = self.0 + rhs.0;
                debug_assert!(res < N as $t, "Addition overflowed");
                Self(res)
            }
        }

        impl<const N: u128> Add<&Index<N, $t>> for Index<N, $t> {
            type Output = Self;

            fn add(self, rhs: &Self) -> Self::Output {
                let res = self.0 + rhs.0;
                debug_assert!(res < N as $t, "Addition overflowed");
                Self(res)
            }
        }

        impl<const N: u128> AddAssign for Index<N, $t> {
            fn add_assign(&mut self, rhs: Self) {
                *self = *self + rhs;
            }
        }

        impl<const N: u128> AddAssign<&Index<N, $t>> for Index<N, $t> {
            fn add_assign(&mut self, rhs: &Self) {
                *self = *self + rhs;
            }
        }

        impl<const N: u128> Add<$t> for Index<N, $t> {
            type Output = Self;

            fn add(self, rhs: $t) -> Self::Output {
                let res = self.0 + rhs;
                debug_assert!(res < N as $t, "Addition overflowed");
                Self(res)
            }
        }

        impl<const N: u128> Add<&$t> for Index<N, $t> {
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
        impl<const N: u128> Index<N, $t> {
            pub fn saturating_sub(self, rhs: Self) -> Self {
                let res = self.0.saturating_sub(rhs.0);
                Self(res)
            }
        }

        impl<const N: u128> Sub for Index<N, $t> {
            type Output = Self;

            fn sub(self, rhs: Self) -> Self::Output {
                let res = self.0 - rhs.0;
                debug_assert!(res < N as $t);
                Self(res)
            }
        }

        impl<const N: u128> Sub<&Index<N, $t>> for Index<N, $t> {
            type Output = Self;

            fn sub(self, rhs: &Self) -> Self::Output {
                let res = self.0 - rhs.0;
                debug_assert!(res < N as $t);
                Self(res)
            }
        }

        impl<const N: u128> SubAssign for Index<N, $t> {
            fn sub_assign(&mut self, rhs: Self) {
                *self = *self - rhs;
            }
        }

        impl<const N: u128> SubAssign<&Index<N, $t>> for Index<N, $t> {
            fn sub_assign(&mut self, rhs: &Self) {
                *self = *self - rhs;
            }
        }

        impl<const N: u128> Sub<$t> for Index<N, $t> {
            type Output = Self;

            fn sub(self, rhs: $t) -> Self::Output {
                let res = self.0 - rhs;
                debug_assert!(res < N as $t);
                Self(res)
            }
        }

        impl<const N: u128> Sub<&$t> for Index<N, $t> {
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
        impl<const N: u128> Index<N, $t> {
            pub fn saturating_mul(self, rhs: Self) -> Self {
                let res = self.0.saturating_mul(rhs.0);
                Self(res.min(N as $t))
            }
        }

        impl<const N: u128> Mul for Index<N, $t> {
            type Output = Self;

            fn mul(self, rhs: Self) -> Self::Output {
                let res = self.0 - rhs.0;
                debug_assert!(res < N as $t, "Multiplication overflowed");
                Self(res)
            }
        }

        impl<const N: u128> Mul<&Index<N, $t>> for Index<N, $t> {
            type Output = Self;

            fn mul(self, rhs: &Self) -> Self::Output {
                let res = self.0 - rhs.0;
                debug_assert!(res < N as $t, "Multiplication overflowed");
                Self(res)
            }
        }

        impl<const N: u128> MulAssign for Index<N, $t> {
            fn mul_assign(&mut self, rhs: Self) {
                *self = *self * rhs;
            }
        }

        impl<const N: u128> MulAssign<&Index<N, $t>> for Index<N, $t> {
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
        impl<const N: u128> Div for Index<N, $t> {
            type Output = Self;

            fn div(self, rhs: Self) -> Self::Output {
                let res = self.0 / rhs.0;
                debug_assert!(res < N as $t);
                Self(res)
            }
        }

        impl<const N: u128> Div<&Index<N, $t>> for Index<N, $t> {
            type Output = Self;

            fn div(self, rhs: &Self) -> Self::Output {
                let res = self.0 / rhs.0;
                debug_assert!(res < N as $t);
                Self(res)
            }
        }

        impl<const N: u128> DivAssign for Index<N, $t> {
            fn div_assign(&mut self, rhs: Self) {
                *self = *self / rhs;
            }
        }

        impl<const N: u128> DivAssign<&Index<N, $t>> for Index<N, $t> {
            fn div_assign(&mut self, rhs: &Self) {
                *self = *self / rhs;
            }
        }

        impl<const N: u128> Div<$t> for Index<N, $t> {
            type Output = Self;

            fn div(self, rhs: $t) -> Self::Output {
                let res = self.0 / rhs;
                debug_assert!(res < N as $t);
                Self(res)
            }
        }

        impl<const N: u128> Div<&$t> for Index<N, $t> {
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
        impl<const N: u128> Rem for Index<N, $t> {
            type Output = Self;

            fn rem(self, rhs: Self) -> Self::Output {
                let res = self.0 % rhs.0;
                debug_assert!(res < N as $t);
                Self(res)
            }
        }

        impl<const N: u128> Rem<&Index<N, $t>> for Index<N, $t> {
            type Output = Self;

            fn rem(self, rhs: &Self) -> Self::Output {
                let res = self.0 % rhs.0;
                debug_assert!(res < N as $t);
                Self(res)
            }
        }

        impl<const N: u128> RemAssign for Index<N, $t> {
            fn rem_assign(&mut self, rhs: Self) {
                *self = *self % rhs;
            }
        }

        impl<const N: u128> RemAssign<&Index<N, $t>> for Index<N, $t> {
            fn rem_assign(&mut self, rhs: &Self) {
                *self = *self % rhs;
            }
        }

        impl<const N: u128> Rem<$t> for Index<N, $t> {
            type Output = Self;

            fn rem(self, rhs: $t) -> Self::Output {
                let res = self.0 % rhs;
                debug_assert!(res < N as $t);
                Self(res)
            }
        }

        impl<const N: u128> Rem<&$t> for Index<N, $t> {
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
        let x = Index::<8, u8>::new(7);
        assert_eq!(x, Some(Index(7)));

        let x = Index::<8, u8>::new(12);
        assert_eq!(x, None);
    }

    #[test]
    #[should_panic = "Addition overflowed"]
    fn addition_overflow_panics() {
        let a = Index::<10, u8>::new(4).unwrap();
        let b = Index::<10, u8>::new(8).unwrap();

        let _ = a + b;
    }

    #[test]
    fn addition_in_bounds_works() {
        let a = Index::<10, u8>::new(4).unwrap();
        let b = Index::<10, u8>::new(3).unwrap();

        let c = a + b;
        assert_eq!(c.into_underlying(), 7);

        assert_eq!(a + 4, Index::<10, u8>::new(8).unwrap());
    }
}
