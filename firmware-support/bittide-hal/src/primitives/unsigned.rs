// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![allow(incomplete_features)]

use core::ops::{Add, AddAssign, Rem, RemAssign, Sub, SubAssign};

pub struct UnsignedMarker<const N: u128>;

pub trait UnsignedType: Sized {
    type Type: Copy
        + core::fmt::Debug
        + ufmt::uDebug
        + Into<u128>
        + TryFrom<u128>
        + PartialEq
        + PartialOrd
        + Add
        + Sub
        + Rem;
}

struct Check<const B: bool>;

trait True {}

impl True for Check<true> {}

pub type GetUnsignedMarker<const N: u128> = UnsignedMarker<N>;
pub type GetUnsignedType<const N: u128> = <GetUnsignedMarker<N> as UnsignedType>::Type;

impl<const N: u128> UnsignedType for UnsignedMarker<N> {
    default type Type = u128;
}

impl<const N: u128> UnsignedType for UnsignedMarker<N>
where
    Check<{ N <= 64 }>: True,
{
    default type Type = u64;
}

impl<const N: u128> UnsignedType for UnsignedMarker<N>
where
    Check<{ N <= 64 }>: True,
    Check<{ N <= 32 }>: True,
{
    default type Type = u32;
}
impl<const N: u128> UnsignedType for UnsignedMarker<N>
where
    Check<{ N <= 64 }>: True,
    Check<{ N <= 32 }>: True,
    Check<{ N <= 16 }>: True,
{
    default type Type = u16;
}

impl<const N: u128> UnsignedType for UnsignedMarker<N>
where
    Check<{ N <= 64 }>: True,
    Check<{ N <= 32 }>: True,
    Check<{ N <= 16 }>: True,
    Check<{ N <= 8 }>: True,
{
    default type Type = u8;
}

#[repr(transparent)]
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub struct Unsigned<const N: u128>(GetUnsignedType<N>);

impl<const N: u128> ufmt::uDebug for Unsigned<N>
where
    GetUnsignedType<N>: ufmt::uDebug,
{
    fn fmt<W>(&self, formatter: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: ufmt::uWrite + ?Sized,
    {
        ufmt::uDebug::fmt(&self.0, formatter)
    }
}

impl<const N: u128> Unsigned<N>
where
    GetUnsignedType<N>: PartialEq + PartialOrd,
{
    pub fn checked_new(val: GetUnsignedType<N>) -> Option<Self> {
        if val.into() < N {
            Some(Unsigned(val))
        } else {
            None
        }
    }
    pub fn new(val: GetUnsignedType<N>) -> Self {
        Self::checked_new(val).unwrap()
    }

    pub fn min_val() -> Self {
        unsafe { Self(0.try_into().unwrap_unchecked()) }
    }

    pub fn max_val() -> Self {
        unsafe { Self((const { N - 1 }).try_into().unwrap_unchecked()) }
    }

    #[doc(hidden)]
    pub unsafe fn new_unchecked(val: GetUnsignedType<N>) -> Self {
        Unsigned(val)
    }

    pub fn into_underlying(self) -> GetUnsignedType<N> {
        self.0
    }
}

impl<const N: u128> Unsigned<N>
where
    GetUnsignedType<N>:
        Add<Output = GetUnsignedType<N>> + Sub<Output = GetUnsignedType<N>> + PartialOrd,
{
    pub fn saturating_add(self, rhs: Self) -> Self {
        let diff = Self::max_val().0 - self.0;
        if rhs.0 <= diff {
            Unsigned(self.0 + rhs.0)
        } else {
            Self::max_val()
        }
    }

    pub fn checked_add(self, rhs: Self) -> Option<Self> {
        let diff = Self::max_val().0 - self.0;
        if rhs.0 <= diff {
            Some(Unsigned(self.0 + rhs.0))
        } else {
            None
        }
    }

    #[inline]
    pub fn wrapping_add(self, rhs: Self) -> Self {
        let diff = Self::max_val().0 - self.0;
        let res = if rhs.0 >= diff {
            rhs.0 - diff
        } else {
            self.0 + rhs.0
        };
        Self(res)
    }
}

impl<const N: u128> core::fmt::Display for Unsigned<N>
where
    GetUnsignedType<N>: PartialOrd + core::fmt::Display,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.0.fmt(f)
    }
}

impl<T, const N: u128> Add<T> for Unsigned<N>
where
    Unsigned<N>: TryFrom<T>,
    GetUnsignedType<N>: PartialEq + PartialOrd,
    GetUnsignedType<N>:
        Add<Output = GetUnsignedType<N>> + Sub<Output = GetUnsignedType<N>> + PartialOrd,
{
    type Output = Self;

    fn add(self, rhs: T) -> Self::Output {
        let rhs = Self::try_from(rhs).ok().expect(
            "Right hand side operand of Unsigned addition cannot be turned into an Unsigned value",
        );
        debug_assert!((Self::max_val().0 - self.0) < rhs.0, "Addition overflowed");
        if cfg!(debug_assertions) {
            Self(self.0 + rhs.0)
        } else {
            self.wrapping_add(rhs)
        }
    }
}

impl<T, const N: u128> AddAssign<T> for Unsigned<N>
where
    Unsigned<N>: TryFrom<T>,
    GetUnsignedType<N>: PartialEq + PartialOrd,
    GetUnsignedType<N>:
        Add<Output = GetUnsignedType<N>> + Sub<Output = GetUnsignedType<N>> + PartialOrd,
{
    fn add_assign(&mut self, rhs: T) {
        *self = *self + rhs;
    }
}

impl<const N: u128> Unsigned<N>
where
    GetUnsignedMarker<N>: UnsignedType,
    GetUnsignedType<N>: PartialEq + PartialOrd,
    GetUnsignedType<N>:
        Add<Output = GetUnsignedType<N>> + Sub<Output = GetUnsignedType<N>> + PartialOrd,
{
    pub fn saturating_sub(self, rhs: Self) -> Self {
        if rhs.0 >= self.0 {
            Self::max_val()
        } else {
            Unsigned(self.0 - rhs.0)
        }
    }

    pub fn checked_sub(self, rhs: Self) -> Option<Self> {
        if rhs.0 > self.0 {
            None
        } else {
            Some(Unsigned(self.0 - rhs.0))
        }
    }

    #[inline]
    pub fn wrapping_sub(self, rhs: Self) -> Self {
        if rhs.0 > self.0 {
            Self(Self::max_val().0 - (rhs.0 - self.0))
        } else {
            Self(self.0 - rhs.0)
        }
    }
}

impl<T, const N: u128> Sub<T> for Unsigned<N>
where
    Unsigned<N>: TryFrom<T>,
    GetUnsignedType<N>: PartialEq + PartialOrd,
    GetUnsignedType<N>:
        Add<Output = GetUnsignedType<N>> + Sub<Output = GetUnsignedType<N>> + PartialOrd,
{
    type Output = Self;

    fn sub(self, rhs: T) -> Self::Output {
        let rhs = Self::try_from(rhs).ok().expect(
            "Right hand side operand of Unsigned addition cannot be turned into an Unsigned value",
        );
        debug_assert!(rhs.0 > self.0, "Subtraction underflowed");
        if cfg!(debug_assertions) {
            Self(self.0 - rhs.0)
        } else {
            self.wrapping_sub(rhs)
        }
    }
}

impl<T, const N: u128> SubAssign<T> for Unsigned<N>
where
    Unsigned<N>: TryFrom<T>,
    GetUnsignedType<N>: PartialEq + PartialOrd,
    GetUnsignedType<N>:
        Add<Output = GetUnsignedType<N>> + Sub<Output = GetUnsignedType<N>> + PartialOrd,
{
    fn sub_assign(&mut self, rhs: T) {
        *self = *self - rhs;
    }
}

impl<const N: u128> Unsigned<N>
where
    GetUnsignedType<N>: PartialEq + PartialOrd,
    GetUnsignedType<N>: Rem<Output = GetUnsignedType<N>>,
{
    pub fn checked_rem(self, rhs: Self) -> Option<Self> {
        if rhs.0 == Self::min_val().0 {
            None
        } else {
            Some(Unsigned(self.0 % rhs.0))
        }
    }

    /*
    pub fn restricting_rem<const M: u128>(self) -> Unsigned<M>
    where
        GetUnsignedMarker<M>: BvType,
        GetUnsignedType<M>: PartialEq + PartialOrd,
    {
        let val: u128 = self.0.into() % M;
        Unsigned::<M>::try_from(val).unwrap()
    }
    */
}

impl<T, const N: u128> Rem<T> for Unsigned<N>
where
    Unsigned<N>: TryFrom<T>,
    GetUnsignedType<N>: PartialEq + PartialOrd,
    GetUnsignedType<N>: Rem<Output = GetUnsignedType<N>>,
{
    type Output = Self;

    fn rem(self, rhs: T) -> Self {
        let rhs = Self::try_from(rhs).ok().unwrap();
        let res = self.0 % rhs.0;
        debug_assert!(res.into() < N);
        Self(res)
    }
}

impl<T, const N: u128> RemAssign<T> for Unsigned<N>
where
    Unsigned<N>: TryFrom<T>,
    GetUnsignedType<N>: PartialEq + PartialOrd,
    GetUnsignedType<N>: Rem<Output = GetUnsignedType<N>>,
{
    fn rem_assign(&mut self, rhs: T) {
        *self = *self % rhs;
    }
}

macro_rules! impl_try_from {
    ($($t:ty),+$(,)?) => {$(
        impl<const N: u128> TryFrom<$t> for Unsigned<N>
        where
            GetUnsignedType<N>: PartialEq + PartialOrd,
        {
            type Error = &'static str;

            fn try_from(value: $t) -> Result<Self, Self::Error> {
                let val = value as u128;
                Self::checked_new(val.try_into().ok().unwrap()).ok_or("value exceeded maximum for this type!")
            }
        }

        impl<const N: u128> TryFrom<&$t> for Unsigned<N>
        where
            GetUnsignedType<N>: PartialEq + PartialOrd,
        {
            type Error = &'static str;

            fn try_from(value: &$t) -> Result<Self, Self::Error> {
                let val = *value as u128;
                Self::checked_new(val.try_into().ok().unwrap()).ok_or("value exceeded maximum for this type!")
            }
        }
    )+}
}

impl_try_from!(u8, u16, u32, u64, u128, usize);

/*
 *
/// Arbitrarily-bounded unsigned integer
///
/// Use `Unsigned![N]` for types or `Unsigned!(v, n = N)` for constructing values.
///
/// Given an upper bound `N`, an `Unsigned<N, ty>` number has a range of: [0..N-1].
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

*/

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_works_with_bounds() {
        let x = Unsigned::<8>::checked_new(7);
        assert_eq!(x, Some(Unsigned(7u8)));

        let x = Unsigned::<8>::checked_new(12);
        assert_eq!(x, None);
    }

    #[test]
    #[should_panic = "Addition overflowed"]
    fn addition_overflow_panics_two_indices() {
        let a = Unsigned::<10>::new(4u8);
        let b = Unsigned::<10>::new(8u8);

        let _ = a + b;
    }

    #[test]
    #[should_panic = "Addition overflowed"]
    fn addition_overflow_panics_non_Unsigned() {
        let a = Unsigned::<10>::new(4);

        let _ = a + 8u8;
    }

    #[test]
    fn addition_in_bounds_works() {
        let a = Unsigned::<10>::new(4);
        let b = Unsigned::<10>::new(3);

        let c = a + b;
        assert_eq!(c.into_underlying(), 7);

        assert_eq!(a + 4, Unsigned::new(8));

        assert_eq!(a.checked_add(Unsigned::new(8)), None);
    }

    #[test]
    fn saturated_add_works() {
        let a = Unsigned::<10>::new(5);
        let b = Unsigned::<10>::new(6);

        assert_eq!(a.saturating_add(b), Unsigned::max_val());
    }

    #[test]
    fn wrapped_add_works() {
        let a = Unsigned::<10>::new(9);
        let b = Unsigned::<10>::new(1);

        assert_eq!(a.wrapping_add(b), Unsigned::new(0));
    }

    #[test]
    fn checked_sub_works() {
        let a = Unsigned::<10>::new(4);
        let b = Unsigned::<10>::new(3);

        assert_eq!(a - b, Unsigned::new(1));
        assert_eq!(a.checked_sub(b), Some(Unsigned::new(1)));

        assert_eq!(b.checked_sub(a), None);
    }

    #[test]
    fn saturated_sub_works() {
        let a = Unsigned::<10>::new(4);
        let b = Unsigned::<10>::new(3);

        assert_eq!(a.saturating_sub(b), Unsigned::new(1));
        assert_eq!(b.saturating_sub(a), Unsigned::new(0));
    }

    #[test]
    fn wrapping_sub_works() {
        let a = Unsigned::<10>::new(3);
        let b = Unsigned::<10>::new(4);

        assert_eq!(a.wrapping_sub(b), Unsigned::new(9));

        assert_eq!(
            Unsigned::<10>::new(0).wrapping_sub(Unsigned::new(1)),
            Unsigned::<10>::new(9)
        );
    }

    #[test]
    fn rem_works() {
        let a = Unsigned::<10>::new(15);
        let b = Unsigned::<10>::new(10);

        assert_eq!(a % b, Unsigned::<10>::new(5,));
    }

    /*
    #[test]
    fn restricting_rem_works() {
        let a = Unsigned::<10>::new(15);

        assert_eq!(a.restricting_rem::<10>(), Unsigned::new(5));
    }
    */
}
