// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![allow(incomplete_features)]

use core::ops::{Add, AddAssign, Rem, RemAssign, Sub, SubAssign};

pub struct IndexMarker<const N: u128>;

pub trait IndexType: Sized {
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

pub type GetIndexMarker<const N: u128> = IndexMarker<N>;
pub type GetIndexType<const N: u128> = <GetIndexMarker<N> as IndexType>::Type;

impl<const N: u128> IndexType for IndexMarker<N> {
    default type Type = u128;
}

impl<const N: u128> IndexType for IndexMarker<N>
where
    Check<{ N <= u64::MAX as u128 }>: True,
{
    default type Type = u64;
}

impl<const N: u128> IndexType for IndexMarker<N>
where
    Check<{ N <= u64::MAX as u128 }>: True,
    Check<{ N <= u32::MAX as u128 }>: True,
{
    default type Type = u32;
}
impl<const N: u128> IndexType for IndexMarker<N>
where
    Check<{ N <= u64::MAX as u128 }>: True,
    Check<{ N <= u32::MAX as u128 }>: True,
    Check<{ N <= u16::MAX as u128 }>: True,
{
    default type Type = u16;
}

impl<const N: u128> IndexType for IndexMarker<N>
where
    Check<{ N <= u64::MAX as u128 }>: True,
    Check<{ N <= u32::MAX as u128 }>: True,
    Check<{ N <= u16::MAX as u128 }>: True,
    Check<{ N <= u8::MAX as u128 }>: True,
{
    default type Type = u8;
}

/*
impl IndexType for IndexMarker<128> {
    type Type = u128;
}

impl IndexType for IndexMarker<64> {
    type Type = u64;
}

impl IndexType for IndexMarker<32> {
    type Type = u32;
}

impl IndexType for IndexMarker<16> {
    type Type = u16;
}

impl IndexType for IndexMarker<8> {
    type Type = u8;
}
*/

#[repr(transparent)]
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub struct Index<const N: u128>(GetIndexType<N>);

impl<const N: u128> ufmt::uDebug for Index<N>
where
    GetIndexType<N>: ufmt::uDebug,
{
    fn fmt<W>(&self, formatter: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: ufmt::uWrite + ?Sized,
    {
        ufmt::uDebug::fmt(&self.0, formatter)
    }
}

impl<const N: u128> Index<N>
where
    IndexMarker<N>: IndexType<Type = u8>,
{
    pub const fn checked_new(v: u8) -> Option<Self> {
        if (v as u128) < N {
            Some(Index(v))
        } else {
            None
        }
    }
    pub const fn new(v: u8) -> Self {
        Self::new_unchecked(v).unwrap()
    }
}

impl<const N: u128> Index<N>
where
    GetIndexType<N>: PartialEq + PartialOrd,
{
    /*
    pub fn checked_new(val: GetIndexType<N>) -> Option<Self> {
        if val.into() < N {
            Some(Index(val))
        } else {
            None
        }
    }
    pub fn new(val: GetIndexType<N>) -> Self {
        Self::checked_new(val).unwrap()
    }
    */

    pub fn min_val() -> Self {
        unsafe { Self(0.try_into().unwrap_unchecked()) }
    }

    pub fn max_val() -> Self {
        unsafe { Self((const { N - 1 }).try_into().unwrap_unchecked()) }
    }

    #[doc(hidden)]
    pub unsafe fn new_unchecked(val: GetIndexType<N>) -> Self {
        Index(val)
    }

    pub fn into_underlying(self) -> GetIndexType<N> {
        self.0
    }
}

impl<const N: u128> Index<N>
where
    GetIndexType<N>: Add<Output = GetIndexType<N>> + Sub<Output = GetIndexType<N>> + PartialOrd,
{
    pub fn saturating_add(self, rhs: Self) -> Self {
        let diff = Self::max_val().0 - self.0;
        if rhs.0 <= diff {
            Index(self.0 + rhs.0)
        } else {
            Self::max_val()
        }
    }

    pub fn checked_add(self, rhs: Self) -> Option<Self> {
        let diff = Self::max_val().0 - self.0;
        if rhs.0 <= diff {
            Some(Index(self.0 + rhs.0))
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

impl<const N: u128> core::fmt::Display for Index<N>
where
    GetIndexType<N>: PartialOrd + core::fmt::Display,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.0.fmt(f)
    }
}

impl<T, const N: u128> Add<T> for Index<N>
where
    Index<N>: TryFrom<T>,
    GetIndexType<N>: PartialEq + PartialOrd,
    GetIndexType<N>: Add<Output = GetIndexType<N>> + Sub<Output = GetIndexType<N>> + PartialOrd,
{
    type Output = Self;

    fn add(self, rhs: T) -> Self::Output {
        let rhs = Self::try_from(rhs).ok().expect(
            "Right hand side operand of index addition cannot be turned into an index value",
        );
        debug_assert!((Self::max_val().0 - self.0) < rhs.0, "Addition overflowed");
        if cfg!(debug_assertions) {
            Self(self.0 + rhs.0)
        } else {
            self.wrapping_add(rhs)
        }
    }
}

impl<T, const N: u128> AddAssign<T> for Index<N>
where
    Index<N>: TryFrom<T>,
    GetIndexType<N>: PartialEq + PartialOrd,
    GetIndexType<N>: Add<Output = GetIndexType<N>> + Sub<Output = GetIndexType<N>> + PartialOrd,
{
    fn add_assign(&mut self, rhs: T) {
        *self = *self + rhs;
    }
}

impl<const N: u128> Index<N>
where
    GetIndexMarker<N>: IndexType,
    GetIndexType<N>: PartialEq + PartialOrd,
    GetIndexType<N>: Add<Output = GetIndexType<N>> + Sub<Output = GetIndexType<N>> + PartialOrd,
{
    pub fn saturating_sub(self, rhs: Self) -> Self {
        if rhs.0 >= self.0 {
            Self::max_val()
        } else {
            Index(self.0 - rhs.0)
        }
    }

    pub fn checked_sub(self, rhs: Self) -> Option<Self> {
        if rhs.0 > self.0 {
            None
        } else {
            Some(Index(self.0 - rhs.0))
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

impl<T, const N: u128> Sub<T> for Index<N>
where
    Index<N>: TryFrom<T>,
    GetIndexType<N>: PartialEq + PartialOrd,
    GetIndexType<N>: Add<Output = GetIndexType<N>> + Sub<Output = GetIndexType<N>> + PartialOrd,
{
    type Output = Self;

    fn sub(self, rhs: T) -> Self::Output {
        let rhs = Self::try_from(rhs).ok().expect(
            "Right hand side operand of index addition cannot be turned into an index value",
        );
        debug_assert!(rhs.0 > self.0, "Subtraction underflowed");
        if cfg!(debug_assertions) {
            Self(self.0 - rhs.0)
        } else {
            self.wrapping_sub(rhs)
        }
    }
}

impl<T, const N: u128> SubAssign<T> for Index<N>
where
    Index<N>: TryFrom<T>,
    GetIndexType<N>: PartialEq + PartialOrd,
    GetIndexType<N>: Add<Output = GetIndexType<N>> + Sub<Output = GetIndexType<N>> + PartialOrd,
{
    fn sub_assign(&mut self, rhs: T) {
        *self = *self - rhs;
    }
}

impl<const N: u128> Index<N>
where
    GetIndexType<N>: PartialEq + PartialOrd,
    GetIndexType<N>: Rem<Output = GetIndexType<N>>,
{
    pub fn checked_rem(self, rhs: Self) -> Option<Self> {
        if rhs.0 == Self::min_val().0 {
            None
        } else {
            Some(Index(self.0 % rhs.0))
        }
    }

    /*
    pub fn restricting_rem<const M: u128>(self) -> Index<M>
    where
        GetIndexMarker<M>: IndexType,
        GetIndexType<M>: PartialEq + PartialOrd,
    {
        let val: u128 = self.0.into() % M;
        Index::<M>::try_from(val).unwrap()
    }
    */
}

impl<T, const N: u128> Rem<T> for Index<N>
where
    Index<N>: TryFrom<T>,
    GetIndexType<N>: PartialEq + PartialOrd,
    GetIndexType<N>: Rem<Output = GetIndexType<N>>,
{
    type Output = Self;

    fn rem(self, rhs: T) -> Self {
        let rhs = Self::try_from(rhs).ok().unwrap();
        let res = self.0 % rhs.0;
        debug_assert!(res.into() < N);
        Self(res)
    }
}

impl<T, const N: u128> RemAssign<T> for Index<N>
where
    Index<N>: TryFrom<T>,
    GetIndexType<N>: PartialEq + PartialOrd,
    GetIndexType<N>: Rem<Output = GetIndexType<N>>,
{
    fn rem_assign(&mut self, rhs: T) {
        *self = *self % rhs;
    }
}

macro_rules! impl_try_from {
    ($($t:ty),+$(,)?) => {$(
        impl<const N: u128> TryFrom<$t> for Index<N>
        where
            GetIndexType<N>: PartialEq + PartialOrd,
        {
            type Error = &'static str;

            fn try_from(value: $t) -> Result<Self, Self::Error> {
                let val = value as u128;
                Self::checked_new(val.try_into().ok().unwrap()).ok_or("value exceeded maximum for this type!")
            }
        }

        impl<const N: u128> TryFrom<&$t> for Index<N>
        where
            GetIndexType<N>: PartialEq + PartialOrd,
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

*/

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_works_with_bounds() {
        let x = Index::<8>::checked_new(7);
        assert_eq!(x, Some(Index(7u8)));

        let x = Index::<8>::checked_new(12);
        assert_eq!(x, None);
    }

    #[test]
    #[should_panic = "Addition overflowed"]
    fn addition_overflow_panics_two_indices() {
        let a = Index::<10>::new(4u8);
        let b = Index::<10>::new(8u8);

        let _ = a + b;
    }

    #[test]
    #[should_panic = "Addition overflowed"]
    fn addition_overflow_panics_non_index() {
        let a = Index::<10>::new(4);

        let _ = a + 8u8;
    }

    #[test]
    fn addition_in_bounds_works() {
        let a = Index::<10>::new(4);
        let b = Index::<10>::new(3);

        let c = a + b;
        assert_eq!(c.into_underlying(), 7);

        assert_eq!(a + 4, Index::new(8));

        assert_eq!(a.checked_add(Index::new(8)), None);
    }

    #[test]
    fn saturated_add_works() {
        let a = Index::<10>::new(5);
        let b = Index::<10>::new(6);

        assert_eq!(a.saturating_add(b), Index::max_val());
    }

    #[test]
    fn wrapped_add_works() {
        let a = Index::<10>::new(9);
        let b = Index::<10>::new(1);

        assert_eq!(a.wrapping_add(b), Index::new(0));
    }

    #[test]
    fn checked_sub_works() {
        let a = Index::<10>::new(4);
        let b = Index::<10>::new(3);

        assert_eq!(a - b, Index::new(1));
        assert_eq!(a.checked_sub(b), Some(Index::new(1)));

        assert_eq!(b.checked_sub(a), None);
    }

    #[test]
    fn saturated_sub_works() {
        let a = Index::<10>::new(4);
        let b = Index::<10>::new(3);

        assert_eq!(a.saturating_sub(b), Index::new(1));
        assert_eq!(b.saturating_sub(a), Index::new(0));
    }

    #[test]
    fn wrapping_sub_works() {
        let a = Index::<10>::new(3);
        let b = Index::<10>::new(4);

        assert_eq!(a.wrapping_sub(b), Index::new(9));

        assert_eq!(
            Index::<10>::new(0).wrapping_sub(Index::new(1)),
            Index::<10>::new(9)
        );
    }

    #[test]
    fn rem_works() {
        let a = Index::<10>::new(15);
        let b = Index::<10>::new(10);

        assert_eq!(a % b, Index::<10>::new(5,));
    }

    /*
    #[test]
    fn restricting_rem_works() {
        let a = Index::<10>::new(15);

        assert_eq!(a.restricting_rem::<10>(), Index::new(5));
    }
    */
}
