// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! A `Mask<M, T>` type with bool-array semantics.
//!
//! `Mask<M, T>` is a transparent newtype around [`Unsigned<M, T>`]: it has
//! identical storage and identical bit ordering (bit `i` of the mask is bit
//! `i` of the underlying primitive integer). The wrapper exists to provide a
//! bool-indexed API (`get`, `set`, `from_iter`, `iter`) without
//! re-implementing storage or size-checking. Conversion to/from `Unsigned`
//! is zero-cost.
//!
//! This is the Rust counterpart of the Haskell `Mask n` type from
//! `Protocols.MemoryMap.Mask`. The Haskell side enforces the
//! "index `i` of a `Vec n Bool` corresponds to bit `i`" convention, which is
//! the same convention this type uses on the Rust side.
#![deny(missing_docs)]

use core::ops::{BitAnd, BitOr, Index, Not, Shl, Shr};

use crate::manual_additions::unsigned::{Unsigned, UnsignedSizeCheck};

/// Bit-mask of `M` bits, backed by an [`Unsigned<M, T>`].
///
/// Bit `i` (for `i in 0..M`) is bit `i` of the underlying primitive integer
/// `T`. This matches the storage convention of [`Unsigned<M, T>`], so
/// converting between `Mask<M, T>` and `Unsigned<M, T>` is zero-cost.
#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Mask<const M: u8, T>(pub(crate) Unsigned<M, T>);

/// Trait for the primitive backing types of [`Mask<M, T>`] (`u8`, `u16`,
/// `u32`, `u64`, `u128`).
///
/// Captures the bit-twiddling operations needed by the mask without
/// requiring callers to repeat the long where-clause.
pub trait MaskBits:
    Copy
    + BitOr<Output = Self>
    + BitAnd<Output = Self>
    + Not<Output = Self>
    + Shl<u32, Output = Self>
    + Shr<u32, Output = Self>
    + PartialEq
{
    /// Additive identity (`0`).
    const ZERO: Self;
    /// Multiplicative identity (`1`).
    const ONE: Self;
}

macro_rules! impl_mask_bits {
    ($($t:ty),+ $(,)?) => {
        $(
            impl MaskBits for $t {
                const ZERO: Self = 0;
                const ONE: Self = 1;
            }
        )+
    };
}

impl_mask_bits!(u8, u16, u32, u64, u128);

impl<const M: u8, T> Mask<M, T>
where
    T: MaskBits,
    Unsigned<M, T>: UnsignedSizeCheck<Inner = T>,
{
    /// Create a `Mask` with all bits cleared.
    #[inline]
    pub fn zero() -> Self {
        let _: () = <Unsigned<M, T> as UnsignedSizeCheck>::SIZE_CHECK;
        // Safe: zero is in range for any (M, T).
        Mask(unsafe { Unsigned::<M, T>::new_unchecked(T::ZERO) })
    }

    /// Read bit `i`. Returns `None` if `i >= M`.
    #[inline]
    pub fn get(&self, i: u8) -> Option<bool> {
        if i >= M {
            None
        } else {
            // SAFETY: just bounds-checked.
            Some(unsafe { self.get_unchecked(i) })
        }
    }

    /// Read bit `i` without bounds-checking.
    ///
    /// # Safety
    /// `i` must be `< M`.
    #[inline]
    pub unsafe fn get_unchecked(&self, i: u8) -> bool {
        let _: () = <Unsigned<M, T> as UnsignedSizeCheck>::SIZE_CHECK;
        let inner = self.0.into_inner();
        ((inner >> i as u32) & T::ONE) == T::ONE
    }

    /// Set bit `i` to `b`. Returns `None` if `i >= M`.
    #[inline]
    pub fn set(&mut self, i: u8, b: bool) -> Option<()> {
        if i >= M {
            return None;
        }
        // SAFETY: just bounds-checked.
        unsafe { self.set_unchecked(i, b) };
        Some(())
    }

    /// Set bit `i` to `b` without bounds-checking.
    ///
    /// # Safety
    /// `i` must be `< M`.
    #[inline]
    pub unsafe fn set_unchecked(&mut self, i: u8, b: bool) {
        let _: () = <Unsigned<M, T> as UnsignedSizeCheck>::SIZE_CHECK;
        let bit = T::ONE << i as u32;
        let inner = self.0.into_inner();
        let new_inner = if b { inner | bit } else { inner & !bit };
        // SAFETY: clearing or setting a single in-range bit cannot push the
        // value outside `0..2^M`.
        self.0 = unsafe { Unsigned::<M, T>::new_unchecked(new_inner) };
    }

    /// Iterate over the bits in order `0..M`.
    #[inline]
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = bool> + '_ {
        (0..M).map(|i| unsafe { self.get_unchecked(i) })
    }

    /// Unwrap to the underlying [`Unsigned<M, T>`].
    #[inline]
    pub fn into_unsigned(self) -> Unsigned<M, T> {
        self.0
    }

    /// Construct a `Mask` from an [`Unsigned<M, T>`] without copying.
    #[inline]
    pub fn from_unsigned(u: Unsigned<M, T>) -> Self {
        Mask(u)
    }
}

impl<const M: u8, T> FromIterator<bool> for Mask<M, T>
where
    T: MaskBits,
    Unsigned<M, T>: UnsignedSizeCheck<Inner = T>,
{
    /// Build a `Mask` from a bool iterator. Bit `i` of the result is the
    /// `i`-th item of the iterator. Items at index `>= M` are silently
    /// ignored; if the iterator yields fewer than `M` items, the missing
    /// bits are taken as `false`.
    #[inline]
    fn from_iter<I: IntoIterator<Item = bool>>(bools: I) -> Self {
        let _: () = <Unsigned<M, T> as UnsignedSizeCheck>::SIZE_CHECK;
        let mut out = Self::zero();
        for (i, b) in bools.into_iter().enumerate().take(M as usize) {
            // SAFETY: `take(M as usize)` keeps `i < M`.
            unsafe { out.set_unchecked(i as u8, b) };
        }
        out
    }
}

impl<const M: u8, T> Index<u8> for Mask<M, T>
where
    T: MaskBits,
    Unsigned<M, T>: UnsignedSizeCheck<Inner = T>,
{
    type Output = bool;

    /// Read bit `i`. Panics if `i >= M`.
    #[inline]
    fn index(&self, i: u8) -> &bool {
        match self.get(i) {
            Some(true) => &true,
            Some(false) => &false,
            None => panic!("Mask index out of bounds: {} >= {}", i, M),
        }
    }
}

impl<const M: u8, T> From<Unsigned<M, T>> for Mask<M, T> {
    #[inline]
    fn from(u: Unsigned<M, T>) -> Self {
        Mask(u)
    }
}

impl<const M: u8, T> From<Mask<M, T>> for Unsigned<M, T> {
    #[inline]
    fn from(m: Mask<M, T>) -> Self {
        m.0
    }
}

impl<const M: u8, T> core::fmt::Debug for Mask<M, T>
where
    Unsigned<M, T>: UnsignedSizeCheck,
    T: core::fmt::Debug,
{
    fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(fmt, "Mask<{M}>(")?;
        <Unsigned<M, T> as core::fmt::Debug>::fmt(&self.0, fmt)?;
        write!(fmt, ")")
    }
}

impl<const M: u8, T> ufmt::uDebug for Mask<M, T>
where
    Unsigned<M, T>: UnsignedSizeCheck,
    T: ufmt::uDebug,
{
    fn fmt<W>(&self, fmt: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: ufmt::uWrite + ?Sized,
    {
        ufmt::uwrite!(fmt, "Mask<{}>(", M)?;
        <Unsigned<M, T> as ufmt::uDebug>::fmt(&self.0, fmt)?;
        ufmt::uwrite!(fmt, ")")
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn from_iter_round_trip() {
        let bs = [true, false, true, false, false, true, true, false];
        let m: Mask<8, u8> = Mask::from_iter(bs);
        let collected: [bool; 8] = core::array::from_fn(|i| m[i as u8]);
        assert_eq!(collected, bs);
    }

    #[test]
    fn from_iter_round_trip_non_byte_aligned() {
        let bs = [
            true, false, true, true, false, false, false, true, true, true, false, false, true,
        ];
        let m: Mask<13, u16> = Mask::from_iter(bs);
        let collected: [bool; 13] = core::array::from_fn(|i| m[i as u8]);
        assert_eq!(collected, bs);
    }

    #[test]
    fn get_and_set() {
        let mut m: Mask<10, u16> = Mask::zero();
        assert_eq!(m.get(0), Some(false));
        assert_eq!(m.get(9), Some(false));
        assert_eq!(m.get(10), None);

        m.set(0, true).unwrap();
        m.set(9, true).unwrap();
        assert_eq!(m.get(0), Some(true));
        assert_eq!(m.get(9), Some(true));
        assert_eq!(m.set(10, true), None);

        m.set(0, false).unwrap();
        assert_eq!(m.get(0), Some(false));
    }

    #[test]
    fn lsb_first_layout() {
        // Bit 0 must be the LSB of the underlying primitive.
        let mut m: Mask<16, u16> = Mask::zero();
        m.set(0, true).unwrap();
        let u: Unsigned<16, u16> = m.into();
        assert_eq!(u.into_inner(), 0x0001);

        // Bit 8 must be bit 8 of the underlying primitive.
        let mut m: Mask<16, u16> = Mask::zero();
        m.set(8, true).unwrap();
        let u: Unsigned<16, u16> = m.into();
        assert_eq!(u.into_inner(), 0x0100);
    }

    #[test]
    fn index_matches_get() {
        let bs = [true, false, true, false, false, true, true, false];
        let m: Mask<8, u8> = Mask::from_iter(bs);
        for i in 0..8u8 {
            assert_eq!(m[i], bs[i as usize]);
        }
    }

    #[test]
    #[should_panic]
    fn index_out_of_bounds_panics() {
        let m: Mask<8, u8> = Mask::zero();
        let _ = m[8];
    }

    #[test]
    fn iter_matches_get() {
        let bs = [true, false, true, false, false, true, true, false];
        let m: Mask<8, u8> = Mask::from_iter(bs);
        let mut collected = [false; 8];
        for (i, b) in m.iter().enumerate() {
            collected[i] = b;
        }
        assert_eq!(collected, bs);
    }
}
