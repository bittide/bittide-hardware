// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! A `Mask<M, N>` type with bool-array semantics.
//!
//! `Mask<M, N>` is a transparent newtype around [`BitVector<M, N>`]: it has
//! identical storage and identical bit ordering (bit `i` of the mask is bit
//! `i` of the underlying byte array — LSB at byte 0, bit 0). The wrapper
//! exists to provide a bool-indexed API (`get`, `set`, `from_iter`, `iter`)
//! without re-implementing storage or size-checking. Conversion to/from
//! `BitVector` is zero-cost.
//!
//! This is the Rust counterpart of the Haskell `Mask n` type from
//! `Protocols.MemoryMap.Mask`. The Haskell side enforces the
//! "index `i` of a `Vec n Bool` corresponds to bit `i`" convention, which is
//! the same convention this type uses on the Rust side.
#![deny(missing_docs)]

use core::ops::Index;

use crate::manual_additions::bitvector::{BitVector, BitVectorSizeCheck};

/// Bit-mask of `M` bits, backed by a [`BitVector<M, N>`].
///
/// Bit `i` (for `i in 0..M`) is bit `i` of the underlying byte array (byte
/// `i / 8`, bit `i % 8`). This matches the storage convention of
/// [`BitVector<M, N>`], so converting between `Mask<M, N>` and
/// `BitVector<M, N>` is zero-cost.
#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Mask<const M: usize, const N: usize>(pub(crate) BitVector<M, N>);

impl<const M: usize, const N: usize> Mask<M, N>
where
    BitVector<M, N>: BitVectorSizeCheck<Inner = [u8; N]>,
{
    /// Create a `Mask` with all bits cleared.
    #[inline]
    pub fn zero() -> Self {
        let _: () = <BitVector<M, N> as BitVectorSizeCheck>::SIZE_CHECK;
        // SAFETY: the all-zero byte array represents the value `0`, which is
        // in range for any `BitVector<M, N>`.
        Mask(unsafe { BitVector::<M, N>::new_unchecked([0u8; N]) })
    }

    /// Read bit `i`. Returns `None` if `i >= M`.
    #[inline]
    pub fn get(&self, i: usize) -> Option<bool> {
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
    pub unsafe fn get_unchecked(&self, i: usize) -> bool {
        let _: () = <BitVector<M, N> as BitVectorSizeCheck>::SIZE_CHECK;
        // PERFORMANCE: The `/8` and `%8` compile to `i >> 3` and `i & 7` respectively
        let byte = self.0 .0[i / 8];
        ((byte >> (i % 8)) & 1) == 1
    }

    /// Set bit `i` to `b`. Returns `None` if `i >= M`.
    #[inline]
    pub fn set(&mut self, i: usize, b: bool) -> Option<()> {
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
    pub unsafe fn set_unchecked(&mut self, i: usize, b: bool) {
        let _: () = <BitVector<M, N> as BitVectorSizeCheck>::SIZE_CHECK;
        // PERFORMANCE: The `/8` and `%8` compile to `i >> 3` and `i & 7` respectively
        let bit = 1u8 << (i % 8);
        let byte = &mut self.0 .0[i / 8];
        if b {
            *byte |= bit;
        } else {
            *byte &= !bit;
        }
    }

    /// Iterate over the bits in order `0..M`.
    #[inline]
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = bool> + '_ {
        (0..M).map(|i| unsafe { self.get_unchecked(i) })
    }

    /// Unwrap to the underlying [`BitVector<M, N>`].
    #[inline]
    pub fn into_bitvector(self) -> BitVector<M, N> {
        self.0
    }

    /// Construct a `Mask` from a [`BitVector<M, N>`]
    #[inline]
    pub fn from_bitvector(bv: BitVector<M, N>) -> Self {
        Mask(bv)
    }
}

impl<const M: usize, const N: usize> FromIterator<bool> for Mask<M, N>
where
    BitVector<M, N>: BitVectorSizeCheck<Inner = [u8; N]>,
{
    /// Build a `Mask` from a bool iterator. Bit `i` of the result is the
    /// `i`-th item of the iterator. Items at index `>= M` are silently
    /// ignored; if the iterator yields fewer than `M` items, the missing
    /// bits are taken as `false`.
    #[inline]
    fn from_iter<I: IntoIterator<Item = bool>>(bools: I) -> Self {
        let _: () = <BitVector<M, N> as BitVectorSizeCheck>::SIZE_CHECK;
        let mut out = Self::zero();
        for (i, b) in bools.into_iter().enumerate().take(M) {
            // SAFETY: `take(M)` keeps `i < M`.
            unsafe { out.set_unchecked(i, b) };
        }
        out
    }
}

impl<const M: usize, const N: usize> Index<usize> for Mask<M, N>
where
    BitVector<M, N>: BitVectorSizeCheck<Inner = [u8; N]>,
{
    type Output = bool;

    /// Read bit `i`. Panics if `i >= M`.
    #[inline]
    fn index(&self, i: usize) -> &bool {
        match self.get(i) {
            Some(true) => &true,
            Some(false) => &false,
            None => panic!("Mask index out of bounds: {i} >= {M}"),
        }
    }
}

impl<const M: usize, const N: usize> From<BitVector<M, N>> for Mask<M, N>
where
    BitVector<M, N>: BitVectorSizeCheck<Inner = [u8; N]>,
{
    #[inline]
    fn from(bv: BitVector<M, N>) -> Mask<M, N> {
        Mask(bv)
    }
}

impl<const M: usize, const N: usize> From<Mask<M, N>> for BitVector<M, N>
where
    BitVector<M, N>: BitVectorSizeCheck<Inner = [u8; N]>,
{
    #[inline]
    fn from(m: Mask<M, N>) -> BitVector<M, N> {
        m.0
    }
}

impl<const M: usize, const N: usize> core::fmt::Debug for Mask<M, N>
where
    BitVector<M, N>: BitVectorSizeCheck,
{
    fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(fmt, "Mask<{M}>(")?;
        <BitVector<M, N> as core::fmt::Debug>::fmt(&self.0, fmt)?;
        write!(fmt, ")")
    }
}

impl<const M: usize, const N: usize> ufmt::uDebug for Mask<M, N>
where
    BitVector<M, N>: BitVectorSizeCheck,
{
    fn fmt<W>(&self, fmt: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: ufmt::uWrite + ?Sized,
    {
        ufmt::uwrite!(fmt, "Mask<{}>(", M)?;
        <BitVector<M, N> as ufmt::uDebug>::fmt(&self.0, fmt)?;
        ufmt::uwrite!(fmt, ")")
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn from_iter_round_trip() {
        let bs = [true, false, true, false, false, true, true, false];
        let m: Mask<8, 1> = Mask::from_iter(bs);
        let collected: [bool; 8] = core::array::from_fn(|i| m[i]);
        assert_eq!(collected, bs);
    }

    #[test]
    fn from_iter_round_trip_non_byte_multiple() {
        let bs = [
            true, false, true, true, false, false, false, true, true, true, false, false, true,
        ];
        let m: Mask<13, 2> = Mask::from_iter(bs);
        let collected: [bool; 13] = core::array::from_fn(|i| m[i]);
        assert_eq!(collected, bs);
    }

    #[test]
    fn get_and_set() {
        let mut m: Mask<10, 2> = Mask::zero();
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
        // Bit 0 must be the LSB of the underlying byte array.
        let mut m: Mask<16, 2> = Mask::zero();
        m.set(0, true).unwrap();
        let bv: BitVector<16, 2> = m.into();
        assert_eq!(bv.into_inner(), [0x01, 0x00]);

        // Bit 8 must be bit 0 of byte 1.
        let mut m: Mask<16, 2> = Mask::zero();
        m.set(8, true).unwrap();
        let bv: BitVector<16, 2> = m.into();
        assert_eq!(bv.into_inner(), [0x00, 0x01]);
    }

    #[test]
    fn index_round_trip() {
        let bs = [true, false, true, false, false, true, true, false];
        let m: Mask<8, 1> = Mask::from_iter(bs);
        for i in 0..8 {
            assert_eq!(m[i], bs[i]);
        }
    }

    #[test]
    #[should_panic]
    fn index_out_of_bounds_panics() {
        let m: Mask<8, 1> = Mask::zero();
        let _ = m[8];
    }

    #[test]
    fn iter_round_trip() {
        let bs = [true, false, true, false, false, true, true, false];
        let m: Mask<8, 1> = Mask::from_iter(bs);
        let mut collected = [false; 8];
        for (i, b) in m.iter().enumerate() {
            collected[i] = b;
        }
        assert_eq!(collected, bs);
    }

    #[test]
    fn bitvector_round_trip() {
        // Round-trip Mask -> BitVector -> Mask preserves bits.
        let bs = [true, false, true, true, false, true, false, false];
        let m: Mask<8, 1> = Mask::from_iter(bs);
        let bv: BitVector<8, 1> = m.into();
        let m2: Mask<8, 1> = bv.into();
        let collected: [bool; 8] = core::array::from_fn(|i| m2[i]);
        assert_eq!(collected, bs);
    }

    #[test]
    fn mask_macro_binary_literal() {
        let m: Mask<8, 1> = bittide_macros::mask!(0b1010_0001, n = 8);
        let bs = [true, false, false, false, false, true, false, true];
        let collected: [bool; 8] = core::array::from_fn(|i| m[i]);
        assert_eq!(collected, bs);
    }

    #[test]
    fn mask_macro_hex_literal() {
        let m: Mask<32, 4> = bittide_macros::mask!(0xab_cd_ef_01, n = 32);
        let bv: BitVector<32, 4> = m.into();
        let expected = bittide_macros::bitvector!(0xab_cd_ef_01, n = 32);
        assert_eq!(bv, expected);
    }

    #[test]
    fn mask_macro_non_byte_multiple() {
        let m: Mask<13, 2> = bittide_macros::mask!(0b1_0110_0011_0101, n = 13);
        let bv: BitVector<13, 2> = m.into();
        let expected = bittide_macros::bitvector!(0b1_0110_0011_0101, n = 13);
        assert_eq!(bv, expected);
    }
}
