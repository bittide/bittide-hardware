// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
//! Types, traits, and implementations for fixed-width arrays of bits
//!
//! # Note
//!
//! The way that this crate constrains [`BitVector<M, N>`]s to the correct size is by ensuring that
//! evaluation of a constant succeeds. As such, when writing `impl`s on [`BitVector<M, N>`], users
//! should make sure to force evaluation of [`BitVectorSizeCheck::SIZE_CHECK`].
//!
//! Alternatively, one can make use of `bittide_macros::BitVector!` and `bittide_macros::bitvector!`
//! to make types/instances with the correct parameters.
#![deny(missing_docs)]

use crate::manual_additions::{
    signed::{Signed, SignedInterface, SignedSizeCheck},
    unsigned::{Unsigned, UnsignedInterface, UnsignedSizeCheck},
};

/// A fixed-width array of bits, backed by a byte array
///
/// This is intended to be an equivalent to a [`packC`'d][bpc] [`BitVector n`][bvn] from Clash.
/// The bytes in this array are little-endian, which is to say that the least significant bit of the
/// bitvector is at array index 0, bit 0.
///
/// [bpc]: https://github.com/QBayLogic/clash-protocols-memmap/blob/main/clash-bitpackc/src/Clash/Class/BitPackC.hs#L39-L44
/// [bvn]: https://hackage-content.haskell.org/package/clash-prelude-1.8.4/docs/Clash-Sized-BitVector.html#t:BitVector
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct BitVector<const M: usize, const N: usize>(pub(crate) [u8; N]);

/// Trait for making guarantees about the size and validity of bitvectors.
pub trait BitVectorSizeCheck {
    /// This `const` should be instantiated in methods implemented on [`BitVector<M, N>`], since it
    /// is how they are constrained to the correct size. If they're improperly sized, this `const`
    /// will fail to evaluate and produce a compile error.
    ///
    /// # Examples
    ///
    /// Correctly sized bitvectors will compile successfully:
    /// ```
    /// # use bittide_hal::manual_additions::bitvector::{BitVector, BitVectorSizeCheck};
    /// trait Marker {
    ///     fn nothing();
    /// }
    ///
    /// impl Marker for BitVector<8, 1> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    ///
    /// impl Marker for BitVector<9, 2> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    ///
    /// impl Marker for BitVector<32, 4> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    ///
    /// impl Marker for BitVector<255, 32> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    /// ```
    /// But improper sizes will not. For example, a bitvector with a backing array that's too large:
    /// ```compile_fail
    /// # use bittide_hal::manual_additions::bitvector::{BitVector, BitVectorSizeCheck};
    /// trait Marker {
    ///     fn nothing();
    /// }
    ///
    /// impl Marker for BitVector<1, 10> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    /// ```
    /// This produces the following error message:
    /// ```text
    /// error[E0080]: evaluation of `<BitVector<1, 10> as BitVectorSizeCheck>::SIZE_CHECK` failed
    ///   --> manual_additions/bitvector.rs
    ///    |
    ///    | /             const_panic::concat_panic!(
    ///    | |                 const_panic::fmt::FmtArg::DISPLAY;
    ///    | |                 "Specified bit size `",
    ///    | |                 M,
    /// ...  |
    ///    | |                 "`."
    ///    | |             );
    ///    | |_____________^ evaluation panicked: Specified bit size `1` should be represented by `1` bytes, not `10`.
    ///    |
    ///    = note: this error originates in the macro `const_panic::concat_panic` (in Nightly builds, run with -Z macro-backtrace for more info)
    ///
    /// note: erroneous constant encountered
    ///   --> manual_additions/bitvector.rs
    ///    |
    ///    |         let _ = Self::SIZE_CHECK;
    ///    |                 ^^^^^^^^^^^^^^^^
    /// ```
    /// And a bitvector with a backing array that's too small:
    /// ```compile_fail
    /// # use bittide_hal::manual_additions::bitvector::{BitVector, BitVectorSizeCheck};
    /// trait Marker {
    ///     fn nothing();
    /// }
    ///
    /// impl Marker for BitVector<16, 1> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    /// ```
    /// This produces the following error message:
    /// ```text
    /// error[E0080]: evaluation of `<BitVector<16, 1> as BitVectorSizeCheck>::SIZE_CHECK` failed
    ///    --> manual_additions/bitvector.rs
    ///     |
    ///     | /             const_panic::concat_panic!(
    ///     | |                 const_panic::fmt::FmtArg::DISPLAY;
    ///     | |                 "Specified bit size `",
    ///     | |                 M,
    /// ...   |
    ///     | |                 "`."
    ///     | |             );
    ///     | |_____________^ evaluation panicked: Specified bit size `16` should be represented by `2` bytes, not `1`.
    ///     |
    ///     = note: this error originates in the macro `const_panic::concat_panic` (in Nightly builds, run with -Z macro-backtrace for more info)
    ///
    /// note: erroneous constant encountered
    ///   --> manual_additions/bitvector.rs
    ///  |
    ///  |         let _ = Self::SIZE_CHECK;
    ///  |                 ^^^^^^^^^^^^^^^^
    /// ```
    /// And also makes sure that a bitvector is not zero-sized:
    /// ```compile_fail
    /// # use bittide_hal::manual_additions::bitvector::{BitVector, BitVectorSizeCheck};
    /// trait Marker {
    ///     fn nothing();
    /// }
    ///
    /// impl Marker for BitVector<0, 0> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    /// ```
    /// Which produces the error message
    /// ```text
    /// error[E0080]: evaluation of `<BitVector<0, 0> as BitVectorSizeCheck>::SIZE_CHECK` failed
    ///    --> manual_additions/bitvector.rs
    ///     |
    ///     |             panic!("Cannot represent a `BitVector<0, 0>`!");
    ///     |             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ evaluation panicked: Cannot represent a `BitVector<0, 0>`!
    ///
    /// note: erroneous constant encountered
    ///   --> manual_additions/bitvector.rs
    ///    |
    ///    |         let _ = Self::SIZE_CHECK;
    ///    |                 ^^^^^^^^^^^^^^^^
    ///
    /// error: aborting due to 1 previous error
    /// ```
    const SIZE_CHECK: ();
    /// Backing type of a bitvector on a trait level
    type Inner;
    /// Perform a bounds check on an instance of a bitvector. Returns `true` if within bounds,
    /// `false` if not.
    fn inner_bounds_check(val: &Self::Inner) -> bool;
}

impl<const M: usize, const N: usize> BitVectorSizeCheck for BitVector<M, N> {
    const SIZE_CHECK: () = {
        if M == 0 {
            panic!("Cannot represent a `BitVector<0, 0>`!");
        }
        let correct_size = M.div_ceil(8);
        if correct_size != N {
            const_panic::concat_panic!(
                const_panic::fmt::FmtArg::DISPLAY;
                "Specified bit size `",
                M,
                "` should be represented by `",
                correct_size,
                "` bytes, not `",
                N,
                "`."
            );
        }
    };
    type Inner = [u8; N];
    #[inline]
    fn inner_bounds_check(val: &[u8; N]) -> bool {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE

        // Check that the most significant byte doesn't have bits set that shouldn't be.
        if const { M.is_multiple_of(8) } {
            // OPTIMIZATION: this cannot happen if the length is a multiple of 8, skip the check
            true
        } else {
            // For `A * 8 + B` bits, create a constant `u8` where the first `B` bits are 1 and the
            // rest are 0. Ensure that the most significant byte (at index `A - 1`) is less than or
            // equal to this constant. For example, in a `BitVector<43, 6>` we will create the
            // constant `0b0000_0111`, and ensure the value at `self.0[5]` is less than or equal to
            // this value.
            val[const { N - 1 }] <= const { !(!0 << (M % 8)) }
        }
    }
}

impl<const M: usize, const N: usize> BitVector<M, N>
where
    BitVector<M, N>: BitVectorSizeCheck<Inner = [u8; N]>,
{
    /// Instantiate a new `BitVector<M, N>`
    ///
    /// Byte arrays given to this function are treated as least significant byte first. So for a
    /// `BitVector<14, 2>` the value `[0xff, 0x3f]` is in range, but `[0xff, 0x7f]` is not, since
    /// that requires 15 bits to represent.
    ///
    /// This function returns `Some(_)` if `val` is in range and returns `None` otherwise.
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `BitVector::<M, N>::new(...)` and the wrong backing
    /// length `N` is chosen. Please read the whole error message carefully, it should tell you what
    /// to do to fix it.
    #[inline]
    pub fn new(val: [u8; N]) -> Option<BitVector<M, N>> {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        if BitVector::<M, N>::inner_bounds_check(&val) {
            Some(BitVector(val))
        } else {
            None
        }
    }

    /// Instantiate a new `BitVector<M, N>` without performing a bounds check
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `BitVector::<M, N>::new_unchecked(...)` and the wrong
    /// backing length `N` is chosen. Please read the whole error message carefully, it should tell
    /// you what to do to fix it.
    ///
    /// # Safety
    ///
    /// Due to the intended use-case of interfacing with Clash hardware, and there being no
    /// guarantee of behaviour in the case that an out-of-bounds value is written to a register,
    /// this function has been marked as `unsafe`. To make calling this function safe, you must
    /// guarantee that `val` is in range.
    ///
    /// There is one exception to this: it is always safe to call `new_unchecked` if `M` is a
    /// multiple of 8. For example, `BitVector::<16, 2>::new_unchecked` is always safe.
    #[inline]
    pub unsafe fn new_unchecked(val: [u8; N]) -> BitVector<M, N> {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        BitVector(val)
    }

    /// Unwrap the inner value contained by this `BitVector<M, N>`
    #[inline]
    pub fn into_inner(self) -> [u8; N] {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        self.0
    }
}

impl<const M: usize, const N: usize> core::fmt::Debug for BitVector<M, N>
where
    Self: BitVectorSizeCheck,
{
    #[inline]
    fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(fmt, "BitVector<{M}>(0x")?;
        if const { M % 8 > 3 } {
            write!(fmt, "{:02X}", self.0[const { N - 1 }])?;
        } else {
            write!(fmt, "{:01X}", self.0[const { N - 1 }])?;
        }
        for &byte in self.0[0..const { N - 1 }].iter().rev() {
            write!(fmt, "{byte:02X}")?;
        }
        write!(fmt, ")")
    }
}

impl<const M: usize, const N: usize> core::fmt::Display for BitVector<M, N>
where
    Self: BitVectorSizeCheck,
{
    #[inline]
    fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(fmt, "0x")?;
        if const { M % 8 > 3 } {
            write!(fmt, "{:02X}", self.0[const { N - 1 }])?;
        } else {
            write!(fmt, "{:01X}", self.0[const { N - 1 }])?;
        }
        for &byte in self.0[0..const { N - 1 }].iter().rev() {
            write!(fmt, "{byte:02X}")?;
        }
        Ok(())
    }
}

impl<const M: usize, const N: usize> ufmt::uDebug for BitVector<M, N>
where
    Self: BitVectorSizeCheck,
{
    #[inline]
    fn fmt<W>(&self, fmt: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: ufmt::uWrite + ?Sized,
    {
        ufmt::uwrite!(fmt, "BitVector<{}>(0x", M)?;
        if const { M % 8 > 3 } {
            ufmt::uwrite!(fmt, "{:02X}", self.0[const { N - 1 }])?;
        } else {
            ufmt::uwrite!(fmt, "{:01X}", self.0[const { N - 1 }])?;
        }
        for &byte in self.0[0..const { N - 1 }].iter().rev() {
            ufmt::uwrite!(fmt, "{:02X}", byte)?;
        }
        ufmt::uwrite!(fmt, ")")
    }
}

impl<const M: usize, const N: usize> ufmt::uDisplay for BitVector<M, N>
where
    Self: BitVectorSizeCheck,
{
    #[inline]
    fn fmt<W>(&self, fmt: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: ufmt::uWrite + ?Sized,
    {
        ufmt::uwrite!(fmt, "0x")?;
        if const { M % 8 > 3 } {
            ufmt::uwrite!(fmt, "{:02X}", self.0[const { N - 1 }])?;
        } else {
            ufmt::uwrite!(fmt, "{:01X}", self.0[const { N - 1 }])?;
        }
        for &byte in self.0[0..const { N - 1 }].iter().rev() {
            ufmt::uwrite!(fmt, "{:02X}", byte)?;
        }
        Ok(())
    }
}

impl<const M: usize, const N: usize> ufmt::uDisplayHex for BitVector<M, N>
where
    Self: BitVectorSizeCheck,
{
    #[inline]
    fn fmt_hex<W>(
        &self,
        fmt: &mut ufmt::Formatter<'_, W>,
        options: ufmt::HexOptions,
    ) -> Result<(), W::Error>
    where
        W: ufmt::uWrite + ?Sized,
    {
        if options.upper_case {
            if options.ox_prefix {
                ufmt::uwrite!(fmt, "0X")?;
                if const { M % 8 > 3 } {
                    ufmt::uwrite!(fmt, "{:02X}", self.0[const { N - 1 }])?;
                } else {
                    ufmt::uwrite!(fmt, "{:01X}", self.0[const { N - 1 }])?;
                }
                for &byte in self.0[0..const { N - 1 }].iter().rev() {
                    ufmt::uwrite!(fmt, "{:02X}", byte)?;
                }
            }
        } else {
            ufmt::uwrite!(fmt, "0x")?;
            if const { M % 8 > 3 } {
                ufmt::uwrite!(fmt, "{:02x}", self.0[const { N - 1 }])?;
            } else {
                ufmt::uwrite!(fmt, "{:01x}", self.0[const { N - 1 }])?;
            }
            for &byte in self.0[0..const { N - 1 }].iter().rev() {
                ufmt::uwrite!(fmt, "{:02x}", byte)?;
            }
        }
        Ok(())
    }
}

impl<const M: usize, const N: usize> AsRef<[u8; N]> for BitVector<M, N>
where
    Self: BitVectorSizeCheck,
{
    #[inline]
    fn as_ref(&self) -> &[u8; N] {
        &self.0
    }
}

impl<const M: usize, const N: usize> super::seal::Seal for BitVector<M, N> {}

/// Trait that guarantees parts of the [`BitVector<M, N>`] inherent implementations
///
/// Intended for use where it is not desirable to constrain a type to be `T = BitVector<M, N>` but
/// rather to `T: BitVectorInterface`.
pub trait BitVectorInterface: Sized + BitVectorSizeCheck + super::seal::Seal {
    /// Instantiate a new [`BitVector<M, N>`]
    ///
    /// Byte arrays given to this function are treated as least significant byte first. So for a
    /// `BitVector<14, 2>` the value `[0xff, 0x3f]` is in range, but `[0xff, 0x7f]` is not, since
    /// that requires 15 bits to represent.
    ///
    /// This function returns `Some(_)` if `val` is in range and returns `None` otherwise.
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `BitVector::<M, N>::bv_new(...)` and the wrong backing
    /// length `N` is chosen. Please read the whole error message carefully, it should tell you what
    /// to do to fix it.
    fn bv_new(val: Self::Inner) -> Option<Self>;
    /// Instantiate a new [`BitVector<M, N>`] without performing a bounds check
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `BitVector::<M, N>::bv_new_unchecked(...)` and the
    /// wrong backing length `N` is chosen. Please read the whole error message carefully, it should
    /// tell you what to do to fix it.
    ///
    /// # Safety
    ///
    /// Due to the intended use-case of interfacing with Clash hardware, and there being no
    /// guarantee of behaviour in the case that an out-of-bounds value is written to a register,
    /// this function has been marked as `unsafe`. To make calling this function safe, you must
    /// guarantee that `val` is in range.
    unsafe fn bv_new_unchecked(val: Self::Inner) -> Self;
    /// Unwrap the inner value contained by this [`BitVector<M, N>`]
    fn bv_into_inner(self) -> Self::Inner;
}

impl<const M: usize, const N: usize> BitVectorInterface for BitVector<M, N>
where
    Self: BitVectorSizeCheck<Inner = [u8; N]>,
{
    #[inline]
    fn bv_new(val: Self::Inner) -> Option<Self> {
        BitVector::<M, N>::new(val)
    }

    #[inline]
    unsafe fn bv_new_unchecked(val: Self::Inner) -> Self {
        BitVector::<M, N>::new_unchecked(val)
    }

    #[inline]
    fn bv_into_inner(self) -> Self::Inner {
        self.into_inner()
    }
}

macro_rules! impl_bvc {
    ($(@$kind:tt [$backer:ty] $sizelist:tt;)+) => {
        $(
            impl_bvc! {
                @$kind
                backer: $backer,
                sizes: $sizelist,
            }
        )+
    };
    (
        @unsigned
        backer: $backer:ty,
        sizes: [$($size:literal),+$(,)?],
    ) => {
        $(
            impl<const A: u8, const B: usize> From<BitVector<B, $size>>
                for Unsigned<A, $backer>
            where
                Self: UnsignedInterface<Inner = $backer>,
                BitVector<B, $size>: BitVectorSizeCheck<Inner = [u8; $size]>,
            {
                #[inline]
                fn from(other: BitVector<B, $size>) -> Unsigned<A, $backer> {
                    let _size_check = const {
                        let _ = BitVector::<B, $size>::SIZE_CHECK; // READ THE REST OF THE MESSAGE
                        let _ = Unsigned::<A, $backer>::SIZE_CHECK; // READ THE REST OF THE MESSAGE
                        if B > A as usize {
                            const_panic::concat_panic!(
                                const_panic::fmt::FmtArg::DISPLAY;
                                "`BitVector<",
                                B,
                                ", ",
                                stringify!($size),
                                ">` cannot be unconditionally converted into `Unsigned<",
                                A,
                                ", ",
                                stringify!($backer),
                                ">`",
                            );
                        }
                    };
                    let mut backer: $backer = 0;
                    unsafe {
                        (&mut backer as *mut $backer)
                            .cast::<[u8; $size]>()
                            .copy_from_nonoverlapping(other.0.as_ptr().cast(), 1);
                    }
                    if cfg!(target_endian = "big") {
                        Unsigned(backer.swap_bytes())
                    } else {
                        Unsigned(backer)
                    }
                }
            }

            impl<const A: u8, const B: usize> From<Unsigned<A, $backer>>
                for BitVector<B, $size>
            where
                Self: BitVectorSizeCheck<Inner = [u8; $size]>,
                Unsigned<A, $backer>: UnsignedInterface<Inner = $backer>,
            {
                #[inline]
                fn from(other: Unsigned<A, $backer>) -> BitVector<B, $size> {
                    let _size_check: () = const {
                        let _ = BitVector::<B, $size>::SIZE_CHECK; // READ THE REST OF THE MESSAGE
                        let _ = Unsigned::<A, $backer>::SIZE_CHECK; // READ THE REST OF THE MESSAGE
                        if A as usize > B {
                            const_panic::concat_panic!(
                                const_panic::fmt::FmtArg::DISPLAY;
                                "`Unsigned<",
                                A,
                                ", ",
                                stringify!($backer),
                                ">` cannot be unconditionally converted into `BitVector<",
                                B,
                                ", ",
                                stringify!($size),
                                ">`",
                            );
                        }
                    };
                    let mut backer = [0; $size];
                    unsafe {
                        backer.as_mut_ptr()
                            .copy_from_nonoverlapping(
                                &other as *const Unsigned<A, $backer> as *const u8,
                                $size,
                            );
                    }
                    if cfg!(target_endian = "big") {
                        backer[const { 0..core::mem::size_of::<$backer>() }].reverse();
                    }
                    BitVector(backer)
                }
            }
        )+
    };
    (
        @signed
        backer: $backer:ty,
        sizes: [$($size:literal),+$(,)?],
    ) => {
        $(
            impl<const A: u8, const B: usize> From<BitVector<B, $size>>
                for Signed<A, $backer>
            where
                Self: SignedInterface<Inner = $backer>,
                BitVector<B, $size>: BitVectorSizeCheck<Inner = [u8; $size]>,
            {
                #[inline]
                fn from(other: BitVector<B, $size>) -> Signed<A, $backer> {
                    let _size_check: () = const {
                        let _ = BitVector::<B, $size>::SIZE_CHECK; // READ THE REST OF THE MESSAGE
                        let _ = Signed::<A, $backer>::SIZE_CHECK; // READ THE REST OF THE MESSAGE
                        if B > A as usize {
                            const_panic::concat_panic!(
                                const_panic::fmt::FmtArg::DISPLAY;
                                "`BitVector<",
                                B,
                                ", ",
                                stringify!($size),
                                ">` cannot be unconditionally converted into `Signed<",
                                A,
                                ", ",
                                stringify!($backer),
                                ">`",
                            );
                        }
                    };
                    let mut backer: $backer = 0;
                    unsafe {
                        (&mut backer as *mut $backer)
                            .cast::<[u8; $size]>()
                            .copy_from_nonoverlapping(other.0.as_ptr().cast(), 1);
                    }
                    let mut backer = if cfg!(target_endian = "big") {
                        backer.swap_bytes()
                    } else {
                        backer
                    };
                    // Sign extend if most significant bit is set
                    if backer & const { 1 << (B - 1) } != 0 {
                        backer |= const { !0 << (B - 1) };
                    }
                    Signed(backer)
                }
            }

            impl<const A: u8, const B: usize> From<Signed<A, $backer>>
                for BitVector<B, $size>
            where
                Self: BitVectorSizeCheck<Inner = [u8; $size]>,
                Signed<A, $backer>: SignedInterface<Inner = $backer>,
            {
                #[inline]
                fn from(other: Signed<A, $backer>) -> BitVector<B, $size> {
                    let _size_check: () = const {
                        let _ = BitVector::<B, $size>::SIZE_CHECK; // READ THE REST OF THE MESSAGE
                        let _ = Signed::<A, $backer>::SIZE_CHECK; // READ THE REST OF THE MESSAGE
                        if A as usize > B {
                            const_panic::concat_panic!(
                                const_panic::fmt::FmtArg::DISPLAY;
                                "`Signed<",
                                A,
                                ", ",
                                stringify!($backer),
                                ">` cannot be unconditionally converted into `BitVector<",
                                B,
                                ", ",
                                stringify!($size),
                                ">`",
                            );
                        }
                    };
                    let mut backer = [0; $size];
                    unsafe {
                        backer.as_mut_ptr()
                            .copy_from_nonoverlapping(
                                &other as *const Signed<A, $backer> as *const u8,
                                $size,
                            );
                    }
                    if cfg!(target_endian = "big") {
                        backer[const { 0..core::mem::size_of::<$backer>() }].reverse();
                    }
                    // Mask sign extension if number is negative and there's bits that could be set
                    // above the limit in the most significant byte
                    if other.0 < 0 && const { !A.is_multiple_of(8) } {
                        backer[const { $size - 1 }] &= const { !(!0 << (A % 8)) };
                    }
                    BitVector(backer)
                }
            }
        )+
    };
}

impl_bvc! {
    @unsigned [u8] [1];
    @unsigned [u16] [1, 2];
    @unsigned [u32] [1, 2, 3, 4];
    @unsigned [u64] [1, 2, 3, 4, 5, 6, 7, 8];
    @unsigned [u128] [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];
    @signed [i8] [1];
    @signed [i16] [1, 2];
    @signed [i32] [1, 2, 3, 4];
    @signed [i64] [1, 2, 3, 4, 5, 6, 7, 8];
    @signed [i128] [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];
}

#[cfg(target_pointer_width = "16")]
impl_bvc! {
    @unsigned [usize] [1, 2];
    @signed [isize] [1, 2];
}

#[cfg(target_pointer_width = "32")]
impl_bvc! {
    @unsigned [usize] [1, 2, 3, 4];
    @signed [isize] [1, 2, 3, 4];
}

#[cfg(target_pointer_width = "64")]
impl_bvc! {
    @unsigned [usize] [1, 2, 3, 4, 5, 6, 7, 8];
    @signed [isize] [1, 2, 3, 4, 5, 6, 7, 8];
}

impl<const M: usize, const N: usize, T> core::ops::Index<T> for BitVector<M, N>
where
    Self: BitVectorSizeCheck,
    [u8; N]: core::ops::Index<T>,
{
    type Output = <[u8; N] as core::ops::Index<T>>::Output;

    #[inline]
    fn index(&self, index: T) -> &Self::Output {
        self.0.index(index)
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn bv_us_conv_prim() {
        // Check for n = 8
        let bv = bittide_macros::bitvector!(0xab, n = 8);
        let us = bittide_macros::unsigned!(0xab, n = 8);
        assert_eq!(bv, us.into(), "US -> BV conversion incorrect");
        assert_eq!(us, bv.into(), "BV -> US conversion incorrect");

        // Check for n = 16
        let bv = bittide_macros::bitvector!(0xab_cd, n = 16);
        let us = bittide_macros::unsigned!(0xab_cd, n = 16);
        assert_eq!(bv, us.into(), "US -> BV conversion incorrect");
        assert_eq!(us, bv.into(), "BV -> US conversion incorrect");

        // Check for n = 32
        let bv = bittide_macros::bitvector!(0xab_cd_ef_01, n = 32);
        let us = bittide_macros::unsigned!(0xab_cd_ef_01, n = 32);
        assert_eq!(bv, us.into(), "US -> BV conversion incorrect");
        assert_eq!(us, bv.into(), "BV -> US conversion incorrect");

        // Check for n = 64
        let bv = bittide_macros::bitvector!(0xab_cd_ef_01_23_45_67_89, n = 64);
        let us = bittide_macros::unsigned!(0xab_cd_ef_01_23_45_67_89, n = 64);
        assert_eq!(bv, us.into(), "US -> BV conversion incorrect");
        assert_eq!(us, bv.into(), "BV -> US conversion incorrect");

        // Check for n = 128
        let bv =
            bittide_macros::bitvector!(0xab_cd_ef_01_23_45_67_89_ab_cd_ef_01_23_45_67_89, n = 128);
        let us =
            bittide_macros::unsigned!(0xab_cd_ef_01_23_45_67_89_ab_cd_ef_01_23_45_67_89, n = 128);
        assert_eq!(bv, us.into(), "US -> BV conversion incorrect");
        assert_eq!(us, bv.into(), "BV -> US conversion incorrect");
    }

    #[test]
    fn bv_us_conv_nonprim() {
        // Check for n = 24 (non-primitive-sized)
        let bv = bittide_macros::bitvector!(0xab_cd_ef, n = 24);
        let us = bittide_macros::unsigned!(0xab_cd_ef, n = 24);
        assert_eq!(bv, us.into(), "US -> BV conversion incorrect");
        assert_eq!(us, bv.into(), "BV -> US conversion incorrect");

        // Check for n = 25 (non-primitive-sized)
        let bv = bittide_macros::bitvector!(0x1_ab_cd_ef, n = 25);
        let us = bittide_macros::unsigned!(0x1_ab_cd_ef, n = 25);
        assert_eq!(bv, us.into(), "US -> BV conversion incorrect");
        assert_eq!(us, bv.into(), "BV -> US conversion incorrect");
    }

    #[test]
    fn bv_sn_conv_prim() {
        // Check for n = 8
        let bv = bittide_macros::bitvector!(0xab, n = 8);
        let sn = bittide_macros::signed!(0xab, n = 8);
        assert_eq!(bv, sn.into(), "SN -> BV conversion incorrect");
        assert_eq!(sn, bv.into(), "BV -> SN conversion incorrect");

        // Check for n = 16
        let bv = bittide_macros::bitvector!(0xab_cd, n = 16);
        let sn = bittide_macros::signed!(0xab_cd, n = 16);
        assert_eq!(bv, sn.into(), "SN -> BV conversion incorrect");
        assert_eq!(sn, bv.into(), "BV -> SN conversion incorrect");

        // Check for n = 32
        let bv = bittide_macros::bitvector!(0xab_cd_ef_01, n = 32);
        let sn = bittide_macros::signed!(0xab_cd_ef_01, n = 32);
        assert_eq!(bv, sn.into(), "SN -> BV conversion incorrect");
        assert_eq!(sn, bv.into(), "BV -> SN conversion incorrect");

        // Check for n = 64
        let bv = bittide_macros::bitvector!(0xab_cd_ef_01_23_45_67_89, n = 64);
        let sn = bittide_macros::signed!(0xab_cd_ef_01_23_45_67_89, n = 64);
        assert_eq!(bv, sn.into(), "SN -> BV conversion incorrect");
        assert_eq!(sn, bv.into(), "BV -> SN conversion incorrect");

        // Check for n = 128
        let bv =
            bittide_macros::bitvector!(0xab_cd_ef_01_23_45_67_89_ab_cd_ef_01_23_45_67_89, n = 128);
        let sn =
            bittide_macros::signed!(0xab_cd_ef_01_23_45_67_89_ab_cd_ef_01_23_45_67_89, n = 128);
        assert_eq!(bv, sn.into(), "SN -> BV conversion incorrect");
        assert_eq!(sn, bv.into(), "BV -> SN conversion incorrect");
    }

    #[test]
    fn bv_sn_conv_nonprim() {
        // Check for n = 24 (non-primitive-sized, no sign extension needed)
        let bv = bittide_macros::bitvector!(0x2b_cd_ef, n = 24);
        let sn = bittide_macros::signed!(0x2b_cd_ef, n = 24);
        assert_eq!(bv, sn.into(), "SN -> BV conversion incorrect");
        assert_eq!(sn, bv.into(), "BV -> SN conversion incorrect");

        // Check for n = 25 (non-primitive-sized, sign extension needed)
        let bv = bittide_macros::bitvector!(0x1_ab_cd_ef, n = 25);
        let sn = bittide_macros::signed!(0x1_ab_cd_ef, n = 25);
        assert_eq!(bv, sn.into(), "SN -> BV conversion incorrect");
        assert_eq!(sn, bv.into(), "BV -> SN conversion incorrect");
    }
}
