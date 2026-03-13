// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use crate::manual_additions::{
    signed::{Signed, SignedInterface},
    unsigned::{Unsigned, UnsignedInterface},
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct BitVector<const M: usize, const N: usize>(pub(crate) [u8; N]);

pub trait BitVectorSizeCheck {
    const SIZE_CHECK: ();
    type Inner;
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
    /// This function may error if invoked as `BitVector::<M, N>::new(...)` and the wrong backing
    /// length `N` is chosen. Please read the whole error message carefully, it should tell you what
    /// to do to fix it.
    ///
    /// # Safety
    ///
    /// Due to the intended use-case of interfacing with Clash hardware, and there being no
    /// guarantee of behaviour in the case that an out-of-bounds value is written to a register,
    /// this function has been marked as `unsafe`. To make calling this function safe, you must
    /// guarantee that `val` is in range.
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
                            .cast::<[u8; core::mem::size_of::<$backer>()]>()
                            .copy_from_nonoverlapping(other.0.as_ptr().cast(), $size)
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
                            .cast::<[u8; core::mem::size_of::<$backer>()]>()
                            .copy_from_nonoverlapping(other.0.as_ptr().cast(), $size)
                    }
                    if cfg!(target_endian = "big") {
                        Signed(backer.swap_bytes())
                    } else {
                        Signed(backer)
                    }
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
