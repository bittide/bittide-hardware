// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use super::{index::Index, signed::Signed, FromAs};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Unsigned<const N: u8, T>(pub(crate) T);

pub trait UnsignedSizeCheck {
    const CORRECT_SIZE: u8;
    const BITS: u32;
    const SIZE_CHECK: ();
    type Inner: Copy;
    fn inner_bounds_check(val: Self::Inner) -> bool;
}

pub type UnsignedInner<T> = <T as UnsignedSizeCheck>::Inner;

macro_rules! impl_usc {
    ($($t:ty),+$(,)?) => {
        $(
            impl<const N: u8> UnsignedSizeCheck for Unsigned<N, $t> {
                const CORRECT_SIZE: u8 = {
                    match N.div_ceil(8).next_power_of_two() * 8 {
                        size if size >= 8 => size,
                        _ => 8,
                    }
                };
                const BITS: u32 = N as u32;
                const SIZE_CHECK: () = {
                    if N == 0 {
                        panic!("Cannot represent Unsigned<0, T>!");
                    }
                    if N > <$t>::BITS as u8 {
                        const_panic::concat_panic!(
                            const_panic::fmt::FmtArg::DISPLAY;
                            "Specified bit size `",
                            N,
                            "` is too large for backing type `",
                            stringify!($t),
                            "`",
                        );
                    } else if Self::CORRECT_SIZE != <$t>::BITS as u8 {
                        const_panic::concat_panic!(
                            const_panic::fmt::FmtArg::DISPLAY;
                            "Type `",
                            stringify!($t),
                            "` is not optimally sized for bound ",
                            N,
                            ". Use type `u",
                            Self::CORRECT_SIZE,
                            "` instead.",
                        );
                    }
                };
                type Inner = $t;
                #[inline]
                fn inner_bounds_check(val: $t) -> bool {
                    let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
                    if const { Self::CORRECT_SIZE == N } {
                        true
                    } else {
                        val < const { 1 << (N - 1) }
                    }
                }
            }
        )+
    };
}

impl_usc!(u8, u16, u32, u64, u128);

impl<const N: u8, T> Unsigned<N, T>
where
    T: Copy,
    Unsigned<N, T>: UnsignedSizeCheck<Inner = T>,
{
    /// Instantiate a new `Unsigned<N, T>`
    ///
    /// Returns `Some(_)` if `val` fits within `N` bits, returns `None` otherwise.
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `Unsigned::<N, T>::new(...)` and the wrong backing
    /// type `T` is chosen. Please read the whole error message carefully, it should tell you what
    /// to do to fix it.
    #[inline]
    pub fn new(val: T) -> Option<Unsigned<N, T>> {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        if Unsigned::<N, T>::inner_bounds_check(val) {
            Some(Unsigned(val))
        } else {
            None
        }
    }

    /// Instantiate a new `Unsigned<N, T>` without performing a bounds check
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `Unsigned::<N, T>::new(...)` and the wrong backing
    /// type `T` is chosen. Please read the whole error message carefully, it should tell you what
    /// to do to fix it.
    ///
    /// # Safety
    ///
    /// Due to the intended use-case of interfacing with Clash hardware, and there being no
    /// guarantee of behaviour in the case that an out-of-bounds value is written to a register,
    /// this function has been marked as `unsafe`. To make calling this function safe, you must
    /// guarantee that `val` is in the range `0..2.pow(N)`.
    #[inline]
    pub unsafe fn new_unchecked(val: T) -> Unsigned<N, T> {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        Unsigned(val)
    }

    /// Unwrap the inner value contained by this `Unsigned<N, T>`
    #[inline]
    pub fn into_inner(self) -> T {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        self.0
    }
}

impl<const N: u8, T> core::fmt::Debug for Unsigned<N, T>
where
    Self: UnsignedSizeCheck,
    T: core::fmt::Debug,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Unsigned<{N}>({:?})", self.0)
    }
}

impl<const N: u8, T> core::fmt::Display for Unsigned<N, T>
where
    Self: UnsignedSizeCheck,
    T: core::fmt::Display,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<const N: u8, T> ufmt::uDebug for Unsigned<N, T>
where
    Self: UnsignedSizeCheck,
    T: ufmt::uDebug,
{
    fn fmt<W>(&self, fmt: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: ufmt::uWrite + ?Sized,
    {
        ufmt::uwrite!(fmt, "Unsigned<{}>({:?})", N, self.0)
    }
}

impl<const N: u8, T> ufmt::uDisplay for Unsigned<N, T>
where
    Self: UnsignedSizeCheck,
    T: ufmt::uDisplay,
{
    fn fmt<W>(&self, fmt: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: ufmt::uWrite + ?Sized,
    {
        ufmt::uwrite!(fmt, "{}", self.0)
    }
}

impl<const N: u8, T> ufmt::uDisplayHex for Unsigned<N, T>
where
    Self: UnsignedSizeCheck,
    T: ufmt::uDisplayHex,
{
    fn fmt_hex<W>(
        &self,
        fmt: &mut ufmt::Formatter<'_, W>,
        options: ufmt::HexOptions,
    ) -> Result<(), W::Error>
    where
        W: ufmt::uWrite + ?Sized,
    {
        <T as ufmt::uDisplayHex>::fmt_hex(&self.0, fmt, options)
    }
}

impl<const N: u8, T> super::seal::Seal for Unsigned<N, T> where T: super::seal::Seal {}

pub trait UnsignedInterface: Sized + UnsignedSizeCheck + super::seal::Seal {
    /// Instantiate a new `Unsigned<N, T>`
    ///
    /// Returns `Some(_)` if `val` fits within `N` bits, returns `None` otherwise.
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `Unsigned::<N, T>::new(...)` and the wrong backing
    /// type `T` is chosen. Please read the whole error message carefully, it should tell you what
    /// to do to fix it.
    fn uns_new(val: Self::Inner) -> Option<Self>;
    /// Instantiate a new `Unsigned<N, T>` without performing a bounds check
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `Unsigned::<N, T>::new(...)` and the wrong backing
    /// type `T` is chosen. Please read the whole error message carefully, it should tell you what
    /// to do to fix it.
    ///
    /// # Safety
    ///
    /// Due to the intended use-case of interfacing with Clash hardware, and there being no
    /// guarantee of behaviour in the case that an out-of-bounds value is written to a register,
    /// this function has been marked as `unsafe`. To make calling this function safe, you must
    /// guarantee that `val` is in the range `0..2.pow(N)`.
    unsafe fn uns_new_unchecked(val: Self::Inner) -> Self;
    /// Unwrap the inner value contained by this `Unsigned<N, T>`
    fn uns_into_inner(self) -> Self::Inner;
}

impl<const N: u8, T> UnsignedInterface for Unsigned<N, T>
where
    T: Copy + super::seal::Seal,
    Unsigned<N, T>: UnsignedSizeCheck<Inner = T>,
{
    #[inline]
    fn uns_new(val: Self::Inner) -> Option<Self> {
        Self::new(val)
    }

    #[inline]
    unsafe fn uns_new_unchecked(val: Self::Inner) -> Self {
        Self::new_unchecked(val)
    }

    #[inline]
    fn uns_into_inner(self) -> Self::Inner {
        self.into_inner()
    }
}

macro_rules! def_unsigned_from {
    // Entry
    ($($t:ty),+$(,)?) => {
        def_unsigned_from! {
            @run
            sup: [$($t),+],
            sub: [],
        }
    };
    // Base case
    (
        @run
        sup: [],
        sub: $sub:tt,
    ) => {};
    // Recursive case
    (
        @run
        sup: [$h:ty$(, $t:ty)*],
        sub: [$($sub:ty),*],
    ) => {
        // Expand defs for current head of big type
        $( // for $sub in $($sub),*:
            def_unsigned_from! {
                @def
                sup: $h,
                sub: $sub,
            }
        )*

        // Push big type to list of smaller types
        def_unsigned_from! {
            @run
            sup: [$($t),*],
            sub: [$($sub,)* $h],
        }
    };
    // Expand to `impl`s
    (
        @def
        sup: $sup:ty,
        sub: $sub:ty,
    ) => {
        impl<const N: u8> From<$sub> for Unsigned<N, $sup> {
            #[inline]
            fn from(value: $sub) -> Self {
                Unsigned(value as $sup)
            }
        }

        impl<const M: u8, const N: u8> From<Unsigned<N, $sub>> for Unsigned<M, $sup> {
            #[inline]
            fn from(value: Unsigned<N, $sub>) -> Self {
                Unsigned(value.0 as $sup)
            }
        }

        impl<const M: u8, const N: u128> From<Index<N, $sub>> for Unsigned<M, $sup> {
            #[inline]
            fn from(value: Index<N, $sub>) -> Self {
                Unsigned(value.0 as $sup)
            }
        }

        impl<const M: u8, const N: u8> From<Signed<N, $sub>> for Unsigned<M, $sup> {
            #[inline]
            fn from(value: Signed<N, $sub>) -> Self {
                Unsigned(value.0 as $sup)
            }
        }

        impl<const N: u8> FromAs<$sub> for Unsigned<N, $sup> {
            #[inline]
            fn from_as(value: $sub) -> Self {
                Unsigned(value as $sup)
            }
        }

        impl<const M: u8, const N: u8> FromAs<Unsigned<N, $sub>> for Unsigned<M, $sup> {
            #[inline]
            fn from_as(value: Unsigned<N, $sub>) -> Self {
                Unsigned(value.0 as $sup)
            }
        }

        impl<const M: u8, const N: u128> FromAs<Index<N, $sub>> for Unsigned<M, $sup> {
            #[inline]
            fn from_as(value: Index<N, $sub>) -> Self {
                Unsigned(value.0 as $sup)
            }
        }

        impl<const M: u8, const N: u8> FromAs<Signed<N, $sub>> for Unsigned<M, $sup> {
            #[inline]
            fn from_as(value: Signed<N, $sub>) -> Self {
                Unsigned(value.0 as $sup)
            }
        }
    }
}

def_unsigned_from!(u8, u16, u32, u64, u128);

#[cfg(target_pointer_width = "16")]
def_unsigned_from! {
    @run
    sup: [usize],
    sub: [u8],
}

#[cfg(target_pointer_width = "32")]
def_unsigned_from! {
    @run
    sup: [usize],
    sub: [u8, u16],
}

#[cfg(target_pointer_width = "64")]
def_unsigned_from! {
    @run
    sup: [usize],
    sub: [u8, u16, u32],
}

subst_macros::repeat_parallel_subst! {
    groups: [
        [group [sub [INTO] = [u8]]]
        [group [sub [INTO] = [u16]]]
        [group [sub [INTO] = [u32]]]
        [group [sub [INTO] = [u64]]]
        [group [sub [INTO] = [u128]]]
        [group [sub [INTO] = [usize]]]
    ],
    callback: NONE,
    in: {
        impl<const N: u8, T> From<Unsigned<N, T>> for INTO
        where
            Unsigned<N, T>: UnsignedSizeCheck,
            INTO: From<T>,
        {
            #[inline]
            fn from(other: Unsigned<N, T>) -> INTO {
                INTO::from(other.0)
            }
        }

        impl<const N: u8, T> FromAs<Unsigned<N, T>> for INTO
        where
            Unsigned<N, T>: UnsignedSizeCheck,
            INTO: FromAs<T>,
        {
            #[inline]
            fn from_as(other: Unsigned<N, T>) -> INTO {
                INTO::from_as(other.0)
            }
        }
    }
}
