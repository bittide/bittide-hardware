// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use super::{index::Index, unsigned::Unsigned, FromAs};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Signed<const N: u8, T>(pub(crate) T);

pub trait SignedSizeCheck {
    const CORRECT_SIZE: u8;
    const SIZE_CHECK: ();
    type Inner: Copy;
    fn inner_bounds_check(val: Self::Inner) -> bool;
}

macro_rules! impl_usc {
    ($($t:ty),+$(,)?) => {
        $(
            impl<const N: u8> SignedSizeCheck for Signed<N, $t> {
                const CORRECT_SIZE: u8 = {
                    match N.div_ceil(8).next_power_of_two() * 8 {
                        size if size >= 8 => size,
                        _ => 8,
                    }
                };
                const SIZE_CHECK: () = {
                    if N < 2 {
                        const_panic::concat_panic!(
                            const_panic::fmt::FmtArg::DISPLAY;
                            "Cannot represent Signed<", N, ", T>!",
                        );
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
                    } else if val.is_negative() {
                        val >= const { 1 << (N - 1) }
                    } else {
                        val <= const { !(!0 << (N - 1)) }
                    }
                }
            }
        )+
    };
}

impl_usc!(i8, i16, i32, i64, i128);

impl<const N: u8, T> Signed<N, T>
where
    T: Copy,
    Signed<N, T>: SignedSizeCheck<Inner = T>,
{
    /// Instantiate a new `Signed<N, T>`
    ///
    /// Returns `Some(_)` if `val` fits within `N` bits, returns `None` otherwise.
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `Signed::<N, T>::new(...)` and the wrong backing type
    /// `T` is chosen. Please read the whole error message carefully, it should tell you what
    /// to do to fix it.
    #[inline]
    pub fn new(val: T) -> Option<Signed<N, T>> {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        if Signed::<N, T>::inner_bounds_check(val) {
            Some(Signed(val))
        } else {
            None
        }
    }

    /// Instantiate a new `Signed<N, T>` without performing a bounds check
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `Signed::<N, T>::new(...)` and the wrong backing
    /// type `T` is chosen. Please read the whole error message carefully, it should tell you what
    /// to do to fix it.
    ///
    /// # Safety
    ///
    /// Due to the intended use-case of interfacing with Clash hardware, and there being no
    /// guarantee of behaviour in the case that an out-of-bounds value is written to a register,
    /// this function has been marked as `unsafe`. To make calling this function safe, you must
    /// guarantee that `val` is in the range `-2.pow(N - 1)..2.pow(N - 1)`.
    #[inline]
    pub unsafe fn new_unchecked(val: T) -> Signed<N, T> {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        Signed(val)
    }

    /// Unwrap the inner value contained by this `Signed<N, T>`
    #[inline]
    pub fn into_inner(self) -> T {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        self.0
    }
}

impl<const N: u8, T> core::fmt::Debug for Signed<N, T>
where
    Self: SignedSizeCheck,
    T: core::fmt::Debug,
{
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Signed<{N}>({:?})", self.0)
    }
}

impl<const N: u8, T> core::fmt::Display for Signed<N, T>
where
    Self: SignedSizeCheck,
    T: core::fmt::Display,
{
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<const N: u8, T> ufmt::uDebug for Signed<N, T>
where
    Self: SignedSizeCheck,
    T: ufmt::uDebug,
{
    #[inline]
    fn fmt<W>(&self, fmt: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: ufmt::uWrite + ?Sized,
    {
        ufmt::uwrite!(fmt, "Signed<{}>({:?})", N, self.0)
    }
}

impl<const N: u8, T> ufmt::uDisplay for Signed<N, T>
where
    Self: SignedSizeCheck,
    T: ufmt::uDisplay,
{
    #[inline]
    fn fmt<W>(&self, fmt: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: ufmt::uWrite + ?Sized,
    {
        ufmt::uwrite!(fmt, "{}", self.0)
    }
}

impl<const N: u8, T> ufmt::uDisplayHex for Signed<N, T>
where
    Self: SignedSizeCheck,
    T: ufmt::uDisplayHex,
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
        <T as ufmt::uDisplayHex>::fmt_hex(&self.0, fmt, options)
    }
}

impl<const N: u8, T> super::seal::Seal for Signed<N, T> where T: super::seal::Seal {}

pub trait SignedInterface: Sized + SignedSizeCheck + super::seal::Seal {
    /// Instantiate a new `Signed<N, T>`
    ///
    /// Returns `Some(_)` if `val` fits within `N` bits, returns `None` otherwise.
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `Signed::<N, T>::new(...)` and the wrong backing type
    /// `T` is chosen. Please read the whole error message carefully, it should tell you what
    /// to do to fix it.
    fn sgn_new(val: Self::Inner) -> Option<Self>;
    /// Instantiate a new `Signed<N, T>` without performing a bounds check
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `Signed::<N, T>::new(...)` and the wrong backing
    /// type `T` is chosen. Please read the whole error message carefully, it should tell you what
    /// to do to fix it.
    ///
    /// # Safety
    ///
    /// Due to the intended use-case of interfacing with Clash hardware, and there being no
    /// guarantee of behaviour in the case that an out-of-bounds value is written to a register,
    /// this function has been marked as `unsafe`. To make calling this function safe, you must
    /// guarantee that `val` is in the range `-2.pow(N - 1)..2.pow(N - 1)`.
    unsafe fn sgn_new_unchecked(val: Self::Inner) -> Self;
    /// Unwrap the inner value contained by this `Signed<N, T>`
    fn sgn_into_inner(self) -> Self::Inner;
}

impl<const N: u8, T> SignedInterface for Signed<N, T>
where
    T: Copy + super::seal::Seal,
    Signed<N, T>: SignedSizeCheck<Inner = T>,
{
    #[inline]
    fn sgn_new(val: Self::Inner) -> Option<Self> {
        Self::new(val)
    }

    #[inline]
    unsafe fn sgn_new_unchecked(val: Self::Inner) -> Self {
        Self::new_unchecked(val)
    }

    #[inline]
    fn sgn_into_inner(self) -> Self::Inner {
        self.into_inner()
    }
}

macro_rules! def_signed_from {
    // Entry
    ($($t:ty),+$(,)?) => {
        def_signed_from! {
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
            def_signed_from! {
                @def
                sup: $h,
                sub: $sub,
            }
        )*

        // Push big type to list of smaller types
        def_signed_from! {
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
        impl<const N: u8> From<$sub> for Signed<N, $sup> {
            #[inline]
            fn from(value: $sub) -> Self {
                Signed(value as $sup)
            }
        }

        impl<const M: u8, const N: u8> From<Signed<N, $sub>> for Signed<M, $sup> {
            #[inline]
            fn from(value: Signed<N, $sub>) -> Self {
                Signed(value.0 as $sup)
            }
        }

        impl<const M: u8, const N: u128> From<Index<N, $sub>> for Signed<M, $sup> {
            #[inline]
            fn from(value: Index<N, $sub>) -> Self {
                Signed(value.0 as $sup)
            }
        }

        impl<const M: u8, const N: u8> From<Unsigned<N, $sub>> for Signed<M, $sup> {
            #[inline]
            fn from(value: Unsigned<N, $sub>) -> Self {
                Signed(value.0 as $sup)
            }
        }
        impl<const N: u8> FromAs<$sub> for Signed<N, $sup> {
            fn from_as(value: $sub) -> Self {
                Signed(value as $sup)
            }
        }

        impl<const M: u8, const N: u8> FromAs<Signed<N, $sub>> for Signed<M, $sup> {
            #[inline]
            fn from_as(value: Signed<N, $sub>) -> Self {
                Signed(value.0 as $sup)
            }
        }

        impl<const M: u8, const N: u128> FromAs<Index<N, $sub>> for Signed<M, $sup> {
            #[inline]
            fn from_as(value: Index<N, $sub>) -> Self {
                Signed(value.0 as $sup)
            }
        }

        impl<const M: u8, const N: u8> FromAs<Unsigned<N, $sub>> for Signed<M, $sup> {
            #[inline]
            fn from_as(value: Unsigned<N, $sub>) -> Self {
                Signed(value.0 as $sup)
            }
        }
    }
}

def_signed_from!(i8, i16, i32, i64, i128);

#[cfg(target_pointer_width = "16")]
def_signed_from! {
    @run
    sup: [isize],
    sub: [i8],
}

#[cfg(target_pointer_width = "32")]
def_signed_from! {
    @run
    sup: [isize],
    sub: [i8, i16],
}

#[cfg(target_pointer_width = "64")]
def_signed_from! {
    @run
    sup: [isize],
    sub: [i8, i16, i32],
}

subst_macros::repeat_parallel_subst! {
    groups: [
        [group [sub [INTO] = [i8]]]
        [group [sub [INTO] = [i16]]]
        [group [sub [INTO] = [i32]]]
        [group [sub [INTO] = [i64]]]
        [group [sub [INTO] = [i128]]]
        [group [sub [INTO] = [isize]]]
    ],
    callback: NONE,
    in: {
        impl<const N: u8, T> From<Signed<N, T>> for INTO
        where
            Signed<N, T>: SignedSizeCheck,
            INTO: From<T>,
        {
            #[inline]
            fn from(other: Signed<N, T>) -> INTO {
                INTO::from(other.0)
            }
        }

        impl<const N: u8, T> FromAs<Signed<N, T>> for INTO
        where
            Signed<N, T>: SignedSizeCheck,
            INTO: FromAs<T>,
        {
            #[inline]
            fn from_as(other: Signed<N, T>) -> INTO {
                INTO::from_as(other.0)
            }
        }
    }
}
