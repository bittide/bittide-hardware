// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
//! Types, traits, and implementations for signed types
//!
//! # Note
//!
//! The way that this crate constrains [`Signed<N, T>`]s to the correct size is by ensuring that
//! evaulation of a constant succeeds. As such, when writing `impl`s on [`Signed<N, T>`], users
//! should make sure to force evaluation of [`SignedSizeCheck::SIZE_CHECK`].
//!
//! Alternatively, one can make use of `bittide_macros::Signed!` and `bittide_macros::signed!` to
//! make types/instances with the correct parameters.
#![deny(missing_docs)]

use super::{index::Index, unsigned::Unsigned, FromAs};

/// Type representing a signed value of `N` bits, backed by the smallest useable signed type
///
/// This is equivalent to a [`packC`'d][bpc] [`Signed n`][sgn] from Clash. This type should be
/// backed by the smallest signed integer of `N` bits or more. For example, a `Signed<7, _>` should
/// be `Signed<7, i8>` and `Signed<65, _>` should be `Signed<65, i128>`.
///
/// [bpc]: https://github.com/QBayLogic/clash-protocols-memmap/blob/main/clash-bitpackc/src/Clash/Class/BitPackC.hs#L39-L44
/// [sgn]: https://hackage-content.haskell.org/package/clash-prelude-1.8.4/docs/Clash-Sized-Signed.html#t:Signed
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Signed<const N: u8, T>(pub(crate) T);

/// Trait for making guarantees about the size and validity of signed types
pub trait SignedSizeCheck {
    /// The correct size of the backing type in bits
    const CORRECT_SIZE: u8;
    /// This `const` should be instantiated in methods implemented on [`Signed<N, T>`], since it is
    /// how they are constrained to the correct size. If they're improperly sized, this `const` will
    /// fail to evaluate and produce a compile error.
    ///
    /// # Examples
    ///
    /// Signed types with an appropriate backing type will compile successfully:
    /// ```
    /// # use bittide_hal::manual_additions::signed::{Signed, SignedSizeCheck};
    /// trait Marker {
    ///     fn nothing();
    /// }
    ///
    /// impl Marker for Signed<3, i8> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    ///
    /// impl Marker for Signed<14, i16> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    ///
    /// impl Marker for Signed<26, i32> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    ///
    /// impl Marker for Signed<43, i64> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    ///
    /// impl Marker for Signed<97, i128> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    /// ```
    /// But signed types with improperly sized backing types will produce an error. For example,
    /// when the backing type is too small:
    /// ```compile_fail
    /// # use bittide_hal::manual_additions::signed::{Signed, SignedSizeCheck};
    /// trait Marker {
    ///     fn nothing();
    /// }
    ///
    /// impl Marker for Signed<15, i8> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    /// ```
    /// This produces the error
    /// ```text
    /// error[E0080]: evaluation of `<Signed<15, i8> as SignedSizeCheck>::SIZE_CHECK` failed
    ///    --> manual_additions/signed.rs
    ///     |
    ///     | impl_usc!(i8, i16, i32, i64, i128);
    ///     | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ evaluation panicked: Specified bit size `15` is too large for backing type `i8`
    ///     |
    ///     = note: this error originates in the macro `const_panic::concat_panic` which comes from the expansion of the macro `impl_usc` (in Nightly builds, run with -Z macro-backtrace for more info)
    ///
    /// note: erroneous constant encountered
    ///   --> manual_additions/signed.rs
    ///    |
    ///    |         let _ = Self::SIZE_CHECK;
    ///    |                 ^^^^^^^^^^^^^^^^
    /// ```
    /// Or when the backing type is too large:
    /// ```compile_fail
    /// # use bittide_hal::manual_additions::signed::{Signed, SignedSizeCheck};
    /// trait Marker {
    ///     fn nothing();
    /// }
    ///
    /// impl Marker for Signed<15, i32> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    /// ```
    /// This produces the error
    /// ```text
    /// error[E0080]: evaluation of `<Signed<15, i32> as SignedSizeCheck>::SIZE_CHECK` failed
    ///    --> manual_additions/signed.rs
    ///     |
    ///     | impl_usc!(i8, i16, i32, i64, i128);
    ///     | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ evaluation panicked: Type `i32` is not optimally sized for bound 15. Use type `i16` instead.
    ///     |
    ///     = note: this error originates in the macro `const_panic::concat_panic` which comes from the expansion of the macro `impl_usc` (in Nightly builds, run with -Z macro-backtrace for more info)
    ///
    /// note: erroneous constant encountered
    ///   --> manual_additions/signed.rs
    ///    |
    ///    |         let _ = Self::SIZE_CHECK;
    ///    |                 ^^^^^^^^^^^^^^^^
    /// ```
    /// Or when requesting a signed 0 bit number:
    /// ```compile_fail
    /// # use bittide_hal::manual_additions::signed::{Signed, SignedSizeCheck};
    /// trait Marker {
    ///     fn nothing();
    /// }
    ///
    /// impl Marker for Signed<0, i32> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    /// ```
    /// ```text
    /// error[E0080]: evaluation of `<Signed<0, i32> as SignedSizeCheck>::SIZE_CHECK` failed
    ///    --> manual_additions/signed.rs
    ///     |
    ///     | impl_usc!(i8, i16, i32, i64, i128);
    ///     | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ evaluation panicked: Cannot represent Signed<0, T>!
    ///     |
    ///     = note: this error originates in the macro `const_panic::concat_panic` which comes from the expansion of the macro `impl_usc` (in Nightly builds, run with -Z macro-backtrace for more info)
    ///
    /// note: erroneous constant encountered
    ///   --> manual_additions/signed.rs
    ///    |
    ///    |         let _ = Self::SIZE_CHECK;
    ///    |                 ^^^^^^^^^^^^^^^^
    /// ```
    const SIZE_CHECK: ();
    /// Backing type of a signed number
    type Inner: Copy;
    /// Perform a bounds check on an instance of a signed number. Returns `true` if within bounds,
    /// `false` if not.
    fn inner_bounds_check(val: Self::Inner) -> bool;
}

macro_rules! impl_snc {
    ($($t:ty | $ut:ty),+$(,)?) => {
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
                            ". Use type `i",
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
                        // val >= const { 1 << (N - 1) }
                        val > const {
                            let ut: $ut = !0;
                            ut.unbounded_shl(N as u32) as $t
                        }
                    } else {
                        // val <= const { !(!0 << (N - 1)) }
                        val < const {
                            let t: $t = 1;
                            t.unbounded_shl(N as u32 - 1)
                        }
                    }
                }
            }
        )+
    };
}

impl_snc!(i8 | u8, i16 | u16, i32 | u32, i64 | u64, i128 | u128);

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
    /// This function may error if invoked as `Signed::<N, T>::new_unchecked(...)` and the wrong
    /// backing type `T` is chosen. Please read the whole error message carefully, it should tell
    /// you what to do to fix it.
    ///
    /// # Safety
    ///
    /// Due to the intended use-case of interfacing with Clash hardware, and there being no
    /// guarantee of behaviour in the case that an out-of-bounds value is written to a register,
    /// this function has been marked as `unsafe`. To make calling this function safe, you must
    /// guarantee that `val` is in the range `-2.pow(N - 1)..2.pow(N - 1)`.
    ///
    /// There's one exception to this: it is always safe to call `new_unchecked` if `N` is the same
    /// number of bits as `T`. For example, `Signed::<32, i32>::new_unchecked` is always safe.
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

/// Trait that guarantees parts of the [`Signed<N, T>`] inherent implementations
///
/// Intended for use where it is not desirable to constrain a type to be `T = Signed<N, T>` but
/// rather to `T: SignedInterface`.
pub trait SignedInterface: Sized + SignedSizeCheck + super::seal::Seal {
    /// Instantiate a new [`Signed<N, T>`]
    ///
    /// Returns `Some(_)` if `val` fits within `N` bits, returns `None` otherwise.
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `Signed::<N, T>::sgn_new(...)` and the wrong backing
    /// type `T` is chosen. Please read the whole error message carefully, it should tell you what
    /// to do to fix it.
    fn sgn_new(val: Self::Inner) -> Option<Self>;
    /// Instantiate a new [`Signed<N, T>`] without performing a bounds check
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `Signed::<N, T>::sgn_new_unchecked(...)` and the wrong
    /// backing type `T` is chosen. Please read the whole error message carefully, it should tell
    /// you what to do to fix it.
    ///
    /// # Safety
    ///
    /// Due to the intended use-case of interfacing with Clash hardware, and there being no
    /// guarantee of behaviour in the case that an out-of-bounds value is written to a register,
    /// this function has been marked as `unsafe`. To make calling this function safe, you must
    /// guarantee that `val` is in the range `-2.pow(N - 1)..2.pow(N - 1)`.
    unsafe fn sgn_new_unchecked(val: Self::Inner) -> Self;
    /// Unwrap the inner value contained by this [`Signed<N, T>`]
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
