// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
//! Types, traits, and implementations for unsigned types
//!
//! # Note
//!
//! The way that this crate constrains [`Unsigned<N, T>`]s to the correct size is by ensuring that
//! evaulation of a constant succeeds. As such, when writing `impl`s on [`Unsigned<N, T>`], users
//! should make sure to force evaluation of [`UnsignedSizeCheck::SIZE_CHECK`].
//!
//! Alternatively, one can make use of `bittide_macros::Unsigned!` and `bittide_macros::unsigned!`
//! to make types/instances with the correct parameters.
#![deny(missing_docs)]

use super::{index::Index, signed::Signed, FromAs};

/// Type representing an unsigned value of `N` bits, backed by the smallest useable unsigned type
///
/// This is equivalent to a [`packC`'d][bpc] [`Unsigned n`][usn] from Clash. This type should be
/// backed by the smallest signed integer of `N` bits or more. For example, a `Unsigned<7, _>` should
/// be `Unsigned<7, u8>` and `Unsigned<65, _>` should be `Unsigned<65, u128>`.
///
/// [bpc]: https://github.com/QBayLogic/clash-protocols-memmap/blob/main/clash-bitpackc/src/Clash/Class/BitPackC.hs#L39-L44
/// [usn]: https://hackage-content.haskell.org/package/clash-prelude-1.8.4/docs/Clash-Sized-Unsigned.html#t:Unsigned
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Unsigned<const N: u8, T>(pub(crate) T);

/// Trait for making guarantees about the size and validity of unsigned types
pub trait UnsignedSizeCheck {
    /// The correct size of the backing type in bits
    const CORRECT_SIZE: u8;
    /// Generic parameter to the unsigned type exposed as a `u32`
    const BITS: u32;
    /// This `const` should be instantiated in methods implemented on [`Unsigned<N, T>`], since it is
    /// how they are constrained to the correct size. If they're improperly sized, this `const` will
    /// fail to evaluate and produce a compile error.
    ///
    /// # Examples
    ///
    /// Unsigned types with an appropriate backing type will compile successfully:
    /// ```
    /// # use bittide_hal::manual_additions::unsigned::{Unsigned, UnsignedSizeCheck};
    /// trait Marker {
    ///     fn nothing();
    /// }
    ///
    /// impl Marker for Unsigned<3, u8> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    ///
    /// impl Marker for Unsigned<14, u16> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    ///
    /// impl Marker for Unsigned<26, u32> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    ///
    /// impl Marker for Unsigned<43, u64> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    ///
    /// impl Marker for Unsigned<97, u128> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    /// ```
    /// But unsigned types with improperly sized backing types will produce an error. For example,
    /// when the backing type is too small:
    /// ```compile_fail
    /// # use bittide_hal::manual_additions::unsigned::{Unsigned, UnsignedSizeCheck};
    /// trait Marker {
    ///     fn nothing();
    /// }
    ///
    /// impl Marker for Unsigned<15, u8> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    /// ```
    /// This produces the error
    /// ```text
    /// error[E0080]: evaluation of `<Unsigned<15, u8> as UnsignedSizeCheck>::SIZE_CHECK` failed
    ///    --> manual_additions/unsigned.rs
    ///     |
    ///     | impl_usc!(u8, u16, u32, u64, u128);
    ///     | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ evaluation panicked: Specified bit size `15` is too large for backing type `u8`
    ///     |
    ///     = note: this error originates in the macro `const_panic::concat_panic` which comes from the expansion of the macro `impl_usc` (in Nightly builds, run with -Z macro-backtrace for more info)
    ///
    /// note: erroneous constant encountered
    ///   --> manual_additions/unsigned.rs
    ///    |
    ///    |         let _ = Self::SIZE_CHECK;
    ///    |                 ^^^^^^^^^^^^^^^^
    /// ```
    /// Or when the backing type is too large:
    /// ```compile_fail
    /// # use bittide_hal::manual_additions::unsigned::{Unsigned, UnsignedSizeCheck};
    /// trait Marker {
    ///     fn nothing();
    /// }
    ///
    /// impl Marker for Unsigned<15, u32> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    /// ```
    /// This produces the error
    /// ```text
    /// error[E0080]: evaluation of `<Unsigned<15, u32> as UnsignedSizeCheck>::SIZE_CHECK` failed
    ///    --> manual_additions/unsigned.rs
    ///     |
    ///     | impl_usc!(u8, u16, u32, u64, u128);
    ///     | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ evaluation panicked: Type `u32` is not optimally sized for bound 15. Use type `u16` instead.
    ///     |
    ///     = note: this error originates in the macro `const_panic::concat_panic` which comes from the expansion of the macro `impl_usc` (in Nightly builds, run with -Z macro-backtrace for more info)
    ///
    /// note: erroneous constant encountered
    ///   --> manual_additions/unsigned.rs
    ///    |
    ///    |         let _ = Self::SIZE_CHECK;
    ///    |                 ^^^^^^^^^^^^^^^^
    /// ```
    /// Or when requesting a unsigned 0 bit number:
    /// ```compile_fail
    /// # use bittide_hal::manual_additions::unsigned::{Unsigned, UnsignedSizeCheck};
    /// trait Marker {
    ///     fn nothing();
    /// }
    ///
    /// impl Marker for Unsigned<0, u32> {
    ///     fn nothing() {
    ///         let _ = Self::SIZE_CHECK;
    ///     }
    /// }
    /// ```
    /// ```text
    /// error[E0080]: evaluation of `<Unsigned<0, u32> as UnsignedSizeCheck>::SIZE_CHECK` failed
    ///    --> manual_additions/unsigned.rs
    ///     |
    ///     | impl_usc!(u8, u16, u32, u64, u128);
    ///     | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ evaluation panicked: Cannot represent Unsigned<0, T>!
    ///     |
    ///     = note: this error originates in the macro `const_panic::concat_panic` which comes from the expansion of the macro `impl_usc` (in Nightly builds, run with -Z macro-backtrace for more info)
    ///
    /// note: erroneous constant encountered
    ///   --> manual_additions/unsigned.rs
    ///    |
    ///    |         let _ = Self::SIZE_CHECK;
    ///    |                 ^^^^^^^^^^^^^^^^
    /// ```
    const SIZE_CHECK: ();
    /// Backing type of this unsigned number
    type Inner: Copy;
    /// Perform a bounds check on an instance of an unsigned number. Returns `true` if within
    /// bounds, `false` if not.
    fn inner_bounds_check(val: Self::Inner) -> bool;
}

/// Type alias to access the backing type of an unsigned number
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
    /// This function may error if invoked as `Unsigned::<N, T>::new_unchecked(...)` and the wrong
    /// backing type `T` is chosen. Please read the whole error message carefully, it should tell
    /// you what to do to fix it.
    ///
    /// # Safety
    ///
    /// Due to the intended use-case of interfacing with Clash hardware, and there being no
    /// guarantee of behaviour in the case that an out-of-bounds value is written to a register,
    /// this function has been marked as `unsafe`. To make calling this function safe, you must
    /// guarantee that `val` is in the range `0..2.pow(N)`.
    ///
    /// There's one exception to this: it is always safe to call `new_unchecked` if `N` is the same
    /// number of bits as `T`. For example, `Unsigned::<8, u8>::new_unchecked` is always safe.
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
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Unsigned<{N}>({:?})", self.0)
    }
}

impl<const N: u8, T> core::fmt::Display for Unsigned<N, T>
where
    Self: UnsignedSizeCheck,
    T: core::fmt::Display,
{
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<const N: u8, T> ufmt::uDebug for Unsigned<N, T>
where
    Self: UnsignedSizeCheck,
    T: ufmt::uDebug,
{
    #[inline]
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
    #[inline]
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

impl<const N: u8, T> super::seal::Seal for Unsigned<N, T> where T: super::seal::Seal {}

/// Trait that guarantees parts of the [`Unsigned<N, T>`] inherent implementations
///
/// Intended for use where it is not desirable to constrain a type to be `T = Unsigned<N, T>` but
/// rather to `T: UnsignedInterface`.
pub trait UnsignedInterface: Sized + UnsignedSizeCheck + super::seal::Seal {
    /// Instantiate a new [`Unsigned<N, T>`]
    ///
    /// Returns `Some(_)` if `val` fits within `N` bits, returns `None` otherwise.
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `Unsigned::<N, T>::uns_new(...)` and the wrong backing
    /// type `T` is chosen. Please read the whole error message carefully, it should tell you what
    /// to do to fix it.
    fn uns_new(val: Self::Inner) -> Option<Self>;
    /// Instantiate a new [`Unsigned<N, T>`] without performing a bounds check
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `Unsigned::<N, T>::uns_new_unchecked(...)` and the
    /// wrong backing type `T` is chosen. Please read the whole error message carefully, it should
    /// tell you what to do to fix it.
    ///
    /// # Safety
    ///
    /// Due to the intended use-case of interfacing with Clash hardware, and there being no
    /// guarantee of behaviour in the case that an out-of-bounds value is written to a register,
    /// this function has been marked as `unsafe`. To make calling this function safe, you must
    /// guarantee that `val` is in the range `0..2.pow(N)`.
    unsafe fn uns_new_unchecked(val: Self::Inner) -> Self;
    /// Unwrap the inner value contained by this [`Unsigned<N, T>`]
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
