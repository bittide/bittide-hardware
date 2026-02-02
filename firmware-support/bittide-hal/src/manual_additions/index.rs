// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use super::{signed::Signed, unsigned::Unsigned, FromAs};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Index<const N: u128, T>(pub(crate) T);

pub trait IndexSizeCheck {
    const SIZE_CHECK: ();
    const N_: u128;
    type Inner: Copy;
    const N_AS_INNER: Self::Inner;
    fn inner_bounds_check(val: Self::Inner) -> bool;
    const MAX: Self::Inner;
    const IMAX: Self;
}

pub type IndexInner<T> = <T as IndexSizeCheck>::Inner;

macro_rules! impl_isc {
    ($($t:ty),+$(,)?) => {
        $(
            impl<const N: u128> IndexSizeCheck for Index<N, $t> {
                const SIZE_CHECK: () = {
                    if N == 0 {
                        panic!("Cannot represent Index<0, T>!");
                    }
                    // `u128::BITS - N.leading_zeros()` is equivalent to `N.clog2()`
                    let correct_size = match
                        (u128::BITS - (N - 1).leading_zeros()).next_power_of_two()
                    {
                        size if size >= 8 => size,
                        _ => 8,
                    };
                    if N - 1 > <$t>::MAX as u128 {
                        const_panic::concat_panic!(
                            const_panic::fmt::FmtArg::DISPLAY;
                            "Max bound ",
                            N,
                            " cannot fit in type `",
                            stringify!($t),
                            "` (max value: ",
                            <$t>::MAX as u128 + 1,
                            ")"
                        );
                    } else if correct_size != <$t>::BITS {
                        const_panic::concat_panic!(
                            const_panic::fmt::FmtArg::DISPLAY;
                            "Type `",
                            stringify!($t),
                            "` is not optimally sized for bound ",
                            N,
                            ". Use type `u",
                            correct_size,
                            "` instead.",
                        );
                    }
                };
                const N_: u128 = N;
                type Inner = $t;
                // NOTE
                // Casting `N` as `$t` will always work, because the above `const SIZE_CHECK` will
                // produce a compile error if `N` doesn't fit into the type `$t`.
                const N_AS_INNER: $t = {
                    let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
                    N as $t
                };
                #[inline]
                fn inner_bounds_check(val: $t) -> bool {
                    let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
                    if const { N - 1 == <$t>::MAX as u128 } {
                        true
                    } else {
                        // NOTE
                        // Casting `N` as `$t` will always work, because the above
                        // `const SIZE_CHECK` will produce a compile error if `N` doesn't fit into
                        // the type `$t`.
                        val < Self::N_AS_INNER
                    }
                }
                const MAX: $t = (N - 1) as $t;
                const IMAX: Self = Index(Self::MAX);
            }
        )+
    };
}

impl_isc!(u8, u16, u32, u64); // READ THE REST OF THE ERROR MESSAGE

impl<const N: u128> IndexSizeCheck for Index<N, u128> {
    const SIZE_CHECK: () = {
        if N == 0 {
            panic!("Cannot represent Index<0>!");
        }
        // `u128::BITS - (N - 1).leading_zeros()` is equivalent to `N.clog2()`
        let correct_size = match (u128::BITS - (N - 1).leading_zeros()).next_power_of_two() {
            size if size >= 8 => size,
            _ => 8,
        };
        if correct_size != u128::BITS {
            const_panic::concat_panic!(
                const_panic::fmt::FmtArg::DISPLAY;
                "Type u128 is not optimally sized for bound ",
                N,
                ". Use type `u",
                correct_size,
                "` instead.",
            );
        }
    };
    const N_: u128 = N;
    type Inner = u128;
    const N_AS_INNER: u128 = {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        N
    };
    #[inline]
    fn inner_bounds_check(val: u128) -> bool {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        val < N
    }
    const MAX: u128 = N - 1;
    const IMAX: Self = Index(Self::MAX);
}

impl<const N: u128, T> Index<N, T>
where
    T: Copy,
    Index<N, T>: IndexSizeCheck<Inner = T>,
{
    /// Instantiate a new `Index<N, T>`
    ///
    /// Returns `Some(_)` if `val` is in the range `0..N`, returns `None` otherwise.
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `Index::<N, T>::new(...)` and the wrong backing type
    /// `T` is chosen. Please read the whole error message carefully, it should tell you what to do
    /// to fix it.
    #[inline]
    pub fn new(val: T) -> Option<Index<N, T>> {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        if Index::<N, T>::inner_bounds_check(val) {
            Some(Index(val))
        } else {
            None
        }
    }

    /// Instantiate a new `Index<N, T>` without performing a bounds check
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `Index::<N, T>::new(...)` and the wrong backing type
    /// `T` is chosen. Please read the whole error message carefully, it should tell you what to do
    /// to fix it.
    ///
    /// # Safety
    ///
    /// Due to the intended use-case of interfacing with Clash hardware, and there being no
    /// guarantee of behaviour in the case that an out-of-bounds value is written to a register,
    /// this function has been marked as `unsafe`. To make calling this function safe, you must
    /// guarantee that `val` is in the range `0..N`.
    #[inline]
    pub unsafe fn new_unchecked(val: T) -> Index<N, T> {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        Index(val)
    }

    /// Unwrap the inner value contained by this `Index<N, T>`
    #[inline]
    pub fn into_inner(self) -> T {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        self.0
    }
}

impl<const N: u128, T> core::fmt::Debug for Index<N, T>
where
    Self: IndexSizeCheck,
    T: core::fmt::Debug,
{
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Index<{N}>({:?})", self.0)
    }
}

impl<const N: u128, T> core::fmt::Display for Index<N, T>
where
    Self: IndexSizeCheck,
    T: core::fmt::Display,
{
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[inline]
const fn clog2_cgu128<const N: u128>() -> usize {
    let mut m = 0;
    let mut n = 1;
    while n - 1 < N {
        m += 1;
        let Some(n_next) = n.checked_mul(10) else {
            break;
        };
        n = n_next;
    }
    m
}

const U128_MAXLEN: usize = clog2_cgu128::<{ u128::MAX }>() + 1;
type CGu128FmtData = [u8; U128_MAXLEN];

struct CGU128Formatter<const N: u128>;

impl<const N: u128> CGU128Formatter<N> {
    const DATA: CGu128FmtData = {
        let clog2 = clog2_cgu128::<N>();
        let mut data = [0; U128_MAXLEN];
        let mut idx = 0;
        let mut acc = N;
        while idx < clog2 {
            data[clog2 - idx - 1] = b'0' + (acc % 10) as u8;
            acc /= 10;
            idx += 1;
        }
        data
    };
    const STR: &'static str = {
        let clog2 = clog2_cgu128::<N>();
        unsafe {
            core::str::from_utf8_unchecked(core::slice::from_raw_parts(Self::DATA.as_ptr(), clog2))
        }
    };
}

impl<const N: u128, T> ufmt::uDebug for Index<N, T>
where
    T: ufmt::uDebug,
{
    #[inline]
    fn fmt<W>(&self, w: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: ufmt::uWrite + ?Sized,
    {
        ufmt::uwrite!(w, "Index<{}>({:?})", CGU128Formatter::<N>::STR, self.0)
    }
}

impl<const N: u128, T> ufmt::uDisplay for Index<N, T>
where
    T: ufmt::uDisplay,
{
    #[inline]
    fn fmt<W>(&self, w: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: ufmt::uWrite + ?Sized,
    {
        ufmt::uwrite!(w, "Index<{}>({})", CGU128Formatter::<N>::STR, self.0)
    }
}

impl<const N: u128, T> ufmt::uDisplayHex for Index<N, T>
where
    Self: IndexSizeCheck,
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

impl<const N: u128, T> super::seal::Seal for Index<N, T> where T: super::seal::Seal {}

pub trait IndexInterface: Sized + IndexSizeCheck + super::seal::Seal {
    /// Instantiate a new `Index<N, T>`
    ///
    /// Returns `Some(_)` if `val` is in the range `0..N`, returns `None` otherwise.
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `Index::<N, T>::new(...)` and the wrong backing type
    /// `T` is chosen. Please read the whole error message carefully, it should tell you what to do
    /// to fix it.
    fn idx_new(val: Self::Inner) -> Option<Self>;
    /// Instantiate a new `Index<N, T>` without performing a bounds check
    ///
    /// # Errors
    ///
    /// This function may error if invoked as `Index::<N, T>::new(...)` and the wrong backing type
    /// `T` is chosen. Please read the whole error message carefully, it should tell you what to do
    /// to fix it.
    ///
    /// # Safety
    ///
    /// Due to the intended use-case of interfacing with Clash hardware, and there being no
    /// guarantee of behaviour in the case that an out-of-bounds value is written to a register,
    /// this function has been marked as `unsafe`. To make calling this function safe, you must
    /// guarantee that `val` is in the range `0..N`.
    unsafe fn idx_new_unchecked(val: Self::Inner) -> Self;
    /// Unwrap the inner value contained by this `Index<N, T>`
    fn idx_into_inner(self) -> Self::Inner;
}

impl<const N: u128, T> IndexInterface for Index<N, T>
where
    T: Copy + super::seal::Seal,
    Index<N, T>: IndexSizeCheck<Inner = T>,
{
    #[inline]
    fn idx_new(val: Self::Inner) -> Option<Self> {
        Index::<N, T>::new(val)
    }

    #[inline]
    unsafe fn idx_new_unchecked(val: Self::Inner) -> Self {
        Index::<N, T>::new_unchecked(val)
    }

    #[inline]
    fn idx_into_inner(self) -> Self::Inner {
        self.into_inner()
    }
}

macro_rules! def_index_from {
    // Entry
    ($($t:ty),+$(,)?) => {
        def_index_from! {
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
            def_index_from! {
                @def
                sup: $h,
                sub: $sub,
            }
        )*

        // Push big type to list of smaller types
        def_index_from! {
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
        impl<const N: u128> From<$sub> for Index<N, $sup> {
            #[inline]
            fn from(value: $sub) -> Self {
                Index(value as $sup)
            }
        }

        impl<const M: u128, const N: u128> From<Index<N, $sub>> for Index<M, $sup> {
            #[inline]
            fn from(value: Index<N, $sub>) -> Self {
                Index(value.0 as $sup)
            }
        }

        impl<const M: u128, const N: u8> From<Unsigned<N, $sub>> for Index<M, $sup> {
            #[inline]
            fn from(value: Unsigned<N, $sub>) -> Self {
                Index(value.0 as $sup)
            }
        }

        impl<const M: u128, const N: u8> From<Signed<N, $sub>> for Index<M, $sup> {
            #[inline]
            fn from(value: Signed<N, $sub>) -> Self {
                Index(value.0 as $sup)
            }
        }
        impl<const N: u128> FromAs<$sub> for Index<N, $sup> {
            #[inline]
            fn from_as(value: $sub) -> Self {
                Index(value as $sup)
            }
        }

        impl<const M: u128, const N: u128> FromAs<Index<N, $sub>> for Index<M, $sup> {
            #[inline]
            fn from_as(value: Index<N, $sub>) -> Self {
                Index(value.0 as $sup)
            }
        }

        impl<const M: u128, const N: u8> FromAs<Unsigned<N, $sub>> for Index<M, $sup> {
            #[inline]
            fn from_as(value: Unsigned<N, $sub>) -> Self {
                Index(value.0 as $sup)
            }
        }

        impl<const M: u128, const N: u8> FromAs<Signed<N, $sub>> for Index<M, $sup> {
            #[inline]
            fn from_as(value: Signed<N, $sub>) -> Self {
                Index(value.0 as $sup)
            }
        }
    }
}

def_index_from!(u8, u16, u32, u64, u128);

#[cfg(target_pointer_width = "16")]
def_index_from! {
    @run
    sup: [usize],
    sub: [u8],
}

#[cfg(target_pointer_width = "32")]
def_index_from! {
    @run
    sup: [usize],
    sub: [u8, u16],
}

#[cfg(target_pointer_width = "64")]
def_index_from! {
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
        impl<const N: u128, T> From<Index<N, T>> for INTO
        where
            Index<N, T>: IndexSizeCheck,
            INTO: From<T>,
        {
            #[inline]
            fn from(other: Index<N, T>) -> INTO {
                INTO::from(other.0)
            }
        }

        impl<const N: u128, T> FromAs<Index<N, T>> for INTO
        where
            Index<N, T>: IndexSizeCheck,
            INTO: FromAs<T>,
        {
            #[inline]
            fn from_as(other: Index<N, T>) -> INTO {
                INTO::from_as(other.0)
            }
        }
    }
}
