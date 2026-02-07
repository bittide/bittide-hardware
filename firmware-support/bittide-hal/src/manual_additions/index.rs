// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Index<const N: u128, T>(pub(crate) T);

pub trait IndexSizeCheck {
    const SIZE_CHECK: ();
    type Inner: Copy;
    const N_AS_INNER: Self::Inner;
    fn inner_bounds_check(val: Self::Inner) -> bool;
}

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
                    if N > <$t>::MAX as u128 + 1 {
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
                type Inner = $t;
                // NOTE
                // Casting `N` as `$t` will always work, because the above `const SIZE_CHECK` will
                // produce a compile error if `N` doesn't fit into the type `$t`.
                const N_AS_INNER: $t = {
                    let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
                    N as $t
                };
                fn inner_bounds_check(val: $t) -> bool {
                    let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
                    if const { N == <$t>::MAX as u128 + 1 } {
                        true
                    } else {
                        // NOTE
                        // Casting `N` as `$t` will always work, because the above
                        // `const SIZE_CHECK` will produce a compile error if `N` doesn't fit into
                        // the type `$t`.
                        val < Self::N_AS_INNER
                    }
                }
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
        // `u128::BITS - N.leading_zeros()` is equivalent to `N.clog2()`
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
    type Inner = u128;
    const N_AS_INNER: u128 = {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        N
    };
    fn inner_bounds_check(val: u128) -> bool {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        val < N
    }
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
    pub unsafe fn new_unchecked(val: T) -> Index<N, T> {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        Index(val)
    }

    /// Unwrap the inner value contained by this `Index<N, T>`
    pub fn into_inner(self) -> T {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        self.0
    }
}
