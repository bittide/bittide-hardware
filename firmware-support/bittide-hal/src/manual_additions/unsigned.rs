// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Unsigned<const N: u8, T>(pub(crate) T);

pub trait UnsignedSizeCheck {
    const CORRECT_SIZE: u8;
    const SIZE_CHECK: ();
    type Inner: Copy;
    fn inner_bounds_check(val: Self::Inner) -> bool;
}

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
    pub unsafe fn new_unchecked(val: T) -> Unsigned<N, T> {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        Unsigned(val)
    }

    /// Unwrap the inner value contained by this `Unsigned<N, T>`
    pub fn into_inner(self) -> T {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        self.0
    }
}
