// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
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
    fn inner_bounds_check(val: &[u8; N]) -> bool {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        if const { M.is_multiple_of(8) } {
            true
        } else {
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
    pub unsafe fn new_unchecked(val: [u8; N]) -> BitVector<M, N> {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        BitVector(val)
    }

    /// Unwrap the inner value contained by this `BitVector<M, N>`
    pub fn into_inner(self) -> [u8; N] {
        let _: () = Self::SIZE_CHECK; // READ THE REST OF THE ERROR MESSAGE
        self.0
    }
}
