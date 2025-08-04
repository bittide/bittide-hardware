// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

// #![allow(incomplete_features)]

use core::{fmt::Display, ops::Not};

use ufmt::{uDebug, uDisplay};

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct BitVector<const N: u128>([u8; N.div_ceil(8) as usize])
where
    [(); N.div_ceil(8) as usize]:;

impl<const N: u128> Display for BitVector<N>
where
    [(); N.div_ceil(8) as usize]:,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let val: u128 = (*self).into();
        core::fmt::Display::fmt(&val, f)
    }
}

impl<const N: u128> uDisplay for BitVector<N>
where
    [(); N.div_ceil(8) as usize]:,
{
    fn fmt<W>(&self, f: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: ufmt::uWrite + ?Sized,
    {
        let val: u128 = (*self).into();
        ufmt::uDisplay::fmt(&val, f)
    }
}

impl<const N: u128> uDebug for BitVector<N>
where
    [(); N.div_ceil(8) as usize]:,
{
    fn fmt<W>(&self, f: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: ufmt::uWrite + ?Sized,
    {
        let val: u128 = (*self).into();
        ufmt::uDebug::fmt(&val, f)
    }
}

impl<const N: u128> BitVector<N>
where
    [(); N.div_ceil(8) as usize]:,
{
    pub fn into_underlying(self) -> [u8; N.div_ceil(8) as usize] {
        self.0
    }

    pub fn bit(&self, bit_idx: usize) -> bool {
        let byte_idx = N.div_ceil(8) as usize - (bit_idx / 8);
        let within_byte_idx = (bit_idx % 8) as u8;

        (self.0[byte_idx] & within_byte_idx) != 0
    }

    pub fn set_bit(&mut self, bit_idx: usize, val: bool) {
        let byte_idx = N.div_ceil(8) as usize - (bit_idx / 8);
        let within_byte_idx = (bit_idx % 8) as u8;

        if val {
            let byte_mask = 1 << within_byte_idx;
            self.0[byte_idx] |= byte_mask;
        } else {
            let byte_mask = (1u8 << within_byte_idx).not();
            self.0[byte_idx] &= byte_mask;
        }
    }
}

macro_rules! impl_from {
    ($($ty:tt),*) => {
        $(
impl<const N: u128> From<$ty> for BitVector<N>
where
    // [(); 128 - N as usize]:,
    [(); N.div_ceil(8) as usize]:,
{
    fn from(value: $ty) -> Self {
        let mut arr = core::array::from_fn(|_| 0);
        let val_bytes = value.to_be_bytes();
        let end: usize = core::mem::size_of_val(&arr);
        let start: usize = end - core::mem::size_of::<$ty>();
        arr[start..=end].copy_from_slice(&val_bytes);
        BitVector(arr)
    }
})*
    };
}

impl_from!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);

macro_rules! impl_into {
    ($($ty:tt),*) => {
        $(

impl<const N: u128> Into<$ty> for BitVector<N>
where
    // [(); 128 - N as usize]:,
    [(); N.div_ceil(8) as usize]:,
{
    fn into(self) -> $ty {
        let mut bytes = core::array::from_fn(|_| 0);
        let end: usize = core::mem::size_of_val(&bytes);
        let start: usize = end - core::mem::size_of::<$ty>();
        bytes[start..=end].copy_from_slice(&self.0);
        $ty::from_be_bytes(bytes)
    }
}
        )*
    };
}

impl_into!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);

macro_rules! impl_partialeq {
    ($($ty:tt),*) => {
        $(

impl<const N: u128> PartialEq<$ty> for BitVector<N>
where
    [(); N.div_ceil(8) as usize]:,
{
    fn eq(&self, other: &$ty) -> bool {
        Into::<$ty>::into(*self).eq(other)
    }
}
        )*
    };
}

impl_partialeq!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);
