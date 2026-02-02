// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

pub mod bitvector;
pub mod calendar;
pub mod capture_ugn;
pub mod dna;
pub mod elastic_buffer;
pub mod index;
pub mod scatter_gather_pe;
pub mod si539x_spi;
pub mod signed;
pub mod soft_ugn_demo_gppe;
pub mod timer;
pub mod uart;
pub mod unsigned;

// Sealing the `FromAs` and `IntoAs` traits with a supertrait
mod seal {
    pub trait Seal {}

    subst_macros::repeat_parallel_subst! {
        groups: [
            [group [sub [TYPE] = [u8]]]
            [group [sub [TYPE] = [u16]]]
            [group [sub [TYPE] = [u32]]]
            [group [sub [TYPE] = [u64]]]
            [group [sub [TYPE] = [u128]]]
            [group [sub [TYPE] = [usize]]]
            [group [sub [TYPE] = [i8]]]
            [group [sub [TYPE] = [i16]]]
            [group [sub [TYPE] = [i32]]]
            [group [sub [TYPE] = [i64]]]
            [group [sub [TYPE] = [i128]]]
            [group [sub [TYPE] = [isize]]]
        ],
        callback: NONE,
        in: {
            impl Seal for TYPE {}
        }
    }
}

/// Trait-level guarantee that `self as U` is valid for `self: T`
pub trait FromAs<T>: seal::Seal {
    fn from_as(other: T) -> Self;
}

subst_macros::repeat_parallel_subst! {
    groups: [
        [group [sub [SELF] = [u8]]]
        [group [sub [SELF] = [u16]]]
        [group [sub [SELF] = [u32]]]
        [group [sub [SELF] = [u64]]]
        [group [sub [SELF] = [u128]]]
        [group [sub [SELF] = [usize]]]
        [group [sub [SELF] = [i8]]]
        [group [sub [SELF] = [i16]]]
        [group [sub [SELF] = [i32]]]
        [group [sub [SELF] = [i64]]]
        [group [sub [SELF] = [i128]]]
        [group [sub [SELF] = [isize]]]
    ],
    callback: [
        macro: subst_macros::repeat_parallel_subst,
        prefix: [
            @callback
            groups: [
                [group [sub [OTHER] = [u8]]]
                [group [sub [OTHER] = [u16]]]
                [group [sub [OTHER] = [u32]]]
                [group [sub [OTHER] = [u64]]]
                [group [sub [OTHER] = [u128]]]
                [group [sub [OTHER] = [usize]]]
                [group [sub [OTHER] = [i8]]]
                [group [sub [OTHER] = [i16]]]
                [group [sub [OTHER] = [i32]]]
                [group [sub [OTHER] = [i64]]]
                [group [sub [OTHER] = [i128]]]
                [group [sub [OTHER] = [isize]]]
            ],
            callback: NONE,
        ],
        suffix: [],
    ],
    in: {
        impl FromAs<OTHER> for SELF {
            fn from_as(other: OTHER) -> SELF {
                other as SELF
            }
        }
    }
}

/// Inverse of [`FromAs`]
pub trait IntoAs<T>: seal::Seal {
    fn into_as(self) -> T;
}

impl<T, U> IntoAs<U> for T
where
    T: seal::Seal,
    U: FromAs<T>,
{
    fn into_as(self) -> U {
        U::from_as(self)
    }
}
