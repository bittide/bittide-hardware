// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![cfg_attr(not(test), no_std)]
#![feature(
    bigint_helper_methods,
    const_cmp,
    const_convert,
    const_destruct,
    const_index,
    const_ops,
    const_option_ops,
    const_trait_impl,
    formatting_options,
    generic_const_exprs,
    iter_intersperse,
    macro_metavar_expr
)]
#![allow(incomplete_features)]
#![recursion_limit = "1024"]

pub mod bitvec;
pub mod index;
pub mod signed;
pub mod traits;
pub mod unsigned;

#[cfg(not(doctest))]
pub(crate) mod macros;
#[cfg(doctest)]
pub mod macros;

/// For a desired `BitVec<N>` width `n`, produces a `u8` that can be used to perform the lookup in
/// the trait LUT.
///
/// # Panics
///
/// Panics if the input is 0.
#[must_use]
pub const fn bitvector_size(n: u16) -> u8 {
    assert!(n != 0, "Cannot represent 0 bits!");
    let nbytes = n.div_ceil(8);
    if (nbytes as usize) <= size_of::<u128>() {
        nbytes.next_power_of_two() as u8
    } else {
        17
    }
}

/// For a desired numeric type `n` bits wide, produces a `u8` that can be used to perform the lookup
/// in a trait LUT.
///
/// # Panics
///
/// Panics if the input is 0.
// TODO: arbitrary-width support. For this, change `n: u8` to `n: u16`
#[must_use]
pub const fn numeric_size(n: u8) -> u8 {
    assert!(n != 0, "Cannot represent 0 bits!");
    let nbytes = n.div_ceil(8);
    if (nbytes as usize) <= size_of::<u128>() {
        nbytes.next_power_of_two()
    } else {
        const_panic::concat_panic!("Cannot make backer for ", n, " bits");
        // TODO: arbitrary-width support. For this, return `17`.
    }
}

/// For a desired indexable range of `0..n`, produces a `u128` that can be used to perform the lookup
/// in the trait LUT.
///
/// # Panics
///
/// Panics if the input is 0.
#[must_use]
pub const fn index_bit_size(n: u128) -> u8 {
    if n == 0 {
        panic!("Cannot represent `Index<0>`!");
    } else if n <= u8::MAX as u128 + 1 {
        8
    } else if n <= u16::MAX as u128 + 1 {
        16
    } else if n <= u32::MAX as u128 + 1 {
        32
    } else if n <= u64::MAX as u128 + 1 {
        64
    } else {
        128
    }
}

/// Marker struct for use in constraints with `generic_const_exprs`
pub struct ConstCheck<const B: bool>;

/// Marks a constraint as `true`
pub trait True {}
impl True for ConstCheck<true> {}
/// Marks a constraint as `false`
pub trait False {}
impl False for ConstCheck<false> {}
