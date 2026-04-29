// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

mod clock_config;

use clock_config::{ClockConfigParser, ConfigEntry, ParserState};
use proc_macro_crate::{FoundCrate, crate_name};
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use std::{fmt::Write, fs};
use syn::{LitInt, LitStr, Token, parse::Parse, parse_macro_input};

fn get_import_prefix() -> TokenStream {
    let prefix = match crate_name("bittide-hal") {
        Ok(FoundCrate::Itself) => "crate",
        _ => "bittide_hal",
    };
    let prefix = Ident::new(prefix, Span::call_site());
    quote! { #prefix::manual_additions }
}

/// Macro to conveniently create an indexed type
///
/// # Example
///
/// ```rust,ignore
/// # use bittide_macros::Index;
/// fn test(v: Index![12]) {
///     // ...
/// #   _ = v;
/// }
/// ```
#[proc_macro]
#[allow(non_snake_case)]
pub fn Index(toks: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let n_lit = parse_macro_input!(toks as LitInt);

    fn optimal_backing_type_size(n: u128) -> u32 {
        let clog2 = (u128::BITS - (n - 1).leading_zeros()).next_power_of_two();
        clog2.max(8)
    }

    let n_val = match n_lit.base10_parse() {
        Err(err) => return err.into_compile_error().into(),
        Ok(v) => v,
    };

    let ty = {
        let type_name = format!("u{}", optimal_backing_type_size(n_val));
        syn::Ident::new(&type_name, n_lit.span())
    };

    let prefix = get_import_prefix();

    quote! {
        #prefix::index::Index<#n_lit, #ty>
    }
    .into()
}

/// Macro to conveniently create an indexed value
///
/// # Example
///
/// ```rust,ignore
/// # use bittide_macros::index;
/// # fn main() {
/// let x = index!(4, n = 10);
/// // This does not compile as 9 is out of bounds
/// // let y = index!(9, n = 8);
/// let y = 4;
/// let z = index!(y, n = 12); // runtime check
/// #}
/// ```
#[proc_macro]
pub fn index(toks: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(toks as IndexValArgs);

    let val = args.value;
    let n_lit = args.n_lit;

    fn optimal_backing_type_size(n: u128) -> u32 {
        let clog2 = (u128::BITS - (n - 1).leading_zeros()).next_power_of_two();
        clog2.max(8)
    }

    let ty = {
        let type_name = format!("u{}", optimal_backing_type_size(args.n_val));
        syn::Ident::new(&type_name, n_lit.span())
    };

    let prefix = get_import_prefix();

    match val {
        // try to detect a constant literal to skip the bounds check
        syn::Expr::Lit(syn::ExprLit {
            attrs: _,
            lit: syn::Lit::Int(int_lit),
        }) => {
            let n: u128 = match int_lit.base10_parse() {
                Err(err) => return err.into_compile_error().into(),
                Ok(v) => v,
            };
            // if statically in bounds, skip bounds check
            if n < args.n_val {
                quote! {
                    unsafe {
                        #prefix::index::Index::<#n_lit, #ty>::new_unchecked(
                            #int_lit
                        )
                    }
                }
                .into()
            } else {
                // if not bounds, oops!
                syn::Error::new(
                    int_lit.span().join(n_lit.span()).unwrap(),
                    format!(
                        "Value out of bounds: value {}, type Index<{}>",
                        n, args.n_val
                    ),
                )
                .into_compile_error()
                .into()
            }
        }
        // non literal case, do slow bounds check
        _ => if args.n_val == optimal_backing_type_size(args.n_val) as u128 {
            quote! {
                unsafe {
                    #prefix::index::Index::<#n_lit, #ty>::new_unchecked(#val)
                }
            }
        } else {
            quote! {
                #prefix::index::Index::<#n_lit, #ty>::new(#val).unwrap()
            }
        }
        .into(),
    }
}

struct IndexValArgs {
    value: syn::Expr,
    n_lit: syn::LitInt,
    n_val: u128,
}

impl Parse for IndexValArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let value = input.parse()?;
        input.parse::<Token![,]>()?;
        let n_ident = input.parse::<syn::Ident>()?;
        if n_ident != "n" {
            return Err(syn::Error::new(
                n_ident.span(),
                "only `n` is allowed here. Example `index!(12, n = 20)`",
            ));
        }
        input.parse::<Token![=]>()?;
        let n_lit: syn::LitInt = input.parse()?;
        let n_val = n_lit.base10_parse()?;

        Ok(IndexValArgs {
            value,
            n_lit,
            n_val,
        })
    }
}

#[proc_macro]
#[allow(non_snake_case)]
pub fn Signed(toks: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let n_lit = parse_macro_input!(toks as LitInt);

    fn optimal_backing_type_size(n: u8) -> u8 {
        (n.div_ceil(8).next_power_of_two() * 8).max(8)
    }

    let n_val = match n_lit.base10_parse() {
        Err(err) => return err.into_compile_error().into(),
        Ok(v) => v,
    };

    let ty = {
        let type_name = format!("i{}", optimal_backing_type_size(n_val));
        syn::Ident::new(&type_name, n_lit.span())
    };

    let prefix = get_import_prefix();

    quote! {
        #prefix::signed::Signed<#n_lit, #ty>
    }
    .into()
}

#[proc_macro]
pub fn signed(toks: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(toks as NumericArgs);

    let val = args.value;
    let nlit = args.n_lit;
    let nval = args.n_val;

    fn optimal_backing_type_size(n: u8) -> u8 {
        (n.div_ceil(8).next_power_of_two() * 8).max(8)
    }

    let ty = {
        let type_name = format!("i{}", optimal_backing_type_size(nval));
        syn::Ident::new(&type_name, nlit.span())
    };

    let uty = {
        let type_name = format!("u{}", optimal_backing_type_size(nval));
        syn::Ident::new(&type_name, nlit.span())
    };

    let prefix = get_import_prefix();

    match val {
        // try to detect a constant literal to skip the bounds check
        syn::Expr::Lit(syn::ExprLit {
            attrs: _,
            lit: syn::Lit::Int(int_lit),
        }) => {
            let litval: i128 = match int_lit.base10_parse::<u128>() {
                Err(err) => return err.into_compile_error().into(),
                Ok(v) => v as i128,
            };
            let lit_is_neg = litval & (1 << (nval - 1)) != 0;
            let litval = if lit_is_neg {
                litval | (!0 << (nval - 1))
            } else {
                litval
            };
            let neg_bound = !0 << (nval - 1);
            let pos_bound = !(!0 << (nval - 1));
            if (neg_bound..=pos_bound).contains(&litval) {
                if lit_is_neg {
                    quote! {
                        unsafe {
                            #prefix::signed::Signed::<#nlit, #ty>::new_unchecked(const {
                                const LIT: #uty = #int_lit;
                                LIT as #ty | (!0 << (#nval - 1))
                            })
                        }
                    }
                } else {
                    quote! {
                        unsafe {
                            #prefix::signed::Signed::<#nlit, #ty>::new_unchecked(const {
                                const LIT: #uty = #int_lit;
                                LIT as #ty
                            })
                        }
                    }
                }
                .into()
            } else {
                let bounds = neg_bound..=pos_bound;
                syn::Error::new(
                    int_lit.span().join(nlit.span()).unwrap_or(int_lit.span()),
                    format!(
                        "Value not in {bounds:?}: value `{litval}`, type Signed<{nlit}, {nval}>"
                    ),
                )
                .into_compile_error()
                .into()
            }
        }
        _ => if nval == optimal_backing_type_size(nval) {
            quote! {
                unsafe {
                    #prefix::signed::Signed::<#nlit, #ty>::new_unchecked(#val)
                }
            }
        } else {
            quote! {
                #prefix::signed::Signed::<#nlit, #ty>::new(#val).expect(concat!(
                    "Failed to construct `Signed<",
                    stringify!(#nlit),
                    ", ",
                    stringify!(#ty),
                    ">` at line ",
                    line!(),
                    ", column ",
                    column!(),
                ))
            }
        }
        .into(),
    }
}

#[proc_macro]
#[allow(non_snake_case)]
pub fn Unsigned(toks: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let n_lit = parse_macro_input!(toks as LitInt);

    fn optimal_backing_type_size(n: u8) -> u8 {
        (n.div_ceil(8).next_power_of_two() * 8).max(8)
    }

    let n_val = match n_lit.base10_parse() {
        Err(err) => return err.into_compile_error().into(),
        Ok(v) => v,
    };

    let ty = {
        let type_name = format!("u{}", optimal_backing_type_size(n_val));
        syn::Ident::new(&type_name, n_lit.span())
    };

    let prefix = get_import_prefix();

    quote! {
        #prefix::unsigned::Unsigned<#n_lit, #ty>
    }
    .into()
}

#[proc_macro]
pub fn unsigned(toks: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(toks as NumericArgs);

    let val = args.value;
    let nlit = args.n_lit;
    let nval = args.n_val;

    fn optimal_backing_type_size(n: u8) -> u8 {
        (n.div_ceil(8).next_power_of_two() * 8).max(8)
    }

    let ty = {
        let type_name = format!("u{}", optimal_backing_type_size(nval));
        syn::Ident::new(&type_name, nlit.span())
    };

    let prefix = get_import_prefix();

    match val {
        // try to detect a constant literal to skip the bounds check
        syn::Expr::Lit(syn::ExprLit {
            attrs: _,
            lit: syn::Lit::Int(int_lit),
        }) => {
            let litval: u128 = match int_lit.base10_parse() {
                Err(err) => return err.into_compile_error().into(),
                Ok(v) => v,
            };
            if nval == 128 || (nval < 128 && (0..(1u128 << nval)).contains(&litval)) {
                quote! {
                    unsafe {
                        #prefix::unsigned::Unsigned::<#nlit, #ty>::new_unchecked(
                            #int_lit
                        )
                    }
                }
                .into()
            } else {
                syn::Error::new(
                    int_lit.span().join(nlit.span()).unwrap(),
                    format!("Value out of bounds: value `{litval}`, type Unsigned<{nlit}, {nval}>"),
                )
                .into_compile_error()
                .into()
            }
        }
        _ => if nval == optimal_backing_type_size(nval) {
            quote! {
                unsafe {
                    #prefix::unsigned::Unsigned::<#nlit, #ty>::new_unchecked(#val)
                }
            }
        } else {
            quote! {
                #prefix::unsigned::Unsigned::<#nlit, #ty>::new(#val).expect(concat!(
                    "Failed to construct `Unigned<",
                    stringify!(#nlit),
                    ", ",
                    stringify!(#ty),
                    ">` at line ",
                    line!(),
                    ", column ",
                    column!(),
                ))
            }
        }
        .into(),
    }
}

struct NumericArgs {
    value: syn::Expr,
    n_lit: syn::LitInt,
    n_val: u8,
}

impl Parse for NumericArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let value = input.parse()?;
        input.parse::<Token![,]>()?;
        let n_ident = input.parse::<syn::Ident>()?;
        if n_ident != "n" {
            return Err(syn::Error::new(
                n_ident.span(),
                "only `n` is allowed here. Example: `n = 20`",
            ));
        }
        input.parse::<Token![=]>()?;
        let n_lit: syn::LitInt = input.parse()?;
        let n_val = n_lit.base10_parse()?;
        if n_val > 128 {
            Err(syn::Error::new(
                n_lit.span(),
                format!("Value `{n_val}` is outside of allowed range 0..=128!"),
            ))
        } else {
            Ok(NumericArgs {
                value,
                n_lit,
                n_val,
            })
        }
    }
}

#[proc_macro]
#[allow(non_snake_case)]
pub fn BitVector(toks: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let n_lit = parse_macro_input!(toks as LitInt);

    let n_val: usize = match n_lit.base10_parse() {
        Err(err) => return err.into_compile_error().into(),
        Ok(v) => v,
    };

    let size = n_val.div_ceil(8);

    let prefix = get_import_prefix();

    quote! {
        #prefix::bitvector::BitVector<#n_val, #size>
    }
    .into()
}

fn read_hex_literal(
    input: &LitInt,
    input_str: &str,
    size: usize,
    buf: &mut [u8],
) -> Result<(), proc_macro::TokenStream> {
    let mut char_iter = input_str.chars().filter(char::is_ascii_hexdigit).rev();
    let mut lower = true;
    let mut idx = 0;
    loop {
        let Some(c) = char_iter.next() else {
            break Ok(());
        };
        let val = match c {
            '0'..='9' => c as u8 - b'0',
            'A'..='F' => c as u8 - b'A' + 10,
            'a'..='f' => c as u8 - b'a' + 10,
            _ => unreachable!(),
        };
        match (buf.get_mut(idx), lower) {
            (None, _) => {
                let rem = char_iter.count();
                return Err(syn::Error::new(
                    input.span(),
                    format!("Literal too long! Expected 0..={size} hexdigits, but {rem} left"),
                )
                .into_compile_error()
                .into());
            }
            (Some(byte), true) => *byte |= val,
            (Some(byte), false) => *byte |= val << 4,
        }
        if !lower {
            idx += 1;
        }
        lower = !lower;
    }
}

fn read_binary_literal(
    input: &LitInt,
    input_str: &str,
    size: usize,
    buf: &mut [u8],
) -> Result<(), proc_macro::TokenStream> {
    let mut char_iter = input_str.chars().filter(|&c| c == '0' || c == '1').rev();
    let mut byte_idx = 0;
    let mut idx = 0;
    loop {
        let Some(c) = char_iter.next() else {
            break Ok(());
        };
        let val = c as u8 - b'0';
        match (buf.get_mut(idx), byte_idx) {
            (None, _) => {
                let rem = char_iter.count();
                return Err(syn::Error::new(
                    input.span(),
                    format!("Literal too long! Expected 0..={size} binary digits, but {rem} left"),
                )
                .into_compile_error()
                .into());
            }
            (Some(byte), 7) => {
                *byte |= val << 7;
                byte_idx = 0;
                idx += 1;
            }
            (Some(byte), shift) => {
                *byte |= val << shift;
                byte_idx += 1;
            }
        }
    }
}

#[proc_macro]
pub fn bitvector(toks: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let BitVectorArgs {
        value,
        n_lit,
        n_val,
    } = parse_macro_input!(toks as BitVectorArgs);

    let size = n_val.div_ceil(8);

    let prefix = get_import_prefix();

    match value {
        syn::Expr::Lit(syn::ExprLit {
            attrs: _,
            lit: syn::Lit::Int(input),
        }) => {
            let input_str = input.to_string();
            let mut buf: Vec<u8> = vec![0; size];
            if input_str.starts_with("0x") || input_str.starts_with("0X") {
                if !input_str
                    .chars()
                    .skip(2)
                    .all(|c| c.is_ascii_hexdigit() || c == ' ' || c == '_')
                {
                    return syn::Error::new(
                        input.span(),
                        "Hexadecimal literals may only contain hex digits, spaces, and underscores",
                    )
                    .into_compile_error()
                    .into();
                }
                let input_str = input_str
                    .trim_start_matches('0')
                    .trim_start_matches(['x', 'X']);
                if let Err(err) = read_hex_literal(&input, input_str, size, &mut buf) {
                    return err;
                }
            } else if input_str.starts_with("0b") || input_str.starts_with("0B") {
                if !input_str
                    .chars()
                    .skip(2)
                    .all(|c| c == '0' || c == '1' || c == ' ' || c == '_')
                {
                    return syn::Error::new(
                        input.span(),
                        "Binary literals may only contain `0`s, `1`s, spaces, and underscores",
                    )
                    .into_compile_error()
                    .into();
                }
                let input_str = input_str
                    .trim_start_matches('0')
                    .trim_start_matches(['b', 'B']);
                if let Err(err) = read_binary_literal(&input, input_str, n_val, &mut buf) {
                    return err;
                }
            } else {
                return syn::Error::new(
                    input.span(),
                    concat!(
                        "Expected a hexadecimal (`0x` or `0X` prefix) or binary (`0b` or `0B`",
                        " prefix) literal! Both literals may contain spaces and/or underscores."
                    )
                    .to_string(),
                )
                .into_compile_error()
                .into();
            };
            let check_bit = n_val % 8;
            if check_bit == 0 || buf.last().unwrap().leading_zeros() as usize >= (8 - check_bit) {
                let mut array_str = String::with_capacity(buf.len() * 6);
                array_str.push('[');
                write!(array_str, "0x{:02x}", buf[0]).unwrap();
                for byte in buf.get(1..).into_iter().flatten() {
                    write!(array_str, ", 0x{byte:02x}").unwrap();
                }
                array_str.push(']');
                let toks: proc_macro2::TokenStream = array_str.parse().unwrap();
                quote! {
                    unsafe {
                        #prefix::bitvector::BitVector::<#n_lit, #size>::new_unchecked(#toks)
                    }
                }
                .into()
            } else {
                syn::Error::new(
                    input.span(),
                    format!("Set bits outside allowed range: {:0nb$b}", 0, nb = 1),
                )
                .into_compile_error()
                .into()
            }
        }
        _ => if n_val.is_multiple_of(8) {
            quote! {
                unsafe {
                    #prefix::bitvector::BitVector::<#n_lit, #size>::new_unchecked(#value)
                }
            }
        } else {
            quote! {
                #prefix::bitvector::BitVector::<#n_lit, #size>::new(#value).unwrap()
            }
        }
        .into(),
    }
}

struct BitVectorArgs {
    value: syn::Expr,
    n_lit: syn::LitInt,
    n_val: usize,
}

impl Parse for BitVectorArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let value = input.parse()?;
        input.parse::<Token![,]>()?;
        let n_ident = input.parse::<syn::Ident>()?;
        if n_ident != "n" {
            return Err(syn::Error::new(
                n_ident.span(),
                "only `n` is allowed here. Example: `n = 20`",
            ));
        }
        input.parse::<Token![=]>()?;
        let n_lit: syn::LitInt = input.parse()?;
        let n_val = n_lit.base10_parse()?;
        Ok(BitVectorArgs {
            value,
            n_lit,
            n_val,
        })
    }
}

// /// Macro to read a CSV file with a clock configuration into a usable format.
#[proc_macro]
// #[allow(non_snake_case)]
pub fn load_clock_config_csv(toks: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let path_lit = parse_macro_input!(toks as LitStr);
    let path_str = path_lit.value();

    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let file_path = std::path::Path::new(&manifest_dir).join(&path_str);

    let file_content = match fs::read_to_string(&file_path) {
        Ok(content) => content,
        Err(e) => {
            let error_msg = format!("Failed to read file '{}': {}", file_path.display(), e);
            return syn::Error::new(path_lit.span(), error_msg)
                .to_compile_error()
                .into();
        }
    };

    let mut preamble_initializers = Vec::new();
    let mut config_initializers = Vec::new();
    let mut postamble_initializers = Vec::new();
    let mut parser = ClockConfigParser::new();
    for line in file_content.lines() {
        if !parser.is_done() {
            let old_state = parser.state;
            match parser.parse_line(line) {
                Ok(Some(ConfigEntry {
                    page,
                    address,
                    data,
                })) => {
                    let init = quote! {
                        ::bittide_hal::manual_additions::si539x_spi::ConfigEntry {
                            page: #page,
                            address: #address,
                            data: #data,
                        }
                    };
                    match old_state {
                        ParserState::Preamble => preamble_initializers.push(init),
                        ParserState::Config => config_initializers.push(init),
                        ParserState::Postamble => postamble_initializers.push(init),
                        _ => {}
                    }
                }
                Ok(None) => {}
                Err(e) => {
                    let error_msg = format!("Error: {e:?}");
                    return syn::Error::new(path_lit.span(), error_msg)
                        .to_compile_error()
                        .into();
                }
            }
        }
    }

    let expanded = quote! {
        Config {
            preamble: [#(#preamble_initializers),*],
            config: [#(#config_initializers),*],
            postamble: [#(#postamble_initializers),*],
        }
    };

    expanded.into()
}
