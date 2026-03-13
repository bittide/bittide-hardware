// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

mod clock_config;

use clock_config::{ClockConfigParser, ConfigEntry, ParserState};
use quote::quote;
use std::{fmt::Write, fs};
use syn::{LitInt, LitStr, Token, parse::Parse, parse_macro_input};

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

    fn get_correct_size(n: u128) -> u32 {
        let clog2 = (u128::BITS - (n - 1).leading_zeros()).next_power_of_two();
        clog2.max(8)
    }

    let n_val = match n_lit.base10_parse() {
        Err(err) => return err.into_compile_error().into(),
        Ok(v) => v,
    };

    let ty = {
        let type_name = format!("u{}", get_correct_size(n_val));
        syn::Ident::new(&type_name, n_lit.span())
    };

    quote! {
        Index<#n_lit, #ty>
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

    fn get_correct_size(n: u128) -> u32 {
        let clog2 = (u128::BITS - (n - 1).leading_zeros()).next_power_of_two();
        clog2.max(8)
    }

    let ty = {
        let type_name = format!("u{}", get_correct_size(args.n_val));
        syn::Ident::new(&type_name, n_lit.span())
    };

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
                        Index::<#n_lit, #ty>::new_unchecked(
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
        _ => quote! {
            Index::<#n_lit, #ty>::new(#val).unwrap()
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

    fn get_correct_size(n: u8) -> u8 {
        (n.div_ceil(8).next_power_of_two() * 8).max(8)
    }

    let n_val = match n_lit.base10_parse() {
        Err(err) => return err.into_compile_error().into(),
        Ok(v) => v,
    };

    let ty = {
        let type_name = format!("i{}", get_correct_size(n_val));
        syn::Ident::new(&type_name, n_lit.span())
    };

    quote! {
        crate::manual_additions::signed::Signed<#n_lit, #ty>
    }
    .into()
}

#[proc_macro]
pub fn signed(toks: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(toks as NumericArgs);

    let val = args.value;
    let nlit = args.n_lit;
    let nval = args.n_val;

    fn get_correct_size(n: u8) -> u8 {
        (n.div_ceil(8).next_power_of_two() * 8).max(8)
    }

    let ty = {
        let type_name = format!("i{}", get_correct_size(nval));
        syn::Ident::new(&type_name, nlit.span())
    };

    match val {
        // try to detect a constant literal to skip the bounds check
        syn::Expr::Lit(syn::ExprLit {
            attrs: _,
            lit: syn::Lit::Int(int_lit),
        }) => {
            let litval: i128 = match int_lit.base10_parse() {
                Err(err) => return err.into_compile_error().into(),
                Ok(v) => v,
            };
            // let neg_bound = 1 << (nval - 1);
            let neg_bound = !0 << (nval - 1);
            let pos_bound = !(!0 << (nval - 1));
            if (neg_bound..=pos_bound).contains(&litval) {
                quote! {
                    unsafe {
                        Signed::<#nlit, #ty>::new_unchecked(
                            #int_lit
                        )
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
        _ => quote! {
            Signed::<#nlit, #ty>::new(#val).expect(concat!(
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
        .into(),
    }
}

#[proc_macro]
#[allow(non_snake_case)]
pub fn Unsigned(toks: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let n_lit = parse_macro_input!(toks as LitInt);

    fn get_correct_size(n: u8) -> u8 {
        (n.div_ceil(8).next_power_of_two() * 8).max(8)
    }

    let n_val = match n_lit.base10_parse() {
        Err(err) => return err.into_compile_error().into(),
        Ok(v) => v,
    };

    let ty = {
        let type_name = format!("u{}", get_correct_size(n_val));
        syn::Ident::new(&type_name, n_lit.span())
    };

    quote! {
        Unsigned<#n_lit, #ty>
    }
    .into()
}

#[proc_macro]
pub fn unsigned(toks: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(toks as NumericArgs);

    let val = args.value;
    let nlit = args.n_lit;
    let nval = args.n_val;

    fn get_correct_size(n: u8) -> u8 {
        (n.div_ceil(8).next_power_of_two() * 8).max(8)
    }

    let ty = {
        let type_name = format!("u{}", get_correct_size(nval));
        syn::Ident::new(&type_name, nlit.span())
    };

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
                        Unsigned::<#nlit, #ty>::new_unchecked(
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
        _ => quote! {
            Unsigned::<#nlit, #ty>::new(#val).expect(concat!(
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

    quote! {
        BitVector<#n_val, #size>
    }
    .into()
}

#[proc_macro]
pub fn bitvector(toks: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let BitVectorArgs {
        value,
        n_lit,
        n_val,
    } = parse_macro_input!(toks as BitVectorArgs);

    let size = n_val.div_ceil(8);

    match value {
        syn::Expr::Lit(syn::ExprLit {
            attrs: _,
            lit: syn::Lit::Int(input),
        }) => {
            let input_str = input.to_string();
            if !(input_str.starts_with("0x") || input_str.starts_with("0X"))
                || input_str
                    .chars()
                    .skip(2)
                    .any(|c| !(c.is_ascii_hexdigit() || c == ' ' || c == '_'))
            {
                return syn::Error::new(
                    input.span(),
                    concat!(
                        "Expected a hexadecimal literal! Must start with `0x` or `0X` and contain ",
                        "only hexdigits, spaces, and underscores",
                    )
                    .to_string(),
                )
                .into_compile_error()
                .into();
            }
            let input_str = input_str
                .trim_start_matches('0')
                .trim_start_matches(['x', 'X']);
            let mut buf: Vec<u8> = vec![0; size];
            let mut char_iter = input_str.chars().filter(char::is_ascii_hexdigit).rev();
            let mut lower = true;
            let mut idx = 0;
            loop {
                let Some(c) = char_iter.next() else {
                    break;
                };
                let val = match c {
                    '0'..='9' => c as u8 - b'0',
                    'A'..='F' => c as u8 - b'A' + 10,
                    'a'..='f' => c as u8 - b'a' + 10,
                    _ => unreachable!(),
                };
                match (buf.get_mut(idx), lower) {
                    (None, _) => {
                        let rem = char_iter.filter(|c| c.is_ascii_hexdigit()).count();
                        return syn::Error::new(
                            input.span(),
                            format!(
                                "Literal too long! Expected 0..={size} hexdigits, but {rem} left"
                            ),
                        )
                        .into_compile_error()
                        .into();
                    }
                    (Some(byte), true) => *byte |= val,
                    (Some(byte), false) => *byte |= val << 4,
                }
                if !lower {
                    idx += 1;
                }
                lower = !lower;
            }
            let check_bit = n_val % 8;
            if check_bit == 0 || buf.last().unwrap().leading_zeros() as usize >= (8 - check_bit) {
                let mut array_str = String::with_capacity(buf.len() * 6);
                array_str.push('[');
                for byte in buf {
                    write!(array_str, "0x{byte:02x}, ").unwrap();
                }
                array_str.pop();
                array_str.pop();
                array_str.push(']');
                let toks: proc_macro2::TokenStream = array_str.parse().unwrap();
                quote! {
                    unsafe {
                        BitVector::<#n_lit, #size>::new_unchecked(
                            #toks
                        )
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
        _ => quote! {
            BitVector::<#n_lit, #size>::new(#value).unwrap()
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
