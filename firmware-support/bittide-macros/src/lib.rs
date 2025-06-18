// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use quote::quote;
use syn::{LitInt, Token, parse::Parse, parse_macro_input};

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

    fn next_pow2(n: &u128) -> u128 {
        n.next_power_of_two().max(8)
    }

    fn clog2(n: &u128) -> u128 {
        n.ilog2().max(8) as u128
    }

    let n_val = match n_lit.base10_parse() {
        Err(err) => return err.into_compile_error().into(),
        Ok(v) => v,
    };

    let ty = {
        let type_name = format!("u{}", next_pow2(&clog2(&n_val)));
        syn::Ident::new(&type_name, n_lit.span())
    };

    quote! {
        bittide_hal::manual_additions::index::IndexTy<#n_lit, #ty>
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

    fn next_pow2(n: &u128) -> u128 {
        n.next_power_of_two().max(8)
    }

    fn clog2(n: &u128) -> u128 {
        n.ilog2().max(8) as u128
    }

    let ty = {
        let type_name = format!("u{}", next_pow2(&clog2(&args.n_val)));
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
                        bittide_hal::manual_additions::index::IndexTy::<#n_lit, #ty>::new_unchecked(#int_lit)
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
            bittide_hal::manual_additions::index::IndexTy::<#n_lit, #ty>::new(#val).unwrap()
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
