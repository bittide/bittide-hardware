// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

mod clock_config;

use clock_config::{ClockConfigParser, ConfigEntry, ParserState};
use quote::quote;
use std::fs;
use syn::{LitStr, parse_macro_input};

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
