// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

pub mod generators;
pub mod parse;

pub use crate::generators::{
    generate_rust_wrappers, DebugDerive, GenerateConfig, ItemScopeMode, RustWrappers,
};
pub use crate::parse::{parse, MemoryMapDesc};

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_generators() {
        let mm = parse(TEST_SRC).unwrap();
        let config = GenerateConfig {
            debug_derive_mode: DebugDerive::Ufmt,
            item_scope_mode: ItemScopeMode::OneFile,
        };
        let wrappers = generate_rust_wrappers(&mm, &config);

        for (dev_name, dev_src) in &wrappers.device_defs {
            // println!("==========");
            // println!("{dev_name}.rs");
            // println!("==========");
            // println!();

            println!("{}", dev_src.to_string());
        }

        for (type_name, ty_src) in &wrappers.type_defs {
            // println!("==========");
            // println!("{type_name}.rs");
            // println!("==========");
            // println!();

            println!("{}", ty_src.to_string());
        }
    }

    const TEST_SRC: &'static str = r#""#;
}
