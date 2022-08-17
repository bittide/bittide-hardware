// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_sys::elf_loading::{load_elf_file, validation};
use elf_common::{create_elf_file, ElfCreateInfo};

use crate::elf_common::{
    elf_config_from_segs, elf_info_set_base_addr, elf_loaded_buffer_size, gen_valid_elf_file,
    with_32bit_addr_buffer,
};
use proptest::prelude::*;
use test_strategy::proptest;

mod elf_common;

// Generate valid ELF files and make sure segments are loaded into the buffer
// properly.
#[proptest(ProptestConfig { cases: 100, max_shrink_iters: 100000, ..ProptestConfig::default() })]
fn all_loaded_sections_copied(#[strategy(gen_valid_elf_file())] mut info: ElfCreateInfo) {
    let buffer_size = elf_loaded_buffer_size(&info);
    unsafe {
        with_32bit_addr_buffer(u32::try_from(buffer_size).unwrap(), |buf| {
            let base_addr = buf.as_mut_ptr() as usize as u64;

            elf_info_set_base_addr(&mut info, base_addr);
            let config = elf_config_from_segs(&info.segments);

            let elf = create_elf_file(&info);

            let tok = validation::validate_elf_file(&elf, &config).unwrap();

            load_elf_file(&tok);

            // check if contents of the ELF file have been written to the buffer
            for seg in &info.segments {
                let start = (seg.addr - base_addr) as usize;
                let end = start as usize + seg.data.len();
                let padding_end = end + seg.zero_padding as usize;
                assert_eq!(&buf[start..end], &seg.data);
                assert!(buf[end..padding_end].iter().all(|b| *b == 0));
            }
        });
    }
}
