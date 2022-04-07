use bittide_sys::elf_loading::{
    load_elf_file,
    validation::{self, ElfConfig},
};
use elf_common::{create_elf_file, ElfCreateInfo};

use crate::elf_common::{
    elf_config_from_segs, elf_info_set_base_addr, elf_loaded_buffer_size, gen_valid_elf_file,
    with_32bit_addr_buffer, Segment, SegmentType,
};
use proptest::prelude::*;
use test_strategy::proptest;

mod elf_common;

#[test]
fn load_elf_file_with_valid_segments() {
    unsafe {
        with_32bit_addr_buffer(300, |buf| {
            let buf_addr = buf.as_mut_ptr() as usize;
            let buf_addr_u64 = buf_addr as u64;

            let config = ElfConfig {
                instruction_memory_address: buf_addr..(buf_addr + 100),
                data_memory_address: (buf_addr + 100)..(buf_addr + 300),
            };

            let elf = create_elf_file(&ElfCreateInfo {
                entry: buf_addr_u64,
                segments: vec![
                    Segment {
                        addr: buf_addr_u64,
                        load: true,
                        data: vec![1, 2, 3, 4],
                        ty: SegmentType::Text,
                    },
                    Segment {
                        addr: buf_addr_u64 + 100,
                        load: true,
                        data: vec![1, 2, 3, 4],
                        ty: SegmentType::Data,
                    },
                    Segment {
                        addr: buf_addr_u64 + 150,
                        load: true,
                        data: vec![1, 2, 3, 4],
                        ty: SegmentType::RoData,
                    },
                ],
                is_64: false,
                endian: object::Endianness::Little,
                machine: object::elf::EM_RISCV,
                elf_ty: object::elf::ET_EXEC,
            });

            let tok = validation::validate_elf_file(&elf, &config).unwrap();

            load_elf_file(&tok);

            assert_eq!(&buf[0..4], [1, 2, 3, 4]);
            assert_eq!(&buf[100..104], [1, 2, 3, 4]);
            assert_eq!(&buf[150..154], [1, 2, 3, 4]);
        })
    }
}

#[proptest(ProptestConfig { cases: 1000, ..ProptestConfig::default() })]
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
                assert_eq!(&buf[start..end], &seg.data);
            }
        });
    }
}
