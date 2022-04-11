use bittide_sys::elf_loading::validation::{self, ElfConfig, ElfValidationError};
use elf_common::{
    create_elf_file, elf_config_from_segs, gen_entry_in_seg_ty, gen_segments, gen_valid_elf_file,
    ElfCreateInfo,
};
use object::elf;
use proptest::prelude::*;
use test_strategy::proptest;

use crate::elf_common::{Segment, SegmentType};

mod elf_common;

#[test]
fn example_elf_file_no_segments() {
    let config = ElfConfig {
        instruction_memory_address: 0..1000,
        data_memory_address: 1000..2000,
    };

    let elf = create_elf_file(&ElfCreateInfo {
        // entry points outside of text segments
        entry: 0,
        segments: vec![],
        is_64: false,
        endian: object::Endianness::Little,
        machine: object::elf::EM_RISCV,
        elf_ty: object::elf::ET_EXEC,
    });

    let res = validation::validate_elf_file(&elf, &config);
    assert!(res.is_err());
    assert_eq!(
        res.unwrap_err(),
        ElfValidationError::EntryPointNotLoaded { entry_point: 0 }
    );
}

proptest::proptest! {
    #[test]
    fn all_valid_elf_files_validate(info in gen_valid_elf_file()) {
        let config = elf_config_from_segs(&info.segments);
        let bin = create_elf_file(&info);
        let res = validation::validate_elf_file(
            &bin,
            &config,
        );

        assert!(res.is_ok());
    }

    #[test]
    fn all_elf64_files_invalid(mut info in gen_valid_elf_file()) {
        let config = elf_config_from_segs(&info.segments);

        info.is_64 = true;

        let bin = create_elf_file(&info);
        let res = validation::validate_elf_file(
            &bin,
            &config,
        );

        assert!(res.is_err());
        assert_eq!(res.unwrap_err(), ElfValidationError::Elf64Found);
    }
}

#[proptest]
fn all_non_riscv_elfs_invalid(
    #[strategy(0usize..10)] _n_texts: usize,
    #[strategy(0usize..10)] _n_datas: usize,
    #[strategy(0usize..10)] _n_rodatas: usize,

    #[strategy(gen_segments(#_n_texts, #_n_datas, #_n_rodatas, true))] segs: Vec<Segment>,

    #[strategy(gen_entry_in_seg_ty(#segs.clone(), SegmentType::Text))] entry: u64,

    #[filter(#machine != elf::EM_RISCV)] machine: u16,
) {
    let config = elf_config_from_segs(&segs);

    let info = ElfCreateInfo {
        segments: segs,
        entry,
        is_64: false,
        endian: object::Endianness::Little,
        machine,
        elf_ty: elf::ET_EXEC,
    };

    let bin = create_elf_file(&info);
    let res = validation::validate_elf_file(&bin, &config);
    assert!(res.is_err());
    assert_eq!(res.unwrap_err(), ElfValidationError::MachineNotRiscv);
}

#[proptest]
fn all_non_le_elfs_invalid(#[strategy(gen_valid_elf_file())] mut info: ElfCreateInfo) {
    let config = elf_config_from_segs(&info.segments);

    info.endian = object::Endianness::Big;

    let bin = create_elf_file(&info);
    let res = validation::validate_elf_file(&bin, &config);
    assert!(res.is_err());
    assert_eq!(res.unwrap_err(), ElfValidationError::UnexpectedEndianess);
}

#[proptest]
fn all_non_text_entry_invalid(
    #[strategy(0usize..10)] _n_texts: usize,
    #[strategy(1usize..10)] _n_datas: usize,
    #[strategy(1usize..10)] _n_rodatas: usize,

    #[strategy(gen_segments(#_n_texts, #_n_datas, #_n_rodatas, true))] segs: Vec<Segment>,

    #[strategy(prop_oneof![Just(SegmentType::Data), Just(SegmentType::RoData)])]
    _seg_type: SegmentType,

    #[strategy(gen_entry_in_seg_ty(#segs.clone(), #_seg_type))] entry: u64,
) {
    let config = elf_config_from_segs(&segs);

    let info = ElfCreateInfo {
        segments: segs,
        entry,
        is_64: false,
        endian: object::Endianness::Little,
        machine: elf::EM_RISCV,
        elf_ty: elf::ET_EXEC,
    };

    let bin = create_elf_file(&info);
    let res = validation::validate_elf_file(&bin, &config);
    assert!(res.is_err());
    assert_eq!(
        res.unwrap_err(),
        ElfValidationError::EntryPointNotLoaded {
            entry_point: entry as usize
        }
    );
}
