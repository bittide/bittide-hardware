// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![allow(dead_code)]

use bittide_sys::elf_loading::validation::ElfConfig;
use object::{
    elf,
    write::elf::{FileHeader, ProgramHeader, Writer},
};
use proptest::{collection, prelude::*};
use rand::Fill;
use std::panic::AssertUnwindSafe;

type Addr = u64;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SegmentType {
    Text,
    Data,
    RoData,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Segment {
    pub addr: Addr,
    pub ty: SegmentType,
    pub data: Vec<u8>,
    pub zero_padding: u64,
    pub load: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ElfCreateInfo {
    pub segments: Vec<Segment>,
    pub entry: Addr,
    pub is_64: bool,
    pub endian: object::Endianness,
    pub machine: u16,
    pub elf_ty: u16,
}

pub fn elf_info_set_base_addr(info: &mut ElfCreateInfo, base_addr: u64) {
    for seg in &mut info.segments {
        seg.addr += base_addr;
    }
    info.entry += base_addr;
}

pub fn create_elf_file(info: &ElfCreateInfo) -> Vec<u8> {
    let mut buffer = vec![];
    let mut writer = Writer::new(info.endian, info.is_64, &mut buffer);
    writer.reserve_file_header();

    writer.reserve_program_headers(info.segments.len() as u32);

    let mut segment_offsets = Vec::with_capacity(info.segments.len());
    for seg in &info.segments {
        let offset = if seg.load {
            writer.reserve(seg.data.len(), 64)
        } else {
            0
        };

        segment_offsets.push(offset);
    }

    // Write file header

    let e_type = info.elf_ty;
    let e_machine = info.machine;
    let e_flags = 0;

    writer
        .write_file_header(&FileHeader {
            os_abi: elf::ELFOSABI_NONE,
            abi_version: 0,
            e_type,
            e_machine,
            e_entry: info.entry,
            e_flags,
        })
        .unwrap();

    // Write segments

    for (i, seg) in info.segments.iter().enumerate() {
        let p_type = if seg.load { elf::PT_LOAD } else { elf::PT_NULL };
        let p_flags = match &seg.ty {
            SegmentType::Text => elf::PF_X | elf::PF_R,
            SegmentType::Data => elf::PF_R | elf::PF_W,
            SegmentType::RoData => elf::PF_R,
        };

        writer.write_program_header(&ProgramHeader {
            p_type,
            p_flags,
            p_offset: segment_offsets[i] as u64,
            p_vaddr: seg.addr,
            p_paddr: seg.addr,
            p_filesz: if seg.load { seg.data.len() as u64 } else { 0 },
            p_memsz: seg.data.len() as u64 + seg.zero_padding,
            p_align: 64,
        });
    }

    for seg in &info.segments {
        if seg.load && !seg.data.is_empty() {
            writer.write_align(64);
            writer.write(&seg.data);
        }
    }

    assert_eq!(writer.reserved_len(), writer.len());

    buffer
}

/// Run a function with an allocated buffer within the 32bit address space.
///
/// To aid with testing, instead of returning a zeroed out buffer, the buffer
/// gets filled with random values.
///
/// # Safety
///
/// This function uses `mmap`, which might not work as intended on Windows or non-POSIX
/// platforms.
pub unsafe fn with_32bit_addr_buffer<R: 'static>(size: u32, f: impl FnOnce(&mut [u8]) -> R) -> R {
    // allocate a buffer within the 32bit address space

    let mem = {
        // SAFETY: The protection flags are valid flags and allow memory
        //         access (not PROT_NONE).
        //         The flags are valid flags, the memory is not shared and
        //         guaranteed to be at the specified address.
        //         The file descriptor is -1, as suggested to be most portable by
        //         https://linux.die.net/man/2/mmap.
        //         The offset is 0, as it is ignored due to MAP_ANONYMOUS.
        libc::mmap(
            std::ptr::null_mut(),
            size as usize,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_ANONYMOUS | libc::MAP_PRIVATE | libc::MAP_32BIT,
            -1,
            0,
        )
    };

    assert!(mem != libc::MAP_FAILED);

    assert!(!mem.is_null(), "mmap'ed pointer is not null");
    assert!(
        (mem as usize) < u32::MAX as usize,
        "mmap'ed pointer is within 32 bit address space"
    );
    assert!(
        (mem as usize + size as usize) < u32::MAX as usize,
        "mmap'ed pointer is within 32 bit address space"
    );
    assert!(
        mem as usize & 0x0FFF == 0,
        "mmap'ed pointer is aligned to 4K"
    );
    let mem_bytes = mem.cast::<u8>();

    let buf = {
        // SAFETY: `size` bytes at `mem_bytes` are initialised to 0
        //         (MAP_ANONYMOUS in mmap initialises memory to 0).
        //         The pointer is a valid pointer (no mmap error, not null).
        //         The pointer has an alignment valid for any reasonable type (4K).
        std::slice::from_raw_parts_mut(mem_bytes, size as usize)
    };

    buf.try_fill(&mut rand::thread_rng()).unwrap();

    let res = std::panic::catch_unwind(AssertUnwindSafe(|| f(buf)));

    // SAFETY: the pointer `mem` was previously allocated by mmap, `size` is the size of
    //         the full allocation.
    libc::munmap(mem, size as usize);

    match res {
        Ok(val) => val,
        Err(payload) => std::panic::resume_unwind(payload),
    }
}

pub fn elf_config_from_segs(segs: &[Segment]) -> ElfConfig {
    // The ranges are inversed so that they don't default to 0..MAX
    // but instead are properly read from the segments.
    // Since they are not used as iterators but instead as a pair, it does not
    // matter that they yield no values when iterated.
    #[allow(clippy::reversed_empty_ranges)]
    let mut config = ElfConfig {
        instruction_memory_address: usize::MAX..0,
        data_memory_address: usize::MAX..0,
    };

    for seg in segs {
        match seg.ty {
            SegmentType::Text => {
                config.instruction_memory_address.start = config
                    .instruction_memory_address
                    .start
                    .min(seg.addr as usize);
                config.instruction_memory_address.end = config
                    .instruction_memory_address
                    .end
                    .max(seg.addr as usize + seg.data.len() + seg.zero_padding as usize);
            }
            SegmentType::Data | SegmentType::RoData => {
                config.data_memory_address.start =
                    config.data_memory_address.start.min(seg.addr as usize);
                config.data_memory_address.end = config
                    .data_memory_address
                    .end
                    .max(seg.addr as usize + seg.data.len() + seg.zero_padding as usize);
            }
        }
    }

    config
}

pub fn elf_loaded_buffer_size(info: &ElfCreateInfo) -> usize {
    let config = elf_config_from_segs(&info.segments);

    let start = config
        .instruction_memory_address
        .start
        .min(config.data_memory_address.start);
    let end = config
        .instruction_memory_address
        .end
        .max(config.data_memory_address.end);

    end - start
}

//
//
// Proptest generators
//
//

/// A strategy to generate ELF segmtents. The addresses of the segments are
/// consecutive.
pub fn gen_segments(
    n_text: usize,
    n_data: usize,
    n_rodata: usize,
    text_can_be_empty: bool,
) -> impl Strategy<Value = Vec<Segment>> {
    let text_lower_bound = if text_can_be_empty { 0 } else { 1 };
    let texts = (0..n_text)
        .map(|_| {
            (
                collection::vec(any::<u8>(), text_lower_bound..1000),
                (0..1000u64),
            )
        })
        .collect::<Vec<_>>();

    let data = (0..n_data)
        .map(|_| (collection::vec(any::<u8>(), 0..1000), (0..1000u64)))
        .collect::<Vec<_>>();

    let rodata = (0..n_rodata)
        .map(|_| (collection::vec(any::<u8>(), 0..1000), (0..1000u64)))
        .collect::<Vec<_>>();

    (texts, data, rodata).prop_map(|(texts, data, rodata)| {
        let mut offset = 0;

        let mut v = vec![];
        for (d, padding) in texts {
            let data_len = d.len() as u64;
            v.push(Segment {
                addr: offset,
                ty: SegmentType::Text,
                zero_padding: padding,
                data: d,
                load: true,
            });

            offset += data_len + padding;
        }

        for (d, padding) in data {
            let data_len = d.len() as u64;
            v.push(Segment {
                addr: offset,
                ty: SegmentType::Data,
                zero_padding: padding,
                data: d,
                load: true,
            });

            offset += data_len + padding;
        }

        for (d, padding) in rodata {
            let data_len = d.len() as u64;
            v.push(Segment {
                addr: offset,
                ty: SegmentType::RoData,
                zero_padding: padding,
                data: d,
                load: true,
            });

            offset += data_len + padding;
        }

        v
    })
}

pub fn gen_entry_in_seg_ty(segs: Vec<Segment>, seg_ty: SegmentType) -> impl Strategy<Value = u64> {
    let text_segments = segs
        .into_iter()
        .filter(|seg| seg.ty == seg_ty)
        .collect::<Vec<_>>();

    // range 0..0 is invalid as a strategy, so a bounds check and special case
    // for the case the segments are empty are added.
    (0..text_segments.len().max(1)).prop_flat_map(move |i| {
        if let Some(seg) = text_segments.get(i) {
            seg.addr..(seg.addr + seg.data.len().max(1) as u64)
        } else {
            0..1
        }
    })
}

pub fn gen_valid_elf_file() -> impl Strategy<Value = ElfCreateInfo> {
    let segments = (0..10usize, 0..10usize)
        .prop_flat_map(|(n_data, n_rodata)| gen_segments(1, n_data, n_rodata, false));

    // generate entry in text segment
    let segments_entry = segments.prop_flat_map(|segs| {
        let entry = gen_entry_in_seg_ty(segs.clone(), SegmentType::Text);
        (Just(segs), entry)
    });

    segments_entry.prop_map(|(segs, entry)| ElfCreateInfo {
        elf_ty: object::elf::ET_EXEC,
        endian: object::Endianness::Little,
        is_64: false,
        machine: object::elf::EM_RISCV,
        segments: segs,
        entry: entry as u64,
    })
}
