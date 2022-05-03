// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use elf_rs::{Elf, Elf32, ElfFile, ProgramHeaderFlags, ProgramType};

#[derive(Debug)]
pub struct ValidatedElfFile<'a>(Elf32<'a>);

impl<'a> ValidatedElfFile<'a> {
    pub fn elf_file(&self) -> &Elf32<'a> {
        &self.0
    }
}

#[derive(Debug)]
pub struct ElfConfig {
    pub instruction_memory_address: core::ops::Range<usize>,
    pub data_memory_address: core::ops::Range<usize>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ElfValidationError {
    MachineNotRiscv,
    ElfParsingError(elf_rs::Error),
    Elf64Found,
    UnexpectedEndianess,
    UnexpectedElfType {
        found: elf_rs::ElfType,
    },
    SegmentOutOfRange {
        expected: core::ops::Range<usize>,
        found: core::ops::Range<usize>,
        segment_type: &'static str,
    },
    EntryPointNotLoaded {
        entry_point: usize,
    },
}

pub fn validate_elf_file<'a>(
    elf_bin: &'a [u8],
    config: &ElfConfig,
) -> Result<ValidatedElfFile<'a>, ElfValidationError> {
    // `elf_rs` reads numbers in native endian, so it reports the wrong
    // size for the header in case the native endianess and the endianess of the
    // ELF file are mismatched. This wrong size leads to an error in calculating
    // the expected buffer size and can cause the loading of the ELF header to
    // fail.
    // With the ELF header not being loaded, the endianess needs to be read from
    // the buffer before attempting to read the ELF header.
    // The endianess is written at offset 0x05 of the header, so
    // it gets read in manually here to make sure an endianess-mismatch
    // can be reported.
    match elf_bin.get(0x05) {
        Some(1) => {}
        Some(_) => return Err(ElfValidationError::UnexpectedEndianess),
        None => {
            return Err(ElfValidationError::ElfParsingError(
                elf_rs::Error::BufferTooShort,
            ))
        }
    }

    let elf = match Elf::from_bytes(elf_bin) {
        Ok(Elf::Elf32(elf_32)) => elf_32,
        Ok(Elf::Elf64(_)) => return Err(ElfValidationError::Elf64Found),
        Err(err) => return Err(ElfValidationError::ElfParsingError(err)),
    };

    if elf.elf_header().machine() != elf_rs::ElfMachine::RISC_V {
        return Err(ElfValidationError::MachineNotRiscv);
    }

    if elf.elf_header().elftype() != elf_rs::ElfType::ET_EXEC {
        return Err(ElfValidationError::UnexpectedElfType {
            found: elf.elf_header().elftype(),
        });
    }

    let entry_point = usize::try_from(elf.entry_point()).unwrap();
    let mut entry_point_loaded = false;

    for p in elf.program_header_iter() {
        if p.ph_type() != ProgramType::LOAD {
            continue;
        }

        let addr = usize::try_from(p.paddr()).unwrap();
        let size = usize::try_from(p.memsz()).unwrap();
        let found_range = addr..(addr + size);

        if p.flags() == ProgramHeaderFlags::EXECUTE | ProgramHeaderFlags::READ {
            // instruction memory

            if found_range.contains(&entry_point) {
                entry_point_loaded = true;
            }

            if found_range.is_empty()
                || config.instruction_memory_address.is_empty()
                || config.instruction_memory_address.contains(&addr)
            {
                continue;
            } else {
                return Err(ElfValidationError::SegmentOutOfRange {
                    expected: config.instruction_memory_address.clone(),
                    found: found_range,
                    segment_type: "instruction memory",
                });
            }
        } else if p.flags() == ProgramHeaderFlags::READ
            || p.flags() == ProgramHeaderFlags::WRITE | ProgramHeaderFlags::READ
        {
            // data memory

            if found_range.is_empty()
                || config.data_memory_address.is_empty()
                || config.data_memory_address.contains(&addr)
            {
                continue;
            } else {
                return Err(ElfValidationError::SegmentOutOfRange {
                    expected: config.data_memory_address.clone(),
                    found: found_range,
                    segment_type: "data memory",
                });
            }
        }
    }

    if !entry_point_loaded {
        return Err(ElfValidationError::EntryPointNotLoaded { entry_point });
    }

    Ok(ValidatedElfFile(elf))
}
