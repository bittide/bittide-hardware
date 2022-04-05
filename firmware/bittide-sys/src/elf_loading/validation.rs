use elf_rs::{Elf32, ElfFile, ProgramHeaderFlags, ProgramType};

pub struct ValidatedElfFile<'a>(&'a Elf32<'a>);

impl<'a> ValidatedElfFile<'a> {
    pub fn elf_file(&self) -> &'a Elf32<'a> {
        self.0
    }
}

pub struct ElfConfig {
    pub instruction_memory_address: core::ops::Range<usize>,
    pub data_memory_address: core::ops::Range<usize>,
}

pub enum ElfValidationError {
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
    elf: &'a Elf32<'a>,
    config: &ElfConfig,
) -> Result<ValidatedElfFile<'a>, ElfValidationError> {
    let entry_point = usize::try_from(elf.entry_point()).unwrap();
    let mut entry_point_loaded = false;

    for p in elf.program_header_iter() {
        if p.ph_type() != ProgramType::LOAD {
            continue;
        }

        let addr = usize::try_from(p.paddr()).unwrap();
        let size = usize::try_from(p.memsz()).unwrap();
        let found_range = addr..(addr + size);

        if found_range.contains(&entry_point) {
            entry_point_loaded = true;
        }

        if p.flags() == ProgramHeaderFlags::EXECUTE | ProgramHeaderFlags::READ {
            // instruction memory

            if config.instruction_memory_address.contains(&addr) {
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

            if config.data_memory_address.contains(&addr) {
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
