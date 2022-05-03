// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

pub mod validation;

use elf_rs::{ElfFile, ProgramType};
use validation::ValidatedElfFile;

/// Load a validated ELF file into memory
///
/// # Safety
///
/// The segments of the ELF file will be written into their specified physical
/// addresses. Calling this function is only safe if those addresses:
///
/// - point to valid memory
/// - point to memory that is not used by the current program
pub unsafe fn load_elf_file(valid_elf: &ValidatedElfFile<'_>) {
    let elf = valid_elf.elf_file();
    for p in elf.program_header_iter() {
        // skip segments that don't need loading
        if p.ph_type() != ProgramType::LOAD {
            continue;
        }

        let paddr = usize::try_from(p.paddr()).unwrap();
        let data = p.content();

        let addr_ptr = paddr as *mut u8;

        // write the segment to its desired memory location
        core::ptr::copy_nonoverlapping(data.as_ptr(), addr_ptr, data.len());

        // the size in memory can be bigger than the data provided in the ELF file,
        // the remaining bytes should be filled with zeroes.
        let zero_padding = (p.memsz() as usize).saturating_sub(data.len());
        if zero_padding > 0 {
            // SAFETY: - `addr_ptr.add(data.len())` is valid since it points at
            //           most one byte after a valid allocation
            //         - the call to `write_bytes` is valid as the memory/buffer
            //           size was validated to be big enough to hold the full
            //           data + padding.
            core::ptr::write_bytes(addr_ptr.add(data.len()), 0, zero_padding);
        }
    }
}

/// Creates a function pointer to the entry point of a validated ELF file.
///
/// # Safety
///
/// This function transmutes the entry point given by the ELF file into a
/// function pointer.
/// This is only allowed if the ELF file was previously loaded into memory using
/// [`load_elf_file`].
pub unsafe fn entry_point_fn(valid_elf: &ValidatedElfFile<'_>) -> unsafe extern "C" fn() -> ! {
    let elf = valid_elf.elf_file();
    // the ELF entry point is a physical address, so no conversion needed
    let entry = usize::try_from(elf.entry_point()).unwrap();
    core::mem::transmute::<_, unsafe extern "C" fn() -> !>(entry)
}
