// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]

#[cfg(feature = "debug-printing")]
pub mod character_device;
#[cfg(feature = "debug-printing")]
pub mod panic_handler;

pub const FDT_ADDR: *const u8 = 0x1000_0000 as *const u8;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum InitialisationError {
    FdtError(fdt::FdtError),
    CharacterDeviceDescIncomplete,
}

/// Initialise contranomy components based off an FDT loaded from a known
/// memory-mapped address.
///
/// # Safety
///
/// The FDT must be accessible at the [`FDT_ADDR`] address and must appropriately
/// describe the hardware configuration that this code is being executed on.
pub unsafe fn initialise() -> Result<(), InitialisationError> {
    let fdt = fdt::Fdt::from_ptr(FDT_ADDR).map_err(InitialisationError::FdtError)?;
    initialise_from_fdt(&fdt)
}

/// Initialise contranomy components based off an FDT
///
/// # Safety
///
/// The FDT must appropriately describe the hardware configuration that this
/// code is being executed on.
pub unsafe fn initialise_from_fdt(fdt: &fdt::Fdt) -> Result<(), InitialisationError> {
    // character device
    if let Some(char_device) = fdt.find_node("/character-device") {
        let addr = char_device
            .reg()
            .ok_or(InitialisationError::CharacterDeviceDescIncomplete)?
            .next()
            .ok_or(InitialisationError::CharacterDeviceDescIncomplete)?
            .starting_address as *mut u8;
        character_device::initialise(addr);
    }

    Ok(())
}
