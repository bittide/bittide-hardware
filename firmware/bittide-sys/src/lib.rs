// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]

pub mod elf_loading;
pub mod gather_unit;
pub mod scatter_unit;

pub struct Components<const FRAME_SIZE: usize> {
    pub gather_unit: gather_unit::GatherUnit<FRAME_SIZE>,
    pub scatter_unit: scatter_unit::ScatterUnit<FRAME_SIZE>,
}

pub const FDT_ADDR: *const u8 = 0x1000_0000 as *const u8;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum InitialisationError {
    FdtError(fdt::FdtError),
    ScatterUnitLoadError(FdtLoadError),
    GatherUnitLoadError(FdtLoadError),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FdtLoadError {
    FdtNodeNotFound(&'static str),
    RegNotFound {
        component: &'static str,
    },
    SizeMismatch {
        property: &'static str,
        expected: usize,
        found: usize,
    },
}

/// Initialise bittide components based off an FDT loaded from a known
/// memory-mapped address.
///
/// # Safety
///
/// The FDT must be accessible at the [`FDT_ADDR`] address and must appropriately
/// describe the hardware configuration that this code is being executed on.
pub unsafe fn initialise<const FRAME_SIZE: usize>(
) -> Result<Components<FRAME_SIZE>, InitialisationError> {
    let fdt = fdt::Fdt::from_ptr(FDT_ADDR).map_err(InitialisationError::FdtError)?;
    initialise_from_fdt(&fdt)
}

/// Initialise bittide components based off an FDT
///
/// # Safety
///
/// The FDT must appropriately describe the hardware configuration that this
/// code is being executed on.
pub unsafe fn initialise_from_fdt<const FRAME_SIZE: usize>(
    fdt: &fdt::Fdt,
) -> Result<Components<FRAME_SIZE>, InitialisationError> {
    let gu =
        gather_unit::GatherUnit::from_fdt(fdt).map_err(InitialisationError::GatherUnitLoadError)?;
    let su = scatter_unit::ScatterUnit::from_fdt(fdt)
        .map_err(InitialisationError::ScatterUnitLoadError)?;

    Ok(Components {
        gather_unit: gu,
        scatter_unit: su,
    })
}
