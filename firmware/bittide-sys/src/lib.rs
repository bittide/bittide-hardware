// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]

use fdt::Fdt;
use utils::matches_fdt_name;

pub mod character_device;
pub mod elf_loading;
pub mod gather_unit;
pub mod scatter_unit;
pub mod uart;

#[cfg(target_arch = "riscv32")]
pub mod panic_handler;

pub(crate) mod utils;

pub struct Initialiser<'a> {
    fdt: Fdt<'a>,
}

/// Address to load the FDT from.
pub const FDT_ADDR: *const u8 = 0x1000_0000 as *const u8;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComponentLoadError {
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

impl<'a> Initialiser<'a> {
    /// Create a new [`Initialiser`] from a memory-mapped FDT.
    ///
    /// # Safety
    ///
    /// This function expected a flattened-device-tree to be located at
    /// [`FDT_ADDR`].
    pub unsafe fn new() -> Result<Initialiser<'static>, fdt::FdtError> {
        let fdt = fdt::Fdt::from_ptr(FDT_ADDR)?;
        Ok(Initialiser { fdt })
    }

    /// Create a new [`Initialiser`] from the contents of a flattened-device-tree.
    pub fn from_bytes(bytes: &'a [u8]) -> Result<Initialiser<'a>, fdt::FdtError> {
        let fdt = fdt::Fdt::new(bytes)?;
        Ok(Initialiser { fdt })
    }

    /// Initialise a Scatter-Unit component.
    ///
    /// # Safety
    ///
    /// The `name` must correspond to a node in the device tree,
    /// the contents of this node must all be valid for the configuration of the
    /// hardware this function gets executed on.
    pub unsafe fn initialise_scatter_unit<const FRAME_SIZE: usize>(
        &self,
        name: &'static str,
    ) -> Result<scatter_unit::ScatterUnit<FRAME_SIZE>, ComponentLoadError> {
        let node = self
            .fdt
            .find_node("/")
            .unwrap()
            .children()
            .find(|child| matches_fdt_name(child, name))
            .ok_or(ComponentLoadError::FdtNodeNotFound(name))?;

        scatter_unit::ScatterUnit::from_fdt_node(&node)
    }

    /// Initialise a Gather-Unit component.
    ///
    /// # Safety
    ///
    /// The `name` must correspond to a node in the device tree,
    /// the contents of this node must all be valid for the configuration of the
    /// hardware this function gets executed on.
    pub unsafe fn initialise_gather_unit<const FRAME_SIZE: usize>(
        &self,
        name: &'static str,
    ) -> Result<gather_unit::GatherUnit<FRAME_SIZE>, ComponentLoadError> {
        let node = self
            .fdt
            .find_node("/")
            .unwrap()
            .children()
            .find(|child| matches_fdt_name(child, name))
            .ok_or(ComponentLoadError::FdtNodeNotFound(name))?;

        gather_unit::GatherUnit::from_fdt_node(&node)
    }

    /// Initialise a character-device (debug printing) component.
    ///
    /// # Safety
    ///
    /// The `name` must correspond to a node in the device tree,
    /// the contents of this node must all be valid for the configuration of the
    /// hardware this function gets executed on.
    pub unsafe fn initialise_character_device(
        &self,
        name: &'static str,
    ) -> Result<(), ComponentLoadError> {
        let node = self
            .fdt
            .find_node("/")
            .unwrap()
            .children()
            .find(|child| matches_fdt_name(child, name))
            .ok_or(ComponentLoadError::FdtNodeNotFound(name))?;

        let addr = node
            .reg()
            .ok_or(ComponentLoadError::RegNotFound {
                component: "character device",
            })?
            .next()
            .ok_or(ComponentLoadError::RegNotFound {
                component: "character device",
            })?
            .starting_address;

        character_device::initialise(addr as *mut _);

        Ok(())
    }
}
