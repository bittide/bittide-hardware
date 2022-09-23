// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]

use fdt::Fdt;
use management::{rx_unit, tx_unit};

pub mod gppe;
pub mod management;

pub mod elf_loading;

#[cfg(target_arch = "riscv32")]
pub mod panic_handler;

pub(crate) mod utils;

use crate::utils::FdtNodeExt;

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
    ) -> Result<gppe::scatter_unit::ScatterUnit<FRAME_SIZE>, ComponentLoadError> {
        let node = self.fdt.find_node("/").unwrap().get_node(name)?;

        gppe::scatter_unit::ScatterUnit::from_fdt_node(&node)
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
    ) -> Result<gppe::gather_unit::GatherUnit<FRAME_SIZE>, ComponentLoadError> {
        let node = self.fdt.find_node("/").unwrap().get_node(name)?;

        gppe::gather_unit::GatherUnit::from_fdt_node(&node)
    }

    /// Initialise an rx-Unit component.
    ///
    /// # Safety
    ///
    /// The `name` must correspond to a node in the device tree,
    /// the contents of this node must all be valid for the configuration of the
    /// hardware this function gets executed on.
    pub unsafe fn initialise_rx_unit(
        &self,
        name: &'static str,
    ) -> Result<rx_unit::TimingOracle, ComponentLoadError> {
        let node = self.fdt.find_node("/").unwrap().get_node(name)?;

        rx_unit::TimingOracle::from_fdt_node(&node)
    }

    /// Initialise a tx-Unit component.
    ///
    /// # Safety
    ///
    /// The `name` must correspond to a node in the device tree,
    /// the contents of this node must all be valid for the configuration of the
    /// hardware this function gets executed on.
    pub unsafe fn initialise_tx_unit(
        &self,
        name: &'static str,
    ) -> Result<tx_unit::TimingOracle, ComponentLoadError> {
        let node = self.fdt.find_node("/").unwrap().get_node(name)?;

        tx_unit::TimingOracle::from_fdt_node(&node)
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
        let node = self.fdt.find_node("/").unwrap().get_node(name)?;

        let addr = node.get_reg("character device")?.starting_address;

        gppe::character_device::initialise(addr as *mut _);

        Ok(())
    }
}
