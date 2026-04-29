// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Protocol types for inter-node communication over [`LinkInterface`](super::link_interface::LinkInterface).
//!
//! Each protocol type has an ergonomic domain representation (e.g. [`UgnEdge`])
//! and a byte-stable wire representation (e.g. [`UgnEdgeWire`]) that derives
//! zerocopy traits. The [`WireEncode`] and [`WireDecode`] traits bridge the
//! two so that [`LinkInterface`](super::link_interface::LinkInterface) can
//! accept any domain type directly.
//!
//! To add a new protocol type:
//! 1. Define the domain type with native Rust fields.
//! 2. Define a `#[repr(C)]` wire type deriving `FromBytes`, `FromZeroes`,
//!    `AsBytes`, and `Unaligned`.
//! 3. Implement [`WireEncode`] and [`WireDecode`] to convert between them.

use log::warn;
use zerocopy::byteorder::{I64, LE, U32};
use zerocopy::{AsBytes, FromBytes, FromZeroes, Unaligned};

// -- Encoding traits --------------------------------------------------------

/// Serialize a domain type into its fixed-size wire representation.
///
/// The associated `Wire` type must implement `AsBytes` so the encoded
/// value can be written directly to the TCP stream.
pub trait WireEncode {
    type Wire: AsBytes;
    fn to_wire(&self) -> Self::Wire;
}

/// Deserialize a domain type from its fixed-size wire representation.
///
/// The associated `Wire` type must implement `FromBytes + FromZeroes`
/// so it can be zero-initialized and filled from the TCP stream.
/// `from_wire` may reject invalid byte patterns (e.g. unknown enum tags).
pub trait WireDecode: Sized {
    type Wire: FromBytes + FromZeroes + AsBytes;
    fn from_wire(wire: Self::Wire) -> Result<Self, &'static str>;
}

// -- Domain types -----------------------------------------------------------

pub const MAX_UGN_EDGES: usize = 64;

/// A measured clock offset between two directly connected nodes.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct UgnEdge {
    pub src_node: [u8; 12],
    pub src_port: u32,
    pub dst_node: [u8; 12],
    pub dst_port: u32,
    pub ugn: i64,
}

/// Collection of [`UgnEdge`]s observed by a single node.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UgnReport {
    pub count: u32,
    pub edges: [Option<UgnEdge>; MAX_UGN_EDGES],
}

impl Default for UgnReport {
    fn default() -> Self {
        Self::new()
    }
}

impl UgnReport {
    pub fn new() -> Self {
        Self {
            count: 0,
            edges: [None; MAX_UGN_EDGES],
        }
    }

    pub fn insert_edge(&mut self, edge: UgnEdge) -> bool {
        if self.count >= MAX_UGN_EDGES as u32 {
            warn!("report is full, cannot insert edge");
            return false;
        };
        self.edges[self.count as usize] = Some(edge);
        self.count = self.count.saturating_add(1);
        true
    }
}

// -- Commands ---------------------------------------------------------------

/// Commands that can be sent between nodes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Command {
    RequestUgnReport,
}

/// On-the-wire representation of [`Command`].
#[repr(C)]
#[derive(Clone, Copy, Debug, Default, FromZeroes, FromBytes, AsBytes, Unaligned, PartialEq, Eq)]
pub struct CommandWire {
    cmd_type: u8,
    _padding: [u8; 3],
}

impl From<Command> for CommandWire {
    fn from(cmd: Command) -> Self {
        let cmd_type = match cmd {
            Command::RequestUgnReport => 1,
        };
        Self {
            cmd_type,
            _padding: [0; 3],
        }
    }
}

impl TryFrom<CommandWire> for Command {
    type Error = &'static str;

    fn try_from(wire: CommandWire) -> Result<Self, Self::Error> {
        match wire.cmd_type {
            1 => Ok(Command::RequestUgnReport),
            _ => Err("Unknown command type"),
        }
    }
}

// -- Wire formats -----------------------------------------------------------

/// On-the-wire representation of [`UgnEdge`].
#[repr(C)]
#[derive(Clone, Copy, Debug, Default, FromZeroes, FromBytes, AsBytes, Unaligned)]
pub struct UgnEdgeWire {
    pub src_node: [u8; 12],
    pub dst_node: [u8; 12],
    pub src_port: U32<LE>,
    pub dst_port: U32<LE>,
    pub ugn: I64<LE>,
}

impl From<UgnEdge> for UgnEdgeWire {
    fn from(edge: UgnEdge) -> Self {
        Self {
            src_node: edge.src_node,
            dst_node: edge.dst_node,
            src_port: U32::new(edge.src_port),
            dst_port: U32::new(edge.dst_port),
            ugn: I64::new(edge.ugn),
        }
    }
}

impl From<UgnEdgeWire> for UgnEdge {
    fn from(edge: UgnEdgeWire) -> Self {
        Self {
            src_node: edge.src_node,
            dst_node: edge.dst_node,
            src_port: edge.src_port.get(),
            dst_port: edge.dst_port.get(),
            ugn: edge.ugn.get(),
        }
    }
}

// -- WireEncode / WireDecode impls ------------------------------------------

impl WireEncode for Command {
    type Wire = CommandWire;
    fn to_wire(&self) -> CommandWire {
        (*self).into()
    }
}

impl WireDecode for Command {
    type Wire = CommandWire;
    fn from_wire(wire: CommandWire) -> Result<Self, &'static str> {
        wire.try_into()
    }
}

impl WireEncode for UgnEdge {
    type Wire = UgnEdgeWire;
    fn to_wire(&self) -> UgnEdgeWire {
        (*self).into()
    }
}

impl WireDecode for UgnEdge {
    type Wire = UgnEdgeWire;
    fn from_wire(wire: UgnEdgeWire) -> Result<Self, &'static str> {
        Ok(wire.into())
    }
}

impl WireEncode for u32 {
    type Wire = U32<LE>;
    fn to_wire(&self) -> Self::Wire {
        U32::new(*self)
    }
}

impl WireDecode for u32 {
    type Wire = U32<LE>;
    fn from_wire(wire: U32<LE>) -> Result<Self, &'static str> {
        Ok(wire.get())
    }
}

impl WireEncode for usize {
    type Wire = U32<LE>;
    fn to_wire(&self) -> Self::Wire {
        U32::new(*self as u32)
    }
}
