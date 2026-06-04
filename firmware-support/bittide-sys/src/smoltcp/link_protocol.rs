// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Wire Format Types for Link Protocol
//!
//! This module provides wire format types for communication between nodes.
//! Use these with LinkInterface's zerocopy send/recv methods.

use crate::net_state::UgnEdge;
use zerocopy::byteorder::{I32, I64, LE, U32, U64};
use zerocopy::{AsBytes, FromBytes, FromZeroes, Unaligned};

pub const DNA_BYTES: usize = 12;

/// Maximum number of links per node. The per-node correction message carries one
/// elastic-buffer frame adjustment per local port, indexed by port number.
pub const MAX_LINKS: usize = 7;

/// Command types that can be sent between nodes
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Command {
    RequestUgnReport,
    /// The manager has groomed the full UGN graph and is about to send the recipient its
    /// per-node correction (a [`NodeCorrectionWire`]).
    ApplyCorrection,
    // Future commands can be added here
}

/// Wire format for commands
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
            Command::ApplyCorrection => 2,
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
            2 => Ok(Command::ApplyCorrection),
            _ => Err("Unknown command type"),
        }
    }
}

/// Wire format for a single node's UGN-grooming correction, computed by the manager and
/// sent to the node over its link.
///
/// `frames[p]` is the elastic-buffer adjustment (in frames) to apply to the recipient's
/// local port `p` (the frames to insert on the incoming edge captured at that port); `0`
/// means none. `release_cycle` is the local-counter value the node writes to its
/// `TimedReset.release_cycle` register to relabel (release its application reset).
#[repr(C)]
#[derive(Clone, Copy, Debug, Default, FromZeroes, FromBytes, AsBytes, Unaligned)]
pub struct NodeCorrectionWire {
    pub release_cycle: U64<LE>,
    pub frames: [I32<LE>; MAX_LINKS],
}

impl NodeCorrectionWire {
    pub fn new(release_cycle: u64, frames: [i32; MAX_LINKS]) -> Self {
        Self {
            release_cycle: U64::new(release_cycle),
            frames: frames.map(I32::new),
        }
    }

    /// The local-counter value to write to this node's `TimedReset.release_cycle`.
    pub fn release_cycle(&self) -> u64 {
        self.release_cycle.get()
    }

    /// The elastic-buffer frame adjustment for local port `port` (`0` = none).
    pub fn frame(&self, port: usize) -> i32 {
        self.frames[port].get()
    }
}

/// Wire format for UGN edges
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
