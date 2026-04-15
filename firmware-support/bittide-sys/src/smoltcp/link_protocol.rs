// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Wire Format Types for Link Protocol
//!
//! This module provides wire format types for communication between nodes.
//! Use these with LinkInterface's zerocopy send/recv methods.

use crate::net_state::UgnEdge;
use zerocopy::byteorder::{I64, LE, U32};
use zerocopy::{AsBytes, FromBytes, FromZeroes, Unaligned};

pub const DNA_BYTES: usize = 12;

/// Command types that can be sent between nodes
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Command {
    RequestUgnReport,
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
