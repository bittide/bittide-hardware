// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use log::warn;
use zerocopy::byteorder::{I64, LE, U32};
use zerocopy::{AsBytes, FromBytes, FromZeroes, Unaligned};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NodeRole {
    Manager,
    Subordinate,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ManagerState {
    WaitForSession,
    Identifying,
    ReceivingUgns,
    Done,
    Failed,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SubordinateState {
    WaitForSession,
    Identifying,
    SendingUgns,
    Done,
    Failed,
}

pub const MAX_UGN_EDGES: usize = 64;
pub const UGN_EDGE_BYTES: usize = core::mem::size_of::<UgnEdgeWire>();

pub const DNA_BYTES: usize = 12;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct UgnEdge {
    pub src_node: [u8; 12],
    pub src_port: u32,
    pub dst_node: [u8; 12],
    pub dst_port: u32,
    pub ugn: i64,
}

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

#[repr(C)]
#[derive(Clone, Copy, Debug, Default, FromZeroes, FromBytes, AsBytes, Unaligned)]
struct UgnEdgeWire {
    src_node: [u8; 12],
    dst_node: [u8; 12],
    src_port: U32<LE>,
    dst_port: U32<LE>,
    ugn: I64<LE>,
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

// fn encode_ugn_edge(edge: UgnEdge) -> [u8; UGN_EDGE_BYTES] {
//     let wire: UgnEdgeWire = edge.into();
//     let mut buf = [0u8; UGN_EDGE_BYTES];
//     buf.copy_from_slice(wire.as_bytes());
//     buf
// }

// fn parse_ugn_edge(msg: &[u8]) -> Option<UgnEdge> {
//     let (wire, _) = Ref::<_, UgnEdgeWire>::new_from_prefix(msg)?;
//     Some(UgnEdge::from(*wire))
// }

// pub fn ip_for_link(role: NodeRole, port: usize) -> [u8; 4] {
//     let host = match role {
//         NodeRole::Manager => 1,
//         NodeRole::Subordinate => 2,
//     };
//     [10, 0, port as u8, host]
// }
