// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use zerocopy::byteorder::{I64, LE, U32};
use zerocopy::{AsBytes, FromBytes, FromZeroes, Ref, Unaligned};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NodeRole {
    Manager,
    Subordinate,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ManagerState {
    WaitForPhy,
    SetupInterface,
    WaitForSession,
    SendWhoAreYou,
    WaitHello,
    SendUgnDump,
    WaitUgnReport,
    Done,
    Failed,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SubordinateState {
    WaitForPhy,
    SetupInterface,
    WaitForSession,
    WaitWhoAreYou,
    SendHello,
    WaitUgnDump,
    SendUgnReport,
    Done,
    Failed,
}

pub const MAX_UGN_EDGES: usize = 2;
pub const UGN_EDGE_BYTES: usize = 32;

const MSG_WHO_ARE_YOU: u8 = 1;
const MSG_HELLO: u8 = 2;
const MSG_UGN_DUMP: u8 = 3;
const MSG_UGN_REPORT: u8 = 4;
const ROLE_MANAGER: u8 = 0;
const ROLE_SUBORDINATE: u8 = 1;
const WHO_ARE_YOU_BYTES: usize = 8;
const HELLO_BYTES: usize = 8;
const UGN_DUMP_BYTES: usize = 1;
const UGN_REPORT_BYTES: usize = 2 + MAX_UGN_EDGES * UGN_EDGE_BYTES;

#[repr(C)]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct UgnEdge {
    pub src_node: u32,
    pub src_port: u32,
    pub dst_node: u32,
    pub dst_port: u32,
    pub ugn: i64,
    pub is_valid: u8,
    pub _padding: [u8; 7],
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct UgnReport {
    pub count: u8,
    pub _padding: [u8; 7],
    pub edges: [UgnEdge; MAX_UGN_EDGES],
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Default, FromZeroes, FromBytes, AsBytes, Unaligned)]
struct UgnEdgeWire {
    src_node: U32<LE>,
    src_port: U32<LE>,
    dst_node: U32<LE>,
    dst_port: U32<LE>,
    ugn: I64<LE>,
    is_valid: u8,
    _padding: [u8; 7],
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Default, FromZeroes, FromBytes, AsBytes, Unaligned)]
struct UgnReportWire {
    msg_type: u8,
    count: u8,
    edges: [UgnEdgeWire; MAX_UGN_EDGES],
}

impl From<UgnEdge> for UgnEdgeWire {
    fn from(edge: UgnEdge) -> Self {
        Self {
            src_node: U32::new(edge.src_node),
            src_port: U32::new(edge.src_port),
            dst_node: U32::new(edge.dst_node),
            dst_port: U32::new(edge.dst_port),
            ugn: I64::new(edge.ugn),
            is_valid: edge.is_valid,
            _padding: [0; 7],
        }
    }
}

impl From<UgnEdgeWire> for UgnEdge {
    fn from(edge: UgnEdgeWire) -> Self {
        Self {
            src_node: edge.src_node.get(),
            src_port: edge.src_port.get(),
            dst_node: edge.dst_node.get(),
            dst_port: edge.dst_port.get(),
            ugn: edge.ugn.get(),
            is_valid: edge.is_valid,
            _padding: [0; 7],
        }
    }
}

const _: [(); UGN_EDGE_BYTES] = [(); core::mem::size_of::<UgnEdgeWire>()];
const _: [(); UGN_REPORT_BYTES] = [(); core::mem::size_of::<UgnReportWire>()];

pub trait NetMedium {
    fn phy_ready(&self, link: usize) -> bool;
    fn setup_interface(&mut self, link: usize, local_ip: [u8; 4]);
    fn connect(&mut self, link: usize, peer_ip: [u8; 4]) -> bool;
    fn listen(&mut self, link: usize) -> bool;
    fn send(&mut self, link: usize, data: &[u8]) -> bool;
    fn recv(&mut self, link: usize, buf: &mut [u8]) -> Option<usize>;
    fn timed_out(&self, link: usize) -> bool;
}

pub struct Manager {
    state: ManagerState,
    retries: u8,
    nonce_seed: u32,
    nonce: u32,
    report: Option<UgnReport>,
}

impl Manager {
    pub fn new() -> Self {
        Self {
            state: ManagerState::WaitForPhy,
            retries: 0,
            nonce_seed: 1,
            nonce: 0,
            report: None,
        }
    }

    pub fn step<M: NetMedium>(&mut self, medium: &mut M, link: usize) {
        self.state = match self.state {
            ManagerState::WaitForPhy => {
                if medium.phy_ready(link) {
                    ManagerState::SetupInterface
                } else {
                    self.state
                }
            }
            ManagerState::SetupInterface => {
                medium.setup_interface(link, ip_for_link(NodeRole::Manager, link));
                ManagerState::WaitForSession
            }
            ManagerState::WaitForSession => {
                let peer_ip = ip_for_link(NodeRole::Subordinate, link);
                if medium.connect(link, peer_ip) {
                    ManagerState::SendWhoAreYou
                } else if medium.timed_out(link) {
                    self.bump_retry_or_fail(ManagerState::WaitForSession)
                } else {
                    self.state
                }
            }
            ManagerState::SendWhoAreYou => {
                self.nonce = self.next_nonce();
                let msg = encode_who_are_you(self.nonce);
                if medium.send(link, &msg) {
                    ManagerState::WaitHello
                } else {
                    self.state
                }
            }
            ManagerState::WaitHello => {
                let mut buf = [0u8; HELLO_BYTES];
                if let Some(len) = medium.recv(link, &mut buf) {
                    if parse_hello(&buf[..len], self.nonce) {
                        ManagerState::SendUgnDump
                    } else {
                        self.state
                    }
                } else if medium.timed_out(link) {
                    self.bump_retry_or_fail(ManagerState::SendWhoAreYou)
                } else {
                    self.state
                }
            }
            ManagerState::SendUgnDump => {
                let msg = [MSG_UGN_DUMP];
                if medium.send(link, &msg) {
                    ManagerState::WaitUgnReport
                } else {
                    self.state
                }
            }
            ManagerState::WaitUgnReport => {
                let mut buf = [0u8; UGN_REPORT_BYTES];
                if let Some(len) = medium.recv(link, &mut buf) {
                    if let Some(report) = parse_ugn_report(&buf[..len]) {
                        self.report = Some(report);
                        ManagerState::Done
                    } else {
                        self.state
                    }
                } else if medium.timed_out(link) {
                    self.bump_retry_or_fail(ManagerState::SendUgnDump)
                } else {
                    self.state
                }
            }
            ManagerState::Done => self.state,
            ManagerState::Failed => self.state,
        };
    }

    pub fn is_done(&self) -> bool {
        self.state == ManagerState::Done
    }

    pub fn is_connected(&self) -> bool {
        self.is_done()
    }

    pub fn state(&self) -> ManagerState {
        self.state
    }

    pub fn report(&self) -> Option<UgnReport> {
        self.report
    }

    fn bump_retry_or_fail(&mut self, retry_state: ManagerState) -> ManagerState {
        self.retries = self.retries.saturating_add(1);
        if self.retries >= 3 {
            ManagerState::Failed
        } else {
            retry_state
        }
    }

    fn next_nonce(&mut self) -> u32 {
        let next = self.nonce_seed;
        self.nonce_seed = self.nonce_seed.wrapping_add(1);
        next
    }
}

impl Default for Manager {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Subordinate {
    state: SubordinateState,
    retries: u8,
    nonce: u32,
    report: UgnReport,
}

impl Subordinate {
    pub fn new() -> Self {
        Self {
            state: SubordinateState::WaitForPhy,
            retries: 0,
            nonce: 0,
            report: UgnReport::default(),
        }
    }

    pub fn with_report(report: UgnReport) -> Self {
        Self {
            report,
            ..Self::new()
        }
    }

    pub fn set_report(&mut self, report: UgnReport) {
        self.report = report;
    }

    pub fn step<M: NetMedium>(&mut self, medium: &mut M, link: usize) {
        self.state = match self.state {
            SubordinateState::WaitForPhy => {
                if medium.phy_ready(link) {
                    SubordinateState::SetupInterface
                } else {
                    self.state
                }
            }
            SubordinateState::SetupInterface => {
                medium.setup_interface(link, ip_for_link(NodeRole::Subordinate, link));
                SubordinateState::WaitForSession
            }
            SubordinateState::WaitForSession => {
                if medium.listen(link) {
                    SubordinateState::WaitWhoAreYou
                } else if medium.timed_out(link) {
                    self.bump_retry_or_fail(SubordinateState::WaitForSession)
                } else {
                    self.state
                }
            }
            SubordinateState::WaitWhoAreYou => {
                let mut buf = [0u8; WHO_ARE_YOU_BYTES];
                if let Some(len) = medium.recv(link, &mut buf) {
                    if let Some(nonce) = parse_who_are_you(&buf[..len]) {
                        self.nonce = nonce;
                        SubordinateState::SendHello
                    } else {
                        self.state
                    }
                } else if medium.timed_out(link) {
                    self.bump_retry_or_fail(SubordinateState::WaitForSession)
                } else {
                    self.state
                }
            }
            SubordinateState::SendHello => {
                let msg = encode_hello(self.nonce);
                if medium.send(link, &msg) {
                    SubordinateState::WaitUgnDump
                } else {
                    self.state
                }
            }
            SubordinateState::WaitUgnDump => {
                let mut buf = [0u8; UGN_DUMP_BYTES];
                if let Some(len) = medium.recv(link, &mut buf) {
                    if parse_ugn_dump(&buf[..len]) {
                        SubordinateState::SendUgnReport
                    } else {
                        self.state
                    }
                } else if medium.timed_out(link) {
                    self.bump_retry_or_fail(SubordinateState::WaitUgnDump)
                } else {
                    self.state
                }
            }
            SubordinateState::SendUgnReport => {
                let msg = encode_ugn_report(&self.report);
                if medium.send(link, &msg) {
                    SubordinateState::Done
                } else {
                    self.state
                }
            }
            SubordinateState::Done => self.state,
            SubordinateState::Failed => self.state,
        };
    }

    pub fn is_done(&self) -> bool {
        self.state == SubordinateState::Done
    }

    pub fn is_connected(&self) -> bool {
        self.is_done()
    }

    pub fn state(&self) -> SubordinateState {
        self.state
    }

    fn bump_retry_or_fail(&mut self, retry_state: SubordinateState) -> SubordinateState {
        self.retries = self.retries.saturating_add(1);
        if self.retries >= 3 {
            SubordinateState::Failed
        } else {
            retry_state
        }
    }
}

impl Default for Subordinate {
    fn default() -> Self {
        Self::new()
    }
}

fn encode_who_are_you(nonce: u32) -> [u8; WHO_ARE_YOU_BYTES] {
    let mut msg = [0u8; WHO_ARE_YOU_BYTES];
    msg[0] = MSG_WHO_ARE_YOU;
    msg[1] = ROLE_MANAGER;
    msg[2] = 0;
    msg[3] = 0;
    msg[4..8].copy_from_slice(&nonce.to_le_bytes());
    msg
}

fn encode_hello(nonce: u32) -> [u8; HELLO_BYTES] {
    let mut msg = [0u8; HELLO_BYTES];
    msg[0] = MSG_HELLO;
    msg[1] = ROLE_SUBORDINATE;
    msg[2] = 0;
    msg[3] = 0;
    msg[4..8].copy_from_slice(&nonce.to_le_bytes());
    msg
}

fn parse_who_are_you(msg: &[u8]) -> Option<u32> {
    if msg.len() < WHO_ARE_YOU_BYTES {
        return None;
    }
    if msg[0] != MSG_WHO_ARE_YOU || msg[1] != ROLE_MANAGER {
        return None;
    }
    Some(u32::from_le_bytes([msg[4], msg[5], msg[6], msg[7]]))
}

fn parse_hello(msg: &[u8], nonce: u32) -> bool {
    if msg.len() < HELLO_BYTES {
        return false;
    }
    msg[0] == MSG_HELLO
        && msg[1] == ROLE_SUBORDINATE
        && u32::from_le_bytes([msg[4], msg[5], msg[6], msg[7]]) == nonce
}

fn parse_ugn_dump(msg: &[u8]) -> bool {
    msg.len() >= UGN_DUMP_BYTES && msg[0] == MSG_UGN_DUMP
}

fn encode_ugn_report(report: &UgnReport) -> [u8; UGN_REPORT_BYTES] {
    let mut wire = UgnReportWire {
        msg_type: MSG_UGN_REPORT,
        count: report.count,
        edges: [UgnEdgeWire::default(); MAX_UGN_EDGES],
    };
    for (idx, edge) in report.edges.iter().enumerate() {
        wire.edges[idx] = (*edge).into();
    }
    let mut buf = [0u8; UGN_REPORT_BYTES];
    buf.copy_from_slice(wire.as_bytes());
    buf
}

fn parse_ugn_report(msg: &[u8]) -> Option<UgnReport> {
    let (wire, _) = Ref::<_, UgnReportWire>::new_from_prefix(msg)?;
    if wire.msg_type != MSG_UGN_REPORT {
        return None;
    }
    let count = wire.count as usize;
    if count > MAX_UGN_EDGES {
        return None;
    }
    let mut report = UgnReport {
        count: wire.count,
        ..Default::default()
    };
    for idx in 0..count {
        report.edges[idx] = wire.edges[idx].into();
    }
    Some(report)
}

pub fn ip_for_link(role: NodeRole, port: usize) -> [u8; 4] {
    let host = match role {
        NodeRole::Manager => 1,
        NodeRole::Subordinate => 2,
    };
    [10, 0, port as u8, host]
}
