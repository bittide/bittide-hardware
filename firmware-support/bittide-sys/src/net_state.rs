// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use log::{debug, info, trace, warn};
use smoltcp::iface::{Interface, SocketHandle, SocketSet};
use smoltcp::socket::tcp;
use smoltcp::wire::{IpAddress, IpCidr};
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
    Identifying,
    ReceivingUgns,
    Done,
    Failed,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SubordinateState {
    WaitForPhy,
    SetupInterface,
    WaitForSession,
    SendingUgns,
    Done,
    Failed,
}

pub const MAX_UGN_EDGES: usize = 8;
pub const UGN_EDGE_BYTES: usize = 16;

const MSG_WHO_ARE_YOU: u8 = 1;
const MSG_HELLO: u8 = 2;
const MSG_UGN_DUMP: u8 = 3;
const ROLE_MANAGER: u8 = 0;
const ROLE_SUBORDINATE: u8 = 1;
const WHO_ARE_YOU_BYTES: usize = 8;
const HELLO_BYTES: usize = 8;
const UGN_DUMP_BYTES: usize = 1;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct UgnEdge {
    pub src_node: u32,
    pub src_port: u32,
    pub dst_node: u32,
    pub dst_port: u32,
    pub ugn: i64,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct UgnReport {
    pub count: u8,
    pub edges: [Option<UgnEdge>; MAX_UGN_EDGES],
}

impl UgnReport {
    pub fn new() -> Self {
        Self {
            count: 0,
            edges: [None; MAX_UGN_EDGES],
        }
    }
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Default, FromZeroes, FromBytes, AsBytes, Unaligned)]
struct UgnEdgeWire {
    src_node: U32<LE>,
    dst_node: U32<LE>,
    ugn: I64<LE>,
}

impl From<UgnEdge> for UgnEdgeWire {
    fn from(edge: UgnEdge) -> Self {
        Self {
            src_node: U32::new(edge.src_node),
            dst_node: U32::new(edge.dst_node),
            ugn: I64::new(edge.ugn),
        }
    }
}

impl From<UgnEdgeWire> for UgnEdge {
    fn from(edge: UgnEdgeWire) -> Self {
        Self {
            src_node: edge.src_node.get(),
            dst_node: edge.dst_node.get(),
            src_port: 0,
            dst_port: 0,
            ugn: edge.ugn.get(),
        }
    }
}

const _: [(); UGN_EDGE_BYTES] = [(); core::mem::size_of::<UgnEdgeWire>()];

const TCP_SERVER_PORT: u16 = 8080;
const TCP_CLIENT_PORT: u16 = 49152;

pub struct SmoltcpLink<'a, 'b> {
    iface: &'a mut Interface,
    sockets: &'a mut SocketSet<'b>,
    socket_handle: SocketHandle,
    link: usize,
    phy_ready: bool,
    timed_out: bool,
}

impl<'a, 'b> SmoltcpLink<'a, 'b> {
    pub fn new(
        iface: &'a mut Interface,
        sockets: &'a mut SocketSet<'b>,
        socket_handle: SocketHandle,
        link: usize,
        phy_ready: bool,
        timed_out: bool,
    ) -> Self {
        Self {
            iface,
            sockets,
            socket_handle,
            link,
            phy_ready,
            timed_out,
        }
    }

    pub fn link(&self) -> usize {
        self.link
    }

    pub fn phy_ready(&self) -> bool {
        self.phy_ready
    }

    pub fn timed_out(&self) -> bool {
        self.timed_out
    }

    pub fn setup_interface(&mut self, role: NodeRole) {
        let local_ip = ip_for_link(role, self.link);
        let ip = IpCidr::new(
            IpAddress::v4(local_ip[0], local_ip[1], local_ip[2], local_ip[3]),
            24,
        );
        self.iface.update_ip_addrs(|addrs| {
            if !addrs.contains(&ip) {
                let _ = addrs.push(ip);
            }
        });
    }

    pub fn connect(&mut self, peer_ip: [u8; 4]) -> bool {
        let socket = self.sockets.get_mut::<tcp::Socket>(self.socket_handle);
        if !socket.is_open() && !socket.is_active() {
            let cx = self.iface.context();
            let _ = socket.connect(
                cx,
                (
                    IpAddress::v4(peer_ip[0], peer_ip[1], peer_ip[2], peer_ip[3]),
                    TCP_SERVER_PORT,
                ),
                TCP_CLIENT_PORT,
            );
        }
        socket.is_active()
    }

    pub fn listen(&mut self) -> bool {
        let socket = self.sockets.get_mut::<tcp::Socket>(self.socket_handle);
        if !socket.is_open() {
            let _ = socket.listen(TCP_SERVER_PORT);
        }
        socket.is_open()
    }

    pub fn send(&mut self, data: &[u8]) -> bool {
        let socket = self.sockets.get_mut::<tcp::Socket>(self.socket_handle);
        if socket.can_send() {
            let _ = socket.send_slice(data);
            return true;
        }
        false
    }

    pub fn recv(&mut self, buf: &mut [u8]) -> Option<usize> {
        let socket = self.sockets.get_mut::<tcp::Socket>(self.socket_handle);
        if socket.can_recv() {
            if let Ok(len) = socket.recv_slice(buf) {
                return Some(len);
            }
        }
        None
    }

    pub fn is_open_manager(&mut self) -> bool {
        let socket = self.sockets.get::<tcp::Socket>(self.socket_handle);
        socket.may_recv()
    }

    pub fn close(&mut self) {
        let socket = self.sockets.get_mut::<tcp::Socket>(self.socket_handle);
        socket.close();
    }
}

pub struct Manager {
    state: ManagerState,
    retries: u8,
    nonce_seed: u32,
    nonce: u32,
    received_edges: u8,
    report: Option<UgnReport>,
    identifying_sent: bool,
    ugn_dump_sent: bool,
}

impl Manager {
    pub fn new() -> Self {
        Self {
            state: ManagerState::WaitForPhy,
            retries: 0,
            nonce_seed: 1,
            nonce: 0,
            received_edges: 0,
            report: None,
            identifying_sent: false,
            ugn_dump_sent: false,
        }
    }

    pub fn step(&mut self, link: &mut SmoltcpLink<'_, '_>) {
        let prev_state = self.state;
        let next_state = match self.state {
            ManagerState::WaitForPhy => {
                if link.phy_ready() {
                    debug!("manager phy ready on link {}", link.link());
                    ManagerState::SetupInterface
                } else {
                    self.state
                }
            }
            ManagerState::SetupInterface => {
                debug!("manager setup interface link {}", link.link());
                link.setup_interface(NodeRole::Manager);
                ManagerState::WaitForSession
            }
            ManagerState::WaitForSession => {
                let peer_ip = ip_for_link(NodeRole::Subordinate, link.link());
                if link.connect(peer_ip) {
                    debug!("manager connected link {} peer {:?}", link.link(), peer_ip);
                    self.identifying_sent = false;
                    self.ugn_dump_sent = false;
                    ManagerState::Identifying
                } else if link.timed_out() {
                    self.bump_retry_or_fail(ManagerState::WaitForSession)
                } else {
                    self.state
                }
            }
            ManagerState::Identifying => {
                if !self.identifying_sent {
                    self.nonce = self.next_nonce();
                    let msg = encode_who_are_you(self.nonce);
                    if link.send(&msg) {
                        debug!("manager sent who-are-you nonce {}", self.nonce);
                        self.identifying_sent = true;
                    }
                }

                if !self.identifying_sent {
                    self.state
                } else {
                    let mut buf = [0u8; HELLO_BYTES];
                    if let Some(len) = link.recv(&mut buf) {
                        if parse_hello(&buf[..len], self.nonce) {
                            debug!("manager received hello nonce {}", self.nonce);
                            self.ugn_dump_sent = false;
                            ManagerState::ReceivingUgns
                        } else {
                            warn!("manager received invalid hello len {}", len);
                            self.state
                        }
                    } else if link.timed_out() {
                        self.identifying_sent = false;
                        self.bump_retry_or_fail(ManagerState::Identifying)
                    } else {
                        self.state
                    }
                }
            }
            ManagerState::ReceivingUgns => {
                if !self.ugn_dump_sent {
                    let msg = [MSG_UGN_DUMP];
                    if link.send(&msg) {
                        debug!("manager sent ugn dump");
                        self.ugn_dump_sent = true;
                    }
                }

                if !self.ugn_dump_sent {
                    self.state
                } else if !link.is_open_manager() {
                    if self.report.is_none() {
                        self.report = Some(UgnReport::new());
                    }
                    info!(
                        "manager connection closed after {} edges",
                        self.received_edges
                    );
                    ManagerState::Done
                } else {
                    let mut buf = [0u8; UGN_EDGE_BYTES];
                    if let Some(len) = link.recv(&mut buf) {
                        if let Some(edge) = parse_ugn_edge(&buf[..len]) {
                            let mut inserted = false;
                            {
                                let report = self.report.get_or_insert_with(UgnReport::new);
                                if insert_edge(report, edge) {
                                    inserted = true;
                                }
                            }
                            if inserted {
                                self.received_edges = self.received_edges.saturating_add(1);
                                if let Some(report) = self.report.as_mut() {
                                    report.count = self.received_edges;
                                }
                                debug!(
                                    "manager stored edge src {} dst {} ugn {}",
                                    edge.src_node, edge.dst_node, edge.ugn
                                );
                            } else {
                                trace!(
                                    "manager dropped duplicate/full edge src {} dst {}",
                                    edge.src_node,
                                    edge.dst_node
                                );
                            }
                            self.state
                        } else {
                            debug!("manager received invalid edge len {}", len);
                            self.state
                        }
                    } else if link.timed_out() {
                        self.ugn_dump_sent = false;
                        self.bump_retry_or_fail(ManagerState::ReceivingUgns)
                    } else {
                        self.state
                    }
                }
            }
            ManagerState::Done => self.state,
            ManagerState::Failed => self.state,
        };
        if next_state != prev_state {
            info!("manager state {:?} -> {:?}", prev_state, next_state);
        }
        self.state = next_state;
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
            warn!("manager retry limit hit, entering Failed");
            ManagerState::Failed
        } else {
            info!(
                "manager timeout, retry {} -> {:?}",
                self.retries, retry_state
            );
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
    sent_edges: [bool; MAX_UGN_EDGES],
    sent_count: u8,
    who_are_you_received: bool,
    hello_sent: bool,
    ugn_dump_received: bool,
}

impl Subordinate {
    pub fn new() -> Self {
        Self {
            state: SubordinateState::WaitForPhy,
            retries: 0,
            nonce: 0,
            report: UgnReport::new(),
            sent_edges: [false; MAX_UGN_EDGES],
            sent_count: 0,
            who_are_you_received: false,
            hello_sent: false,
            ugn_dump_received: false,
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
        self.sent_edges = [false; MAX_UGN_EDGES];
        self.sent_count = 0;
        self.reset_handshake();
    }

    pub fn step(&mut self, link: &mut SmoltcpLink<'_, '_>) {
        let prev_state = self.state;
        let next_state = match self.state {
            SubordinateState::WaitForPhy => {
                if link.phy_ready() {
                    debug!("subordinate phy ready on link {}", link.link());
                    SubordinateState::SetupInterface
                } else {
                    self.state
                }
            }
            SubordinateState::SetupInterface => {
                debug!("subordinate setup interface link {}", link.link());
                link.setup_interface(NodeRole::Subordinate);
                SubordinateState::WaitForSession
            }
            SubordinateState::WaitForSession => {
                if link.listen() {
                    debug!("subordinate listening on link {}", link.link());
                    self.reset_handshake();
                    SubordinateState::SendingUgns
                } else if link.timed_out() {
                    self.bump_retry_or_fail(SubordinateState::WaitForSession)
                } else {
                    self.state
                }
            }
            SubordinateState::SendingUgns => {
                if link.timed_out() && !self.ugn_dump_received {
                    self.reset_handshake();
                    self.bump_retry_or_fail(SubordinateState::SendingUgns)
                } else if !self.who_are_you_received {
                    let mut buf = [0u8; WHO_ARE_YOU_BYTES];
                    if let Some(len) = link.recv(&mut buf) {
                        if let Some(nonce) = parse_who_are_you(&buf[..len]) {
                            self.nonce = nonce;
                            self.who_are_you_received = true;
                            debug!("subordinate received who-are-you nonce {}", nonce);
                        } else {
                            warn!("subordinate received invalid who-are-you len {}", len);
                        }
                    }
                    self.state
                } else if !self.hello_sent {
                    let msg = encode_hello(self.nonce);
                    if link.send(&msg) {
                        debug!("subordinate sent hello nonce {}", self.nonce);
                        self.hello_sent = true;
                    }
                    self.state
                } else if !self.ugn_dump_received {
                    let mut buf = [0u8; UGN_DUMP_BYTES];
                    if let Some(len) = link.recv(&mut buf) {
                        if parse_ugn_dump(&buf[..len]) {
                            debug!("subordinate received ugn dump");
                            self.ugn_dump_received = true;
                        } else {
                            warn!("subordinate received invalid ugn dump len {}", len);
                        }
                    }
                    self.state
                } else {
                    let total = self.report.count as usize;
                    if total == 0 {
                        info!("subordinate no edges to send, closing link {}", link.link());
                        link.close();
                        SubordinateState::Done
                    } else if (self.sent_count as usize) >= total {
                        info!("subordinate sent all edges, closing link {}", link.link());
                        link.close();
                        SubordinateState::Done
                    } else if let Some((idx, edge)) = self.next_unsent_edge(total) {
                        let msg = encode_ugn_edge(edge);
                        if link.send(&msg) {
                            debug!(
                                "subordinate sent edge idx {} src {} dst {} ugn {}",
                                idx, edge.src_node, edge.dst_node, edge.ugn
                            );
                            self.sent_edges[idx] = true;
                            self.sent_count = self.sent_count.saturating_add(1);
                            if (self.sent_count as usize) >= total {
                                info!("subordinate finished edges, closing link {}", link.link());
                                link.close();
                                SubordinateState::Done
                            } else {
                                self.state
                            }
                        } else {
                            self.state
                        }
                    } else {
                        debug!(
                            "subordinate has no unsent edges (count {}, sent {})",
                            total, self.sent_count
                        );
                        self.state
                    }
                }
            }
            SubordinateState::Done => self.state,
            SubordinateState::Failed => self.state,
        };
        if next_state != prev_state {
            info!("subordinate state {:?} -> {:?}", prev_state, next_state);
        }
        self.state = next_state;
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

    fn reset_handshake(&mut self) {
        self.who_are_you_received = false;
        self.hello_sent = false;
        self.ugn_dump_received = false;
    }

    fn bump_retry_or_fail(&mut self, retry_state: SubordinateState) -> SubordinateState {
        self.retries = self.retries.saturating_add(1);
        if self.retries >= 3 {
            warn!("subordinate retry limit hit, entering Failed");
            SubordinateState::Failed
        } else {
            info!(
                "subordinate timeout, retry {} -> {:?}",
                self.retries, retry_state
            );
            retry_state
        }
    }

    fn next_unsent_edge(&self, total: usize) -> Option<(usize, UgnEdge)> {
        let limit = core::cmp::min(total, self.report.edges.len());
        for idx in 0..limit {
            if !self.sent_edges[idx] {
                if let Some(edge) = self.report.edges[idx] {
                    return Some((idx, edge));
                }
            }
        }
        None
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

fn encode_ugn_edge(edge: UgnEdge) -> [u8; UGN_EDGE_BYTES] {
    let wire: UgnEdgeWire = edge.into();
    let mut buf = [0u8; UGN_EDGE_BYTES];
    buf.copy_from_slice(wire.as_bytes());
    buf
}

fn parse_ugn_edge(msg: &[u8]) -> Option<UgnEdge> {
    let (wire, _) = Ref::<_, UgnEdgeWire>::new_from_prefix(msg)?;
    Some(UgnEdge::from(*wire))
}

fn insert_edge(report: &mut UgnReport, edge: UgnEdge) -> bool {
    if report
        .edges
        .iter()
        .flatten()
        .any(|existing| existing.src_node == edge.src_node && existing.dst_node == edge.dst_node)
    {
        return false;
    }
    if let Some(slot) = report.edges.iter_mut().find(|slot| slot.is_none()) {
        *slot = Some(edge);
        true
    } else {
        false
    }
}

pub fn ip_for_link(role: NodeRole, port: usize) -> [u8; 4] {
    let host = match role {
        NodeRole::Manager => 1,
        NodeRole::Subordinate => 2,
    };
    [10, 0, port as u8, host]
}
