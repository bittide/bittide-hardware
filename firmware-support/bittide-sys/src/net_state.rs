// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use log::{debug, info, trace, warn};
use smoltcp::iface::{Interface, SocketHandle, SocketSet};
use smoltcp::socket::tcp;
use smoltcp::wire::IpAddress;
use zerocopy::byteorder::{I64, LE, U32};
use zerocopy::{AsBytes, FromBytes, FromZeroes, Ref, Unaligned};

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

pub const MAX_UGN_EDGES: usize = 8;
pub const UGN_EDGE_BYTES: usize = core::mem::size_of::<UgnEdgeWire>();

const DNA_BYTES: usize = 12;

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
    src_port: U32<LE>,
    dst_port: U32<LE>,
    ugn: I64<LE>,
}

impl From<UgnEdge> for UgnEdgeWire {
    fn from(edge: UgnEdge) -> Self {
        Self {
            src_node: U32::new(edge.src_node),
            dst_node: U32::new(edge.dst_node),
            src_port: U32::new(edge.src_port),
            dst_port: U32::new(edge.dst_port),
            ugn: I64::new(edge.ugn),
        }
    }
}

impl From<UgnEdgeWire> for UgnEdge {
    fn from(edge: UgnEdgeWire) -> Self {
        Self {
            src_node: edge.src_node.get(),
            dst_node: edge.dst_node.get(),
            src_port: edge.src_port.get(),
            dst_port: edge.dst_port.get(),
            ugn: edge.ugn.get(),
        }
    }
}

// const _: [(); UGN_EDGE_BYTES] = [(); core::mem::size_of::<UgnEdgeWire>()];

const TCP_SERVER_PORT: u16 = 8080;
const TCP_CLIENT_PORT: u16 = 49152;

pub struct Manager {
    iface: Interface,
    socket_handle: SocketHandle,
    link: usize,
    partner_ip: [u8; 4],
    state: ManagerState,
    retries: u8,
    report: UgnReport,
    partner_dna: Option<[u8; DNA_BYTES]>,
}

impl Manager {
    pub fn new(
        iface: Interface,
        socket_handle: SocketHandle,
        link: usize,
        partner_ip: [u8; 4],
    ) -> Self {
        Self {
            iface,
            socket_handle,
            link,
            partner_ip,
            state: ManagerState::WaitForSession,
            retries: 0,
            report: UgnReport::new(),
            partner_dna: None,
        }
    }

    pub fn iface(&self) -> &Interface {
        &self.iface
    }

    pub fn iface_mut(&mut self) -> &mut Interface {
        &mut self.iface
    }

    pub fn step(&mut self, sockets: &mut SocketSet) {
        let prev_state = self.state;
        let next_state = match self.state {
            ManagerState::WaitForSession => {
                self.connect(sockets);
                if self.can_receive(sockets) {
                    debug!(
                        "manager connected to peer {:?} on link {}",
                        self.partner_ip, self.link
                    );
                    ManagerState::Identifying
                } else {
                    self.state
                }
            }
            ManagerState::Identifying => {
                // Expect the subordinate to identify itself

                let mut buf = [0u8; DNA_BYTES];
                match self.recv(sockets, &mut buf) {
                    Some(len) if len == DNA_BYTES => {
                        self.partner_dna = Some(buf);
                        info!(
                            "manager received partner DNA {:02X?} on link {}",
                            self.partner_dna, self.link
                        );
                        ManagerState::ReceivingUgns
                    }
                    Some(len) => {
                        warn!(
                            "manager received invalid identify len {} on link {}",
                            len, self.link
                        );
                        self.reset(sockets);
                        self.bump_retry_or_fail(ManagerState::Identifying)
                    }
                    None => self.state,
                }
            }
            ManagerState::ReceivingUgns => {
                trace!("manager receiving ugns on link {}", self.link);
                if self.can_receive(sockets) {
                    let mut buf = [0u8; UGN_EDGE_BYTES];
                    match self.recv(sockets, &mut buf) {
                        Some(len) if len == UGN_EDGE_BYTES => match parse_ugn_edge(&buf[..len]) {
                            Some(edge) => {
                                info!(
                                    "manager received edge src {} dst {} ugn {} on link {}",
                                    edge.src_node, edge.dst_node, edge.ugn, self.link
                                );
                                self.insert_edge(edge);
                                self.state
                            }
                            None => {
                                warn!("manager received invalid ugn edge on link {}", self.link);
                                self.reset(sockets);
                                self.bump_retry_or_fail(ManagerState::ReceivingUgns)
                            }
                        },
                        Some(len) => {
                            warn!(
                                "manager received invalid ugn edge len {} on link {}",
                                len, self.link
                            );
                            self.reset(sockets);
                            self.bump_retry_or_fail(ManagerState::ReceivingUgns)
                        }
                        None => self.state,
                    }
                } else {
                    ManagerState::Done
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

    pub fn poll(
        &mut self,
        timestamp: smoltcp::time::Instant,
        device: &mut impl smoltcp::phy::Device,
        sockets: &mut SocketSet,
    ) {
        self.step(sockets);
        self.iface.poll(timestamp, device, sockets);
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

    pub fn report(&self) -> UgnReport {
        self.report
    }

    fn reset(&mut self, sockets: &mut SocketSet) {
        let socket = sockets.get_mut::<tcp::Socket>(self.socket_handle);
        debug!("link {} resetting interface", self.link);
        socket.close();
    }

    fn connect(&mut self, sockets: &mut SocketSet) -> bool {
        let socket = sockets.get_mut::<tcp::Socket>(self.socket_handle);
        if !socket.is_open() && !socket.is_active() {
            let cx = self.iface.context();
            match socket.connect(
                cx,
                (
                    IpAddress::v4(
                        self.partner_ip[0],
                        self.partner_ip[1],
                        self.partner_ip[2],
                        self.partner_ip[3],
                    ),
                    TCP_SERVER_PORT,
                ),
                TCP_CLIENT_PORT,
            ) {
                Ok(()) => {
                    debug!(
                        "link {} connect requested to {:?}:{} from {}",
                        self.link, self.partner_ip, TCP_SERVER_PORT, TCP_CLIENT_PORT
                    );
                }
                Err(err) => {
                    debug!("link {} connect error: {:?}", self.link, err);
                }
            }
        }
        trace!(
            "link {} connect state open {} active {} can_send {} can_recv {} may_recv {}",
            self.link,
            socket.is_open(),
            socket.is_active(),
            socket.can_send(),
            socket.can_recv(),
            socket.may_recv()
        );
        socket.is_active()
    }

    fn recv(&mut self, sockets: &mut SocketSet, buf: &mut [u8]) -> Option<usize> {
        let socket = sockets.get_mut::<tcp::Socket>(self.socket_handle);
        if socket.can_recv() {
            match socket.recv_slice(buf) {
                Ok(len) => {
                    trace!("link {} recv {} bytes", self.link, len);
                    return Some(len);
                }
                Err(err) => {
                    debug!("link {} recv error: {:?}", self.link, err);
                }
            }
        } else {
            trace!(
                "link {} recv blocked open {} active {}",
                self.link,
                socket.is_open(),
                socket.is_active()
            );
        }
        None
    }

    fn can_receive(&self, sockets: &SocketSet) -> bool {
        let socket = sockets.get::<tcp::Socket>(self.socket_handle);
        socket.may_recv()
    }

    fn bump_retry_or_fail(&mut self, retry_state: ManagerState) -> ManagerState {
        self.retries = self.retries.saturating_add(1);
        if self.retries >= 3 {
            warn!("manager retry limit hit, entering Failed");
            ManagerState::Failed
        } else {
            retry_state
        }
    }

    fn insert_edge(&mut self, edge: UgnEdge) -> bool {
        if self.report.count >= MAX_UGN_EDGES as u8 {
            warn!("manager received edge but report is full, ignoring");
            return false;
        }
        self.report.edges[self.report.count as usize] = Some(edge);
        self.report.count = self.report.count.saturating_add(1);
        true
    }
}

pub struct Subordinate {
    iface: Interface,
    socket_handle: SocketHandle,
    link: usize,
    state: SubordinateState,
    report: UgnReport,
    sent_edges: [bool; MAX_UGN_EDGES],
    sent_count: u8,
    dna: [u8; DNA_BYTES],
}

impl Subordinate {
    pub fn new(
        iface: Interface,
        socket_handle: SocketHandle,
        link: usize,
        dna: [u8; DNA_BYTES],
    ) -> Self {
        Self {
            iface,
            socket_handle,
            link,
            state: SubordinateState::WaitForSession,
            report: UgnReport::new(),
            sent_edges: [false; MAX_UGN_EDGES],
            sent_count: 0,
            dna,
        }
    }

    pub fn iface(&self) -> &Interface {
        &self.iface
    }

    pub fn iface_mut(&mut self) -> &mut Interface {
        &mut self.iface
    }

    pub fn set_report(&mut self, report: UgnReport) {
        self.report = report;
        self.sent_edges = [false; MAX_UGN_EDGES];
        self.sent_count = 0;
    }

    pub fn insert_edge(&mut self, edge: UgnEdge) -> bool {
        if self.report.count >= MAX_UGN_EDGES as u8 {
            warn!("subordinate received edge but report is full, ignoring");
            return false;
        }
        self.report.edges[self.report.count as usize] = Some(edge);
        self.report.count = self.report.count.saturating_add(1);
        true
    }

    pub fn step(&mut self, sockets: &mut SocketSet) {
        let prev_state = self.state;
        let next_state = match self.state {
            SubordinateState::WaitForSession => {
                trace!(
                    "subordinate state {:?} attempting listen on link {}",
                    self.state,
                    self.link
                );
                if self.listen(sockets) {
                    debug!("subordinate listening on link {}", self.link);

                    SubordinateState::Identifying
                } else {
                    self.state
                }
            }
            SubordinateState::Identifying => {
                if self.can_send(sockets) {
                    let dna = self.dna;
                    if self.send(sockets, &dna) {
                        debug!(
                            "subordinate sent dna {:02X?} on link {}",
                            self.dna, self.link
                        );
                        SubordinateState::SendingUgns
                    } else {
                        self.state
                    }
                } else {
                    self.state
                }
            }
            SubordinateState::SendingUgns => {
                trace!(
                    "subordinate state {:?} checking for manager connection on link {}",
                    self.state,
                    self.link
                );
                if self.can_send(sockets) {
                    if let Some((idx, edge)) = self.next_unsent_edge() {
                        let edge_bytes = encode_ugn_edge(edge);
                        if self.send(sockets, &edge_bytes) {
                            debug!(
                                "subordinate sent edge idx {} src {} dst {} ugn {} on link {}",
                                idx, edge.src_node, edge.dst_node, edge.ugn, self.link
                            );
                            self.sent_edges[idx] = true;
                            self.sent_count = self.sent_count.saturating_add(1);
                        }
                    }
                    if self.sending_done() {
                        info!("Closing link {}", self.link);
                        self.close(sockets);
                        SubordinateState::Done
                    } else {
                        self.state
                    }
                } else {
                    self.state
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

    pub fn poll(
        &mut self,
        timestamp: smoltcp::time::Instant,
        device: &mut impl smoltcp::phy::Device,
        sockets: &mut SocketSet,
    ) {
        self.iface.poll(timestamp, device, sockets);
        self.step(sockets);
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

    fn listen(&mut self, sockets: &mut SocketSet) -> bool {
        let socket = sockets.get_mut::<tcp::Socket>(self.socket_handle);
        if !socket.is_open() {
            match socket.listen(TCP_SERVER_PORT) {
                Ok(()) => {
                    debug!("link {} listen on port {}", self.link, TCP_SERVER_PORT);
                }
                Err(err) => {
                    debug!("link {} listen error: {:?}", self.link, err);
                }
            }
        }
        trace!(
            "link {} listen state open {} active {} can_send {} can_recv {} may_recv {}",
            self.link,
            socket.is_open(),
            socket.is_active(),
            socket.can_send(),
            socket.can_recv(),
            socket.may_recv()
        );
        socket.is_open()
    }

    fn send(&mut self, sockets: &mut SocketSet, data: &[u8]) -> bool {
        let socket = sockets.get_mut::<tcp::Socket>(self.socket_handle);
        if socket.can_send() {
            match socket.send_slice(data) {
                Ok(len) => {
                    trace!("link {} sent {} bytes", self.link, len);
                    return true;
                }
                Err(err) => {
                    debug!("link {} send error: {:?}", self.link, err);
                }
            }
        } else {
            trace!(
                "link {} send blocked open {} active {}",
                self.link,
                socket.is_open(),
                socket.is_active()
            );
        }
        false
    }

    fn close(&mut self, sockets: &mut SocketSet) {
        let socket = sockets.get_mut::<tcp::Socket>(self.socket_handle);
        socket.close();
    }

    fn next_unsent_edge(&self) -> Option<(usize, UgnEdge)> {
        let limit = core::cmp::min(self.report.count as usize, self.report.edges.len());
        for idx in 0..limit {
            if !self.sent_edges[idx] {
                if let Some(edge) = self.report.edges[idx] {
                    return Some((idx, edge));
                }
            }
        }
        None
    }

    fn sending_done(&self) -> bool {
        self.sent_count as usize >= self.report.count as usize
    }

    fn can_send(&self, sockets: &SocketSet) -> bool {
        let socket = sockets.get::<tcp::Socket>(self.socket_handle);
        socket.can_send()
    }
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

// pub fn ip_for_link(role: NodeRole, port: usize) -> [u8; 4] {
//     let host = match role {
//         NodeRole::Manager => 1,
//         NodeRole::Subordinate => 2,
//     };
//     [10, 0, port as u8, host]
// }
