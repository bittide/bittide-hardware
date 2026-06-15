// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::manual_additions::ring_buffer::{
    AlignedReceiveBuffer, ReceiveRingBufferInterface, TransmitRingBufferInterface,
};
use bittide_hal::manual_additions::timer::{Duration, Instant};
use smoltcp::iface::{Config, Interface, SocketSet, SocketStorage};
use smoltcp::socket::tcp;
use smoltcp::wire::{HardwareAddress, IpAddress, IpCidr, IpEndpoint};
use zerocopy::{AsBytes, FromBytes, FromZeroes};

use log::debug;

use crate::smoltcp::ring_buffer::RingBufferDevice;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecvError {
    WouldBlock,
    NotEstablished,
    Closed,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SendError {
    WouldBlock,
    NotEstablished,
    Closed,
}

// Each link is a point-to-point connection with its own interface and socket, so no
// routing takes place and the addresses don't matter: we only use smoltcp's TCP state
// machine. Both endpoints of every link therefore use this same IP and port, which
// also makes the connection a TCP simultaneous open.
const LINK_IP: IpAddress = IpAddress::v4(100, 100, 100, 100);
const LINK_PORT: u16 = 8080;

/// TCP communication over a pair of ring_buffers.
///
/// The TCP buffers are borrowed from the caller for `'a`, so the struct
/// contains no self-references and can move freely. Constructing it
/// initializes smoltcp and starts TCP simultaneous open.
pub struct LinkInterface<'a, RxRb, TxRb> {
    device: RingBufferDevice<RxRb, TxRb>,
    timer: bittide_hal::shared_devices::Timer,
    iface: Interface,
    sockets: SocketSet<'a>,
    socket_handle: smoltcp::iface::SocketHandle,
    last_state: tcp::State,
}

impl<'a, RxRb, TxRb> LinkInterface<'a, RxRb, TxRb>
where
    RxRb: ReceiveRingBufferInterface + 'static,
    TxRb: TransmitRingBufferInterface + 'static,
{
    /// Initialize smoltcp and start TCP simultaneous open.
    pub fn new(
        rx_aligned: AlignedReceiveBuffer<RxRb, TxRb>,
        tx_buffer: TxRb,
        timer: bittide_hal::shared_devices::Timer,
        tcp_rx_buffer: &'a mut [u8],
        tcp_tx_buffer: &'a mut [u8],
        socket_storage: &'a mut [SocketStorage<'a>],
    ) -> Self {
        let mut device = RingBufferDevice::new(rx_aligned, tx_buffer);

        let config = Config::new(HardwareAddress::Ip);
        let now = to_smoltcp_instant(timer.now());
        let mut iface = Interface::new(config, &mut device, now);

        iface.update_ip_addrs(|addrs| {
            addrs.push(IpCidr::new(LINK_IP, 24)).unwrap();
        });

        let socket = tcp::Socket::new(
            tcp::SocketBuffer::new(tcp_rx_buffer),
            tcp::SocketBuffer::new(tcp_tx_buffer),
        );

        let mut sockets = SocketSet::new(socket_storage);
        let handle = sockets.add(socket);

        let remote = IpEndpoint::new(LINK_IP, LINK_PORT);
        sockets
            .get_mut::<tcp::Socket>(handle)
            .connect(iface.context(), remote, LINK_PORT)
            .unwrap();

        let last_state = sockets.get::<tcp::Socket>(handle).state();
        debug!("LinkInterface connected, initial state: {last_state:?}");
        Self {
            device,
            timer,
            iface,
            sockets,
            socket_handle: handle,
            last_state,
        }
    }

    fn socket(&self) -> &tcp::Socket<'a> {
        self.sockets.get::<tcp::Socket>(self.socket_handle)
    }

    fn socket_mut(&mut self) -> &mut tcp::Socket<'a> {
        self.sockets.get_mut::<tcp::Socket>(self.socket_handle)
    }

    pub fn poll(&mut self) {
        let timestamp = to_smoltcp_instant(self.timer.now());
        self.iface
            .poll(timestamp, &mut self.device, &mut self.sockets);

        let new_state = self.socket().state();
        if new_state != self.last_state {
            debug!("TCP state: {:?} -> {:?}", self.last_state, new_state);
            self.last_state = new_state;
        }
    }

    pub fn is_established(&self) -> bool {
        self.socket().state() == tcp::State::Established
    }

    pub fn is_open(&self) -> bool {
        self.socket().is_open()
    }

    pub fn is_active(&self) -> bool {
        self.socket().is_active()
    }

    pub fn is_closed(&self) -> bool {
        self.socket().state() == tcp::State::Closed
    }

    pub fn is_closing_or_closed(&self) -> bool {
        !matches!(
            self.socket().state(),
            tcp::State::Established | tcp::State::SynSent | tcp::State::SynReceived
        )
    }

    pub fn state(&self) -> tcp::State {
        self.socket().state()
    }

    pub fn send(&mut self, data: &[u8]) -> usize {
        let socket = self.socket_mut();
        if socket.can_send() {
            socket.send_slice(data).unwrap_or(0)
        } else {
            0
        }
    }

    pub fn try_send_bytes(&mut self, data: &[u8]) -> Result<usize, SendError> {
        let socket = self.socket_mut();
        if socket.state() == tcp::State::Closed {
            return Err(SendError::Closed);
        }
        if !socket.is_active() {
            return Err(SendError::NotEstablished);
        }
        if socket.can_send() {
            let n = socket.send_slice(data).unwrap_or(0);
            if n > 0 {
                Ok(n)
            } else {
                Err(SendError::WouldBlock)
            }
        } else {
            Err(SendError::WouldBlock)
        }
    }

    pub fn send_blocking_bytes(
        &mut self,
        data: &[u8],
        timeout: Duration,
    ) -> Result<usize, SendError> {
        let deadline = self.timer.now() + timeout;
        loop {
            self.poll();
            match self.try_send_bytes(data) {
                Ok(n) => return Ok(n),
                Err(SendError::WouldBlock) if self.timer.now() < deadline => continue,
                Err(SendError::WouldBlock) => return Err(SendError::WouldBlock),
                Err(e) => return Err(e),
            }
        }
    }

    pub fn try_send<T: AsBytes>(&mut self, value: &T) -> Result<(), SendError> {
        let bytes = value.as_bytes();
        {
            let socket = self.socket();
            if socket.state() == tcp::State::Closed {
                return Err(SendError::Closed);
            }
            if !socket.is_active() {
                return Err(SendError::NotEstablished);
            }
            // Only enqueue if the whole value fits; a partial send followed by
            // a retry would duplicate the already-sent prefix in the TCP stream.
            if socket.send_capacity() < bytes.len() {
                return Err(SendError::WouldBlock);
            }
        }
        self.socket_mut().send_slice(bytes).unwrap_or(0);
        Ok(())
    }

    pub fn send_blocking<T: AsBytes>(
        &mut self,
        value: &T,
        timeout: Duration,
    ) -> Result<(), SendError> {
        let deadline = self.timer.now() + timeout;
        loop {
            self.poll();
            match self.try_send(value) {
                Ok(()) => return Ok(()),
                Err(SendError::WouldBlock) if self.timer.now() < deadline => continue,
                Err(SendError::WouldBlock) => return Err(SendError::WouldBlock),
                Err(e) => return Err(e),
            }
        }
    }

    pub fn recv(&mut self, buffer: &mut [u8]) -> usize {
        let socket = self.socket_mut();
        if socket.can_recv() {
            socket.recv_slice(buffer).unwrap_or(0)
        } else {
            0
        }
    }

    pub fn try_recv_bytes(&mut self, buffer: &mut [u8]) -> Result<usize, RecvError> {
        let socket = self.socket_mut();
        if socket.state() == tcp::State::Closed {
            return Err(RecvError::Closed);
        }
        if !socket.is_active() {
            return Err(RecvError::NotEstablished);
        }
        if socket.can_recv() {
            let n = socket.recv_slice(buffer).unwrap_or(0);
            if n > 0 {
                Ok(n)
            } else {
                Err(RecvError::WouldBlock)
            }
        } else {
            Err(RecvError::WouldBlock)
        }
    }

    pub fn recv_blocking_bytes(
        &mut self,
        buffer: &mut [u8],
        timeout: Duration,
    ) -> Result<usize, RecvError> {
        let deadline = self.timer.now() + timeout;
        loop {
            self.poll();
            match self.try_recv_bytes(buffer) {
                Ok(n) => return Ok(n),
                Err(RecvError::WouldBlock) if self.timer.now() < deadline => continue,
                Err(RecvError::WouldBlock) => return Err(RecvError::WouldBlock),
                Err(e) => return Err(e),
            }
        }
    }

    pub fn try_recv<T>(&mut self) -> Result<T, RecvError>
    where
        T: FromBytes + FromZeroes + AsBytes,
    {
        let size = core::mem::size_of::<T>();
        {
            let socket = self.socket();
            if socket.state() == tcp::State::Closed {
                return Err(RecvError::Closed);
            }
            if !socket.is_active() {
                return Err(RecvError::NotEstablished);
            }
            // Only consume bytes once a complete T has arrived; a partial read
            // would permanently corrupt the TCP stream on retry.
            if socket.recv_queue() < size {
                return Err(RecvError::WouldBlock);
            }
        }
        let mut buffer = T::new_zeroed();
        self.socket_mut()
            .recv_slice(buffer.as_bytes_mut())
            .unwrap_or(0);
        Ok(buffer)
    }

    pub fn recv_blocking<T>(&mut self, timeout: Duration) -> Result<T, RecvError>
    where
        T: FromBytes + FromZeroes + AsBytes,
    {
        let deadline = self.timer.now() + timeout;
        loop {
            self.poll();
            match self.try_recv::<T>() {
                Ok(value) => return Ok(value),
                Err(RecvError::WouldBlock) if self.timer.now() < deadline => continue,
                Err(RecvError::WouldBlock) => return Err(RecvError::WouldBlock),
                Err(e) => return Err(e),
            }
        }
    }

    pub fn close(&mut self) {
        debug!("closing TCP connection (state: {:?})", self.state());
        self.socket_mut().close();
    }
}

pub fn to_smoltcp_instant(instant: Instant) -> smoltcp::time::Instant {
    smoltcp::time::Instant::from_micros(instant.micros() as i64)
}
