// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! TCP communication layer over aligned ring_buffers.
//!
//! [`LinkInterface`] wraps a smoltcp TCP socket and a
//! [`RingBufferDevice`](super::ring_buffer::RingBufferDevice) into a
//! single abstraction for exchanging data with a direct neighbor.
//!
//! Both sides use TCP *simultaneous open* (both connect to the same
//! IP/port), so there is no client/server distinction.
//!
//! # Two-phase construction
//!
//! ```ignore
//! let mut link = LinkInterface::<_, _, 512>::new(rx_aligned, tx_buffer, timer);
//! link.connect(); // must not move the struct after this
//! ```

use bittide_hal::manual_additions::ring_buffer::{
    AlignedReceiveBuffer, ReceiveRingBufferInterface, TransmitRingBufferInterface,
};
use bittide_hal::manual_additions::timer::{Duration, Instant};
use smoltcp::iface::{Config, Interface, SocketSet, SocketStorage};
use smoltcp::socket::tcp;
use smoltcp::wire::{HardwareAddress, IpAddress, IpCidr, IpEndpoint};
use zerocopy::{AsBytes, FromBytes, FromZeroes};

use core::mem::MaybeUninit;
use log::debug;

use crate::smoltcp::link_protocol::{WireDecode, WireEncode};
use crate::smoltcp::ring_buffer::RingBufferDevice;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecvError {
    WouldBlock,
    NotEstablished,
    Closed,
    Decode(&'static str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SendError {
    WouldBlock,
    NotEstablished,
    Closed,
}

const LINK_IP: IpAddress = IpAddress::v4(100, 100, 100, 100);
const LINK_PORT: u16 = 8080;

/// TCP communication over a pair of ring_buffers.
///
/// Construction is two-phase to allow the struct to own its TCP buffers:
/// 1. `new()` — allocates buffers, stores the device and timer
/// 2. `connect()` — initializes smoltcp and starts TCP simultaneous open
///
/// `connect()` creates internal borrows into the struct's own buffer arrays,
/// so the struct must not be moved after `connect()` is called.
pub struct LinkInterface<RxRb, TxRb, const BUF_SIZE: usize> {
    // Owned buffer storage — borrowed by smoltcp after connect()
    tcp_rx_buffer: [u8; BUF_SIZE],
    tcp_tx_buffer: [u8; BUF_SIZE],
    socket_storage: MaybeUninit<[SocketStorage<'static>; 1]>,

    // Always initialized
    device: RingBufferDevice<RxRb, TxRb>,
    timer: bittide_hal::shared_devices::Timer,

    // Initialized by connect()
    iface: MaybeUninit<Interface>,
    sockets: MaybeUninit<SocketSet<'static>>,
    socket_handle: smoltcp::iface::SocketHandle,
    last_state: tcp::State,
}

impl<RxRb, TxRb, const BUF_SIZE: usize> LinkInterface<RxRb, TxRb, BUF_SIZE>
where
    RxRb: ReceiveRingBufferInterface + 'static,
    TxRb: TransmitRingBufferInterface + 'static,
{
    pub fn new(
        rx_aligned: AlignedReceiveBuffer<RxRb, TxRb>,
        tx_buffer: TxRb,
        timer: bittide_hal::shared_devices::Timer,
    ) -> Self {
        Self {
            tcp_rx_buffer: [0; BUF_SIZE],
            tcp_tx_buffer: [0; BUF_SIZE],
            socket_storage: MaybeUninit::uninit(),
            device: RingBufferDevice::new(rx_aligned, tx_buffer),
            timer,
            iface: MaybeUninit::uninit(),
            sockets: MaybeUninit::uninit(),
            socket_handle: smoltcp::iface::SocketHandle::default(),
            last_state: tcp::State::Closed,
        }
    }

    /// Initialize smoltcp and start TCP simultaneous open.
    ///
    /// # Safety
    /// After this call the struct contains self-referential borrows.
    /// It must not be moved (e.g. via `Vec::push` or assignment).
    pub fn connect(&mut self) {
        // SAFETY: We create 'static references into our own buffer fields.
        // Sound because:
        //  - The struct is not moved after this call (caller obligation).
        //  - In embedded main() -> !, the stack frame lives forever.
        unsafe {
            self.socket_storage.write([SocketStorage::EMPTY; 1]);
            let storage: &'static mut [SocketStorage<'static>] = core::slice::from_raw_parts_mut(
                self.socket_storage.assume_init_mut().as_mut_ptr(),
                1,
            );
            let rx_slice: &'static mut [u8] =
                core::slice::from_raw_parts_mut(self.tcp_rx_buffer.as_mut_ptr(), BUF_SIZE);
            let tx_slice: &'static mut [u8] =
                core::slice::from_raw_parts_mut(self.tcp_tx_buffer.as_mut_ptr(), BUF_SIZE);

            let config = Config::new(HardwareAddress::Ip);
            let now = to_smoltcp_instant(self.timer.now());
            let mut iface = Interface::new(config, &mut self.device, now);

            iface.update_ip_addrs(|addrs| {
                addrs.push(IpCidr::new(LINK_IP, 24)).unwrap();
            });

            let socket = tcp::Socket::new(
                tcp::SocketBuffer::new(rx_slice),
                tcp::SocketBuffer::new(tx_slice),
            );

            let mut sockets = SocketSet::new(storage);
            let handle = sockets.add(socket);

            let remote = IpEndpoint::new(LINK_IP, LINK_PORT);
            sockets
                .get_mut::<tcp::Socket>(handle)
                .connect(iface.context(), remote, LINK_PORT)
                .unwrap();

            self.last_state = sockets.get::<tcp::Socket>(handle).state();
            self.socket_handle = handle;
            self.iface.write(iface);
            self.sockets.write(sockets);
        }
        debug!(
            "LinkInterface connected, initial state: {:?}",
            self.last_state
        );
    }

    fn socket(&self) -> &tcp::Socket<'static> {
        unsafe {
            self.sockets
                .assume_init_ref()
                .get::<tcp::Socket>(self.socket_handle)
        }
    }

    fn socket_mut(&mut self) -> &mut tcp::Socket<'static> {
        let handle = self.socket_handle;
        unsafe {
            self.sockets
                .assume_init_mut()
                .get_mut::<tcp::Socket>(handle)
        }
    }

    /// Drive the smoltcp state machine. Must be called regularly.
    pub fn poll(&mut self) {
        let timestamp = to_smoltcp_instant(self.timer.now());
        let iface = unsafe { self.iface.assume_init_mut() };
        let sockets = unsafe { self.sockets.assume_init_mut() };
        iface.poll(timestamp, &mut self.device, sockets);

        let handle = self.socket_handle;
        let new_state = sockets.get::<tcp::Socket>(handle).state();
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

    // -- Slice API --------------------------------------------------------------

    /// Try to send `data`; returns bytes accepted or an error.
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

    /// Blocking send with timeout. Polls internally until bytes are accepted.
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

    /// Try to receive into `buffer`; returns bytes read or an error.
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

    /// Blocking receive with timeout. Polls internally until data arrives.
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

    // -- Typed API (WireEncode / WireDecode) ---------------------------------

    /// Send a value by encoding it to its wire format (all-or-nothing).
    pub fn try_send<T: WireEncode>(&mut self, value: &T) -> Result<(), SendError> {
        let wire = value.to_wire();
        let bytes = wire.as_bytes();
        let n = self.try_send_bytes(bytes)?;
        if n == bytes.len() {
            Ok(())
        } else {
            Err(SendError::WouldBlock)
        }
    }

    /// Blocking typed send with timeout. Encodes once, then polls until accepted.
    pub fn send_blocking<T: WireEncode>(
        &mut self,
        value: &T,
        timeout: Duration,
    ) -> Result<(), SendError> {
        let wire = value.to_wire();
        let deadline = self.timer.now() + timeout;
        loop {
            self.poll();
            let bytes = wire.as_bytes();
            let n = self.try_send_bytes(bytes)?;
            if n == bytes.len() {
                return Ok(());
            }
            if self.timer.now() >= deadline {
                return Err(SendError::WouldBlock);
            }
        }
    }

    /// Receive a value by decoding it from its wire format (all-or-nothing).
    pub fn try_recv<T: WireDecode>(&mut self) -> Result<T, RecvError> {
        let wire: T::Wire = self.try_recv_raw()?;
        T::from_wire(wire).map_err(RecvError::Decode)
    }

    /// Blocking typed receive with timeout.
    pub fn recv_blocking<T: WireDecode>(&mut self, timeout: Duration) -> Result<T, RecvError> {
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

    // -- Private helpers -------------------------------------------------------

    fn try_recv_raw<W>(&mut self) -> Result<W, RecvError>
    where
        W: FromBytes + FromZeroes + AsBytes,
    {
        let mut buffer = W::new_zeroed();
        let n = self.try_recv_bytes(buffer.as_bytes_mut())?;
        if n == core::mem::size_of::<W>() {
            Ok(buffer)
        } else {
            Err(RecvError::WouldBlock)
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
