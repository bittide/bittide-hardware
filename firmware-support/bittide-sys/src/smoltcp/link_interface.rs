// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! LinkInterface: Clean abstraction for TCP communication over ringbuffers
//!
//! This module provides a high-level interface for establishing TCP connections
//! over hardware ringbuffers, hiding the complexity of smoltcp's interface,
//! socket, and device management.
//!
//! # Features
//!
//! - TCP simultaneous open for peer-to-peer connections
//! - Automatic network interface configuration
//! - Simple send/recv API for data exchange
//! - Connection state management
//! - Graceful connection closing

use bittide_hal::manual_additions::ringbuffer::{
    AlignedReceiveBuffer, ReceiveRingbufferInterface, TransmitRingbufferInterface,
};
use bittide_hal::manual_additions::timer::{Duration, Instant};
use smoltcp::iface::{Config, Interface, SocketSet, SocketStorage};
use smoltcp::socket::tcp;
use smoltcp::wire::{HardwareAddress, IpAddress, IpCidr, IpEndpoint};
use zerocopy::{AsBytes, FromBytes, FromZeroes};

use log::{debug, trace, warn};

use crate::smoltcp::ringbuffer::RingbufferDevice;

/// Error type for receive operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecvError {
    /// No data available (would block)
    WouldBlock,
    /// Connection is not established
    NotEstablished,
    /// Connection is closed
    Closed,
}

/// Error type for send operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SendError {
    /// Would block (buffer full)
    WouldBlock,
    /// Connection is not established
    NotEstablished,
    /// Connection is closed
    Closed,
}

/// Fixed IP address used for all link interfaces.
/// Each link is point-to-point with no routing, so both sides use the same IP.
const LINK_IP: IpAddress = IpAddress::v4(100, 100, 100, 100);

/// Fixed port used for TCP simultaneous open on all link interfaces.
const LINK_PORT: u16 = 8080;

/// Buffer storage for LinkInterface
pub struct LinkBuffers<'a> {
    pub socket_storage: &'a mut [SocketStorage<'a>],
    pub tcp_rx_buffer: &'static mut [u8],
    pub tcp_tx_buffer: &'static mut [u8],
}

/// LinkInterface: Clean abstraction for TCP communication over ringbuffers
pub struct LinkInterface<'a, RxRb, TxRb> {
    iface: Interface,
    device: RingbufferDevice<RxRb, TxRb>,
    sockets: SocketSet<'a>,
    socket_handle: smoltcp::iface::SocketHandle,
    timer: bittide_hal::shared_devices::Timer,
    last_state: tcp::State,
}

impl<'a, RxRb, TxRb> LinkInterface<'a, RxRb, TxRb>
where
    RxRb: ReceiveRingbufferInterface + 'static,
    TxRb: TransmitRingbufferInterface + 'static,
{
    /// Create a new LinkInterface with TCP simultaneous open.
    ///
    /// Both endpoints use the same fixed IP and port, initiating
    /// a TCP simultaneous open where both sides send SYN packets.
    /// This works because each link is a point-to-point connection
    /// with no routing.
    pub fn new(
        rx_aligned: AlignedReceiveBuffer<RxRb, TxRb>,
        tx_buffer: TxRb,
        buffers: LinkBuffers<'a>,
        timer: bittide_hal::shared_devices::Timer,
    ) -> Self {
        // Create device
        let mut device: RingbufferDevice<RxRb, TxRb> = RingbufferDevice::new(rx_aligned, tx_buffer);

        // Configure network interface
        let hw_addr = HardwareAddress::Ip;
        let iface_config = Config::new(hw_addr);
        let now = to_smoltcp_instant(timer.now());
        let mut iface = Interface::new(iface_config, &mut device, now);

        // Set IP address
        iface.update_ip_addrs(|addrs| {
            addrs.push(IpCidr::new(LINK_IP, 24)).unwrap();
        });

        // Create TCP socket
        let rx_buf = tcp::SocketBuffer::new(buffers.tcp_rx_buffer);
        let tx_buf = tcp::SocketBuffer::new(buffers.tcp_tx_buffer);
        let socket = tcp::Socket::new(rx_buf, tx_buf);

        // Create socket set
        let mut sockets = SocketSet::new(&mut buffers.socket_storage[..]);
        let socket_handle = sockets.add(socket);

        // Simultaneous open: both sides connect to the same endpoint
        let remote_endpoint = IpEndpoint::new(LINK_IP, LINK_PORT);
        let socket = sockets.get_mut::<tcp::Socket>(socket_handle);
        let cx = iface.context();
        socket
            .connect(cx, remote_endpoint, LINK_PORT)
            .map_err(|_| "Failed to initiate connection")
            .unwrap();

        let state = sockets.get::<tcp::Socket>(socket_handle).state();
        debug!("LinkInterface created, initial state: {:?}", state);

        Self {
            iface,
            device,
            sockets,
            socket_handle,
            timer,
            last_state: state,
        }
    }

    /// Poll the interface to process network events
    ///
    /// This should be called regularly in the main loop to process
    /// incoming packets, retransmissions, and connection state changes.
    pub fn poll(&mut self) {
        debug!("Polling LinkInterface (last state: {:?})", self.last_state);
        let timestamp = to_smoltcp_instant(self.timer.now());
        self.iface
            .poll(timestamp, &mut self.device, &mut self.sockets);

        let new_state = self.sockets.get::<tcp::Socket>(self.socket_handle).state();
        if new_state != self.last_state {
            debug!("TCP state: {:?} -> {:?}", self.last_state, new_state);
            self.last_state = new_state;
        }
    }

    /// Check if the TCP connection is established
    pub fn is_established(&self) -> bool {
        let socket = self.sockets.get::<tcp::Socket>(self.socket_handle);
        socket.state() == tcp::State::Established
    }

    /// Check if the socket is open (listening or connected)
    pub fn is_open(&self) -> bool {
        let socket = self.sockets.get::<tcp::Socket>(self.socket_handle);
        socket.is_open()
    }

    /// Check if the socket is active (has an active connection)
    pub fn is_active(&self) -> bool {
        let socket = self.sockets.get::<tcp::Socket>(self.socket_handle);
        socket.is_active()
    }

    /// Check if the TCP connection is closed
    pub fn is_closed(&self) -> bool {
        let socket = self.sockets.get::<tcp::Socket>(self.socket_handle);
        socket.state() == tcp::State::Closed
    }

    /// Check if the TCP connection is closing or closed
    ///
    /// Returns true if the connection has left the Established state.
    /// This is useful for checking if a close has been initiated, as
    /// the connection may go through several intermediate states
    /// (FinWait1, FinWait2, Closing, TimeWait) before reaching Closed.
    pub fn is_closing_or_closed(&self) -> bool {
        let socket = self.sockets.get::<tcp::Socket>(self.socket_handle);
        !matches!(
            socket.state(),
            tcp::State::Established | tcp::State::SynSent | tcp::State::SynReceived
        )
    }

    /// Get the current connection state
    pub fn state(&self) -> tcp::State {
        let socket = self.sockets.get::<tcp::Socket>(self.socket_handle);
        socket.state()
    }

    /// Send data over the TCP connection
    ///
    /// Simple send that returns the number of bytes sent.
    /// For error handling, use `try_send_bytes` instead.
    ///
    /// # Arguments
    ///
    /// * `data` - Data to send
    ///
    /// # Returns
    ///
    /// Returns the number of bytes sent, or 0 if unable to send
    /// (e.g., if the connection is not established or the send buffer is full).
    pub fn send(&mut self, data: &[u8]) -> usize {
        let socket = self.sockets.get_mut::<tcp::Socket>(self.socket_handle);
        if socket.can_send() {
            socket.send_slice(data).unwrap_or(0)
        } else {
            0
        }
    }

    /// Try to send raw bytes over the TCP connection without polling
    ///
    /// This checks if the send buffer has space and sends data immediately,
    /// or returns an error if unable to send or the connection is not valid.
    ///
    /// # Arguments
    ///
    /// * `data` - Data to send
    ///
    /// # Returns
    ///
    /// * `Ok(n)` - Successfully sent n bytes
    /// * `Err(SendError::WouldBlock)` - Send buffer is full
    /// * `Err(SendError::NotEstablished)` - Connection not established
    /// * `Err(SendError::Closed)` - Connection is closed
    ///
    /// # Example
    ///
    /// ```ignore
    /// match link.try_send_bytes(b"Hello") {
    ///     Ok(n) => { /* sent n bytes */ },
    ///     Err(SendError::WouldBlock) => { /* buffer full */ },
    ///     Err(SendError::Closed) => { /* connection closed */ },
    ///     _ => {},
    /// }
    /// ```
    pub fn try_send_bytes(&mut self, data: &[u8]) -> Result<usize, SendError> {
        let socket = self.sockets.get::<tcp::Socket>(self.socket_handle);

        // Check connection state
        if socket.state() == tcp::State::Closed {
            return Err(SendError::Closed);
        }

        if !socket.is_active() {
            return Err(SendError::NotEstablished);
        }

        // Try to send data
        let socket = self.sockets.get_mut::<tcp::Socket>(self.socket_handle);
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

    /// Send raw bytes over the TCP connection, polling until space is available
    ///
    /// This method internally polls the network interface until the send buffer
    /// has space or the timeout duration is reached.
    ///
    /// # Arguments
    ///
    /// * `data` - Data to send
    /// * `timeout` - Maximum duration to wait before giving up
    ///
    /// # Returns
    ///
    /// * `Ok(n)` - Successfully sent n bytes
    /// * `Err(SendError::WouldBlock)` - Timed out waiting for buffer space
    /// * `Err(SendError::NotEstablished)` - Connection not established
    /// * `Err(SendError::Closed)` - Connection is closed
    ///
    /// # Example
    ///
    /// ```ignore
    /// // Wait up to 100ms to send data
    /// match link.send_blocking_bytes(b"Hello", Duration::from_millis(100)) {
    ///     Ok(n) => info!("Sent {} bytes", n),
    ///     Err(SendError::WouldBlock) => info!("Timeout waiting to send"),
    ///     Err(e) => info!("Error: {:?}", e),
    /// }
    /// ```
    pub fn send_blocking_bytes(
        &mut self,
        data: &[u8],
        timeout: Duration,
    ) -> Result<usize, SendError> {
        let start = self.timer.now();
        let deadline = start + timeout;

        loop {
            self.poll();

            match self.try_send_bytes(data) {
                Ok(n) => {
                    trace!("send_blocking_bytes: sent {} bytes", n);
                    return Ok(n);
                }
                Err(SendError::WouldBlock) => {
                    if self.timer.now() >= deadline {
                        warn!(
                            "send_blocking_bytes: timed out sending {} bytes",
                            data.len()
                        );
                        return Err(SendError::WouldBlock);
                    }
                    continue;
                }
                Err(e) => {
                    warn!("send_blocking_bytes: error {:?}", e);
                    return Err(e);
                }
            }
        }
    }

    /// Try to send a typed value using zerocopy serialization without polling
    ///
    /// This checks if the send buffer has space, serializes the value using zerocopy,
    /// and sends it immediately. Uses zerocopy for efficient zero-copy serialization.
    ///
    /// # Type Parameters
    ///
    /// * `T` - Type to serialize, must implement `AsBytes`
    ///
    /// # Arguments
    ///
    /// * `value` - Reference to the value to send
    ///
    /// # Returns
    ///
    /// * `Ok(())` - Successfully sent the value
    /// * `Err(SendError::WouldBlock)` - Send buffer is full
    /// * `Err(SendError::NotEstablished)` - Connection not established
    /// * `Err(SendError::Closed)` - Connection is closed
    ///
    /// # Example
    ///
    /// ```ignore
    /// #[derive(AsBytes)]
    /// #[repr(C)]
    /// struct MyMessage { id: u32, value: i64 }
    ///
    /// let msg = MyMessage { id: 1, value: 42 };
    /// match link.try_send(&msg) {
    ///     Ok(()) => info!("Sent message"),
    ///     Err(SendError::WouldBlock) => { /* buffer full */ },
    ///     _ => {},
    /// }
    /// ```
    pub fn try_send<T>(&mut self, value: &T) -> Result<(), SendError>
    where
        T: AsBytes,
    {
        let bytes = value.as_bytes();
        let n = self.try_send_bytes(bytes)?;

        if n == bytes.len() {
            Ok(())
        } else {
            // Partial send - this shouldn't happen with TCP but handle it
            Err(SendError::WouldBlock)
        }
    }

    /// Send a typed value using zerocopy serialization, polling until space is available
    ///
    /// This method internally polls the network interface until the send buffer has space,
    /// then serializes and sends the value. Uses zerocopy for efficient zero-copy serialization.
    ///
    /// # Type Parameters
    ///
    /// * `T` - Type to serialize, must implement `AsBytes`
    ///
    /// # Arguments
    ///
    /// * `value` - Reference to the value to send
    /// * `timeout` - Maximum duration to wait before giving up
    ///
    /// # Returns
    ///
    /// * `Ok(())` - Successfully sent the value
    /// * `Err(SendError::WouldBlock)` - Timed out waiting for buffer space
    /// * `Err(SendError::NotEstablished)` - Connection not established
    /// * `Err(SendError::Closed)` - Connection is closed
    ///
    /// # Example
    ///
    /// ```ignore
    /// #[derive(AsBytes)]
    /// #[repr(C)]
    /// struct MyMessage { id: u32, value: i64 }
    ///
    /// let msg = MyMessage { id: 1, value: 42 };
    /// match link.send_blocking(&msg, Duration::from_millis(100)) {
    ///     Ok(()) => info!("Sent message"),
    ///     Err(SendError::WouldBlock) => info!("Timeout"),
    ///     Err(e) => info!("Error: {:?}", e),
    /// }
    /// ```
    pub fn send_blocking<T>(&mut self, value: &T, timeout: Duration) -> Result<(), SendError>
    where
        T: AsBytes,
    {
        let start = self.timer.now();
        let deadline = start + timeout;

        loop {
            self.poll();

            match self.try_send(value) {
                Ok(()) => return Ok(()),
                Err(SendError::WouldBlock) => {
                    if self.timer.now() >= deadline {
                        warn!(
                            "send_blocking: timed out sending {} bytes",
                            core::mem::size_of::<T>()
                        );
                        return Err(SendError::WouldBlock);
                    }
                    continue;
                }
                Err(e) => {
                    warn!("send_blocking: error {:?}", e);
                    return Err(e);
                }
            }
        }
    }

    /// Receive data from the TCP connection
    ///
    /// Simple receive that returns the number of bytes received.
    /// For error handling, use `try_recv_bytes` instead.
    ///
    /// # Arguments
    ///
    /// * `buffer` - Buffer to receive data into
    ///
    /// # Returns
    ///
    /// Returns the number of bytes received, or 0 if no data is available.
    pub fn recv(&mut self, buffer: &mut [u8]) -> usize {
        let socket = self.sockets.get_mut::<tcp::Socket>(self.socket_handle);
        if socket.can_recv() {
            socket.recv_slice(buffer).unwrap_or(0)
        } else {
            0
        }
    }

    /// Try to receive raw bytes from the TCP connection without polling
    ///
    /// This checks if data is available and returns it immediately,
    /// or returns an error if no data is available or the connection
    /// is not in a valid state.
    ///
    /// # Arguments
    ///
    /// * `buffer` - Buffer to receive data into
    ///
    /// # Returns
    ///
    /// * `Ok(n)` - Successfully received n bytes
    /// * `Err(RecvError::WouldBlock)` - No data available
    /// * `Err(RecvError::NotEstablished)` - Connection not established
    /// * `Err(RecvError::Closed)` - Connection is closed
    ///
    /// # Example
    ///
    /// ```ignore
    /// match link.try_recv_bytes(&mut buffer) {
    ///     Ok(n) => { /* process n bytes */ },
    ///     Err(RecvError::WouldBlock) => { /* no data yet */ },
    ///     Err(RecvError::Closed) => { /* connection closed */ },
    ///     _ => {},
    /// }
    /// ```
    pub fn try_recv_bytes(&mut self, buffer: &mut [u8]) -> Result<usize, RecvError> {
        let socket = self.sockets.get::<tcp::Socket>(self.socket_handle);

        // Check connection state
        if socket.state() == tcp::State::Closed {
            return Err(RecvError::Closed);
        }

        if !socket.is_active() {
            return Err(RecvError::NotEstablished);
        }

        // Try to receive data
        let socket = self.sockets.get_mut::<tcp::Socket>(self.socket_handle);
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

    /// Receive raw bytes from the TCP connection, polling until data arrives
    ///
    /// This method internally polls the network interface until data is available
    /// or the timeout duration is reached.
    ///
    /// # Arguments
    ///
    /// * `buffer` - Buffer to receive data into
    /// * `timeout` - Maximum duration to wait before giving up
    ///
    /// # Returns
    ///
    /// * `Ok(n)` - Successfully received n bytes
    /// * `Err(RecvError::WouldBlock)` - Timed out waiting for data
    /// * `Err(RecvError::NotEstablished)` - Connection not established
    /// * `Err(RecvError::Closed)` - Connection is closed
    ///
    /// # Example
    ///
    /// ```ignore
    /// // Wait up to 100ms for data
    /// match link.recv_blocking_bytes(&mut buffer, Duration::from_millis(100)) {
    ///     Ok(n) => info!("Received {} bytes", n),
    ///     Err(RecvError::WouldBlock) => info!("Timeout waiting for data"),
    ///     Err(e) => info!("Error: {:?}", e),
    /// }
    /// ```
    pub fn recv_blocking_bytes(
        &mut self,
        buffer: &mut [u8],
        timeout: Duration,
    ) -> Result<usize, RecvError> {
        let start = self.timer.now();
        let deadline = start + timeout;

        loop {
            self.poll();

            match self.try_recv_bytes(buffer) {
                Ok(n) => {
                    trace!("recv_blocking_bytes: received {} bytes", n);
                    return Ok(n);
                }
                Err(RecvError::WouldBlock) => {
                    if self.timer.now() >= deadline {
                        warn!("recv_blocking_bytes: timed out");
                        return Err(RecvError::WouldBlock);
                    }
                    continue;
                }
                Err(e) => {
                    warn!("recv_blocking_bytes: error {:?}", e);
                    return Err(e);
                }
            }
        }
    }

    /// Try to receive a typed value using zerocopy deserialization without polling
    ///
    /// This checks if data is available, deserializes it into the specified type,
    /// and returns it immediately. Uses zerocopy for efficient zero-copy deserialization.
    ///
    /// # Type Parameters
    ///
    /// * `T` - Type to deserialize, must implement `FromBytes` and `FromZeroes`
    ///
    /// # Returns
    ///
    /// * `Ok(T)` - Successfully received and deserialized the value
    /// * `Err(RecvError::WouldBlock)` - No data available
    /// * `Err(RecvError::NotEstablished)` - Connection not established
    /// * `Err(RecvError::Closed)` - Connection is closed
    ///
    /// # Example
    ///
    /// ```ignore
    /// #[derive(FromBytes, FromZeroes, AsBytes)]
    /// struct MyMessage { id: u32, value: i64 }
    ///
    /// match link.try_recv::<MyMessage>() {
    ///     Ok(msg) => info!("Received: id={}, value={}", msg.id, msg.value),
    ///     Err(RecvError::WouldBlock) => { /* no data yet */ },
    ///     _ => {},
    /// }
    /// ```
    pub fn try_recv<T>(&mut self) -> Result<T, RecvError>
    where
        T: FromBytes + FromZeroes + AsBytes,
    {
        let mut buffer = T::new_zeroed();
        let bytes = buffer.as_bytes_mut();
        let n = self.try_recv_bytes(bytes)?;

        if n == core::mem::size_of::<T>() {
            Ok(buffer)
        } else {
            warn!(
                "try_recv: expected {} bytes, got {}",
                core::mem::size_of::<T>(),
                n
            );
            Err(RecvError::WouldBlock)
        }
    }

    /// Receive a typed value using zerocopy deserialization, polling until it arrives
    ///
    /// This method internally polls the network interface until data is available,
    /// then deserializes it into the specified type. Uses zerocopy for efficient
    /// zero-copy deserialization.
    ///
    /// # Type Parameters
    ///
    /// * `T` - Type to deserialize, must implement `FromBytes` and `FromZeroes`
    ///
    /// # Arguments
    ///
    /// * `timeout` - Maximum duration to wait before giving up
    ///
    /// # Returns
    ///
    /// * `Ok(T)` - Successfully received and deserialized the value
    /// * `Err(RecvError::WouldBlock)` - Timed out waiting for data
    /// * `Err(RecvError::NotEstablished)` - Connection not established
    /// * `Err(RecvError::Closed)` - Connection is closed
    ///
    /// # Example
    ///
    /// ```ignore
    /// #[derive(FromBytes, FromZeroes, AsBytes)]
    /// struct MyMessage { id: u32, value: i64 }
    ///
    /// match link.recv_blocking::<MyMessage>(Duration::from_millis(100)) {
    ///     Ok(msg) => info!("Received: id={}, value={}", msg.id, msg.value),
    ///     Err(RecvError::WouldBlock) => info!("Timeout"),
    ///     Err(e) => info!("Error: {:?}", e),
    /// }
    /// ```
    pub fn recv_blocking<T>(&mut self, timeout: Duration) -> Result<T, RecvError>
    where
        T: FromBytes + FromZeroes + AsBytes,
    {
        let start = self.timer.now();
        let deadline = start + timeout;

        while self.timer.now() < deadline {
            self.poll();

            match self.try_recv::<T>() {
                Ok(value) => return Ok(value),
                Err(RecvError::WouldBlock) => continue,
                Err(e) => {
                    warn!("recv_blocking: error {:?}", e);
                    return Err(e);
                }
            }
        }
        warn!(
            "recv_blocking: timed out waiting for {} bytes",
            core::mem::size_of::<T>()
        );
        Err(RecvError::WouldBlock)
    }

    /// Close the TCP connection gracefully
    ///
    /// Initiates a graceful close of the TCP connection. The connection
    /// will go through the normal TCP close handshake (FIN exchange).
    /// Call `poll()` repeatedly and check `is_closing_or_closed()` to
    /// track the close progress.
    pub fn close(&mut self) {
        debug!("closing TCP connection (state: {:?})", self.state());
        let socket = self.sockets.get_mut::<tcp::Socket>(self.socket_handle);
        socket.close();
    }
}

/// Convert HAL Instant to smoltcp Instant
pub fn to_smoltcp_instant(instant: Instant) -> smoltcp::time::Instant {
    smoltcp::time::Instant::from_micros(instant.micros() as i64)
}
