// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]
#![feature(sync_unsafe_cell)]

use bittide_hal::manual_additions::ringbuffer_test::ringbuffers::AlignedReceiveBuffer;
use bittide_hal::manual_additions::timer::Instant;
use bittide_hal::ringbuffer_test::DeviceInstances;
use bittide_sys::net_state::{Manager, NetMedium, Subordinate, UgnEdge, UgnReport};
use bittide_sys::smoltcp::ringbuffer::RingbufferDevice;
use core::fmt::Write;
use log::{info, trace, LevelFilter};
use smoltcp::iface::{Config, Interface, SocketHandle, SocketSet, SocketStorage};
use smoltcp::socket::tcp;
use smoltcp::wire::{HardwareAddress, IpAddress, IpCidr};
use ufmt::uwriteln;

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

const SERVER_PORT: u16 = 8080;
const CLIENT_PORT: u16 = 49152;

fn to_smoltcp_instant(instant: Instant) -> smoltcp::time::Instant {
    smoltcp::time::Instant::from_micros(instant.micros() as i64)
}

struct SmoltcpManagerMedium<'a, 'b> {
    iface: &'a mut Interface,
    sockets: &'a mut SocketSet<'b>,
    client_handle: SocketHandle,
}

impl<'a, 'b> SmoltcpManagerMedium<'a, 'b> {
    fn new(
        iface: &'a mut Interface,
        sockets: &'a mut SocketSet<'b>,
        client_handle: SocketHandle,
    ) -> Self {
        Self {
            iface,
            sockets,
            client_handle,
        }
    }
}

impl NetMedium for SmoltcpManagerMedium<'_, '_> {
    fn phy_ready(&self, _link: usize) -> bool {
        true
    }

    fn setup_interface(&mut self, _link: usize, local_ip: [u8; 4]) {
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

    fn connect(&mut self, _link: usize, peer_ip: [u8; 4]) -> bool {
        let client = self.sockets.get_mut::<tcp::Socket>(self.client_handle);
        if !client.is_open() && !client.is_active() {
            let cx = self.iface.context();
            let _ = client.connect(
                cx,
                (
                    IpAddress::v4(peer_ip[0], peer_ip[1], peer_ip[2], peer_ip[3]),
                    SERVER_PORT,
                ),
                CLIENT_PORT,
            );
        }
        client.is_active()
    }

    fn listen(&mut self, _link: usize) -> bool {
        false
    }

    fn send(&mut self, _link: usize, data: &[u8]) -> bool {
        let client = self.sockets.get_mut::<tcp::Socket>(self.client_handle);
        if client.can_send() {
            let _ = client.send_slice(data);
            return true;
        }
        false
    }

    fn recv(&mut self, _link: usize, buf: &mut [u8]) -> Option<usize> {
        let client = self.sockets.get_mut::<tcp::Socket>(self.client_handle);
        if client.can_recv() {
            if let Ok(len) = client.recv_slice(buf) {
                return Some(len);
            }
        }
        None
    }

    fn timed_out(&self, _link: usize) -> bool {
        false
    }
}

struct SmoltcpSubordinateMedium<'a, 'b> {
    iface: &'a mut Interface,
    sockets: &'a mut SocketSet<'b>,
    server_handle: SocketHandle,
}

impl<'a, 'b> SmoltcpSubordinateMedium<'a, 'b> {
    fn new(
        iface: &'a mut Interface,
        sockets: &'a mut SocketSet<'b>,
        server_handle: SocketHandle,
    ) -> Self {
        Self {
            iface,
            sockets,
            server_handle,
        }
    }
}

impl NetMedium for SmoltcpSubordinateMedium<'_, '_> {
    fn phy_ready(&self, _link: usize) -> bool {
        true
    }

    fn setup_interface(&mut self, _link: usize, local_ip: [u8; 4]) {
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

    fn connect(&mut self, _link: usize, _peer_ip: [u8; 4]) -> bool {
        false
    }

    fn listen(&mut self, _link: usize) -> bool {
        let server = self.sockets.get_mut::<tcp::Socket>(self.server_handle);
        if !server.is_open() {
            let _ = server.listen(SERVER_PORT);
        }
        server.is_open()
    }

    fn send(&mut self, _link: usize, data: &[u8]) -> bool {
        let server = self.sockets.get_mut::<tcp::Socket>(self.server_handle);
        if server.can_send() {
            let _ = server.send_slice(data);
            return true;
        }
        false
    }

    fn recv(&mut self, _link: usize, buf: &mut [u8]) -> Option<usize> {
        let server = self.sockets.get_mut::<tcp::Socket>(self.server_handle);
        if server.can_recv() {
            if let Ok(len) = server.recv_slice(buf) {
                return Some(len);
            }
        }
        None
    }

    fn timed_out(&self, _link: usize) -> bool {
        false
    }
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    let timer = INSTANCES.timer;

    // Set up logging
    unsafe {
        use bittide_sys::uart::log::LOGGER;
        let logger = &mut (*LOGGER.get());
        logger.set_logger(uart.clone());
        logger.set_timer(INSTANCES.timer);
        logger.display_source = LevelFilter::Warn;
        log::set_logger_racy(logger).ok();
        log::set_max_level_racy(LevelFilter::Trace);
    }

    info!("=== Ringbuffer smoltcp Loopback Test ===");

    // Set up ringbuffers
    info!("Step 1: Finding ringbuffer alignment...");
    let tx_buffer = INSTANCES.transmit_ringbuffer;
    let rx_buffer = INSTANCES.receive_ringbuffer;
    let mut rx_aligned = AlignedReceiveBuffer::new(rx_buffer);
    rx_aligned.align(&tx_buffer);
    let rx_offset = rx_aligned
        .get_alignment_offset()
        .expect("Failed to find RX buffer alignment");
    trace!("  Alignment offset: {}", rx_offset);

    // Step 2: Create smoltcp device
    info!("Step 2: Creating RingbufferDevice...");
    let mut device = RingbufferDevice::new(rx_aligned, tx_buffer);
    let mtu = device.mtu();
    trace!("  MTU: {} bytes", mtu);

    // Step 3: Configure network interface
    info!("Step 3: Configuring network interface...");
    let hw_addr = HardwareAddress::Ip;
    let config = Config::new(hw_addr);
    let now = to_smoltcp_instant(timer.now());
    let mut iface = Interface::new(config, &mut device, now);
    iface.update_ip_addrs(|addrs| {
        addrs.clear();
    });

    // Step 4: Create TCP sockets
    info!("Step 4: Creating TCP sockets...");

    // Server socket - reduced buffer sizes to fit in memory
    static mut SERVER_RX_BUF: [u8; 256] = [0; 256];
    static mut SERVER_TX_BUF: [u8; 256] = [0; 256];
    let server_rx_buffer = tcp::SocketBuffer::new(unsafe { &mut SERVER_RX_BUF[..] });
    let server_tx_buffer = tcp::SocketBuffer::new(unsafe { &mut SERVER_TX_BUF[..] });
    let server_socket = tcp::Socket::new(server_rx_buffer, server_tx_buffer);

    // Client socket - reduced buffer sizes to fit in memory
    static mut CLIENT_RX_BUF: [u8; 256] = [0; 256];
    static mut CLIENT_TX_BUF: [u8; 256] = [0; 256];
    let client_rx_buffer = tcp::SocketBuffer::new(unsafe { &mut CLIENT_RX_BUF[..] });
    let client_tx_buffer = tcp::SocketBuffer::new(unsafe { &mut CLIENT_TX_BUF[..] });
    let client_socket = tcp::Socket::new(client_rx_buffer, client_tx_buffer);

    let mut sockets_storage: [SocketStorage; 2] = Default::default();
    let mut sockets = SocketSet::new(&mut sockets_storage[..]);
    let server_handle = sockets.add(server_socket);
    let client_handle = sockets.add(client_socket);

    // Step 5: Initialize link state machines
    info!("Step 5: Initializing link state machines...");

    // Main event loop
    info!("Step 7: Running main event loop...");
    let mut done_logged = false;

    let mut manager = Manager::new();
    let mut subordinate = Subordinate::new();
    subordinate.set_report(build_placeholder_report());

    for _ in 0..1000 {
        let timestamp = to_smoltcp_instant(timer.now());
        iface.poll(timestamp, &mut device, &mut sockets);

        let mut manager_medium = SmoltcpManagerMedium::new(&mut iface, &mut sockets, client_handle);
        manager.step(&mut manager_medium, 0);

        let mut subordinate_medium =
            SmoltcpSubordinateMedium::new(&mut iface, &mut sockets, server_handle);
        subordinate.step(&mut subordinate_medium, 0);

        if manager.is_done() && subordinate.is_done() && !done_logged {
            info!("  Manager collected UGN report");
            done_logged = true;
            break;
        }
    }

    // Verify results
    info!("Step 8: Verifying results...");

    if !done_logged {
        info!("  FAILURE: UGN report timeout!");
    } else if let Some(report) = manager.report() {
        info!("  SUCCESS: Manager received {} UGN edges", report.count);
        for (idx, edge) in report.edges.iter().enumerate() {
            if idx >= report.count as usize {
                break;
            }
            info!(
                "  Edge {}: {}:{} -> {}:{}, ugn={}, valid={}",
                idx,
                edge.src_node,
                edge.src_port,
                edge.dst_node,
                edge.dst_port,
                edge.ugn,
                edge.is_valid
            );
        }
    } else {
        info!("  FAILURE: Missing UGN report data!");
    }

    uwriteln!(uart, "=== Test Complete ===").unwrap();

    loop {
        continue;
    }
}

fn build_placeholder_report() -> UgnReport {
    let mut report = UgnReport {
        count: 2,
        ..Default::default()
    };
    report.edges[0] = UgnEdge {
        src_node: 1,
        src_port: 0,
        dst_node: 0,
        dst_port: 0,
        ugn: 123,
        is_valid: 1,
        _padding: [0; 7],
    };
    report.edges[1] = UgnEdge {
        src_node: 1,
        src_port: 1,
        dst_node: 0,
        dst_port: 1,
        ugn: 456,
        is_valid: 1,
        _padding: [0; 7],
    };
    report
}

#[cfg(not(test))]
#[panic_handler]
fn panic(info: &core::panic::PanicInfo) -> ! {
    let mut uart = INSTANCES.uart;
    writeln!(uart, "PANIC: {}", info).ok();
    loop {}
}
