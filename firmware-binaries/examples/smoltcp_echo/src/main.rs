// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![cfg_attr(not(feature = "std"), no_std)]
#![allow(unused_mut)]
#![allow(clippy::collapsible_if)]
#![no_main]

use bittide_sys::axi::{AxiRx, AxiTx};
use bittide_sys::dna_port_e2::{dna_to_u128, DnaValue};
use bittide_sys::mac::MacStatus;
use bittide_sys::smoltcp::axi::AxiEthernet;
use bittide_sys::smoltcp::{set_local, set_unicast};
use bittide_sys::time::{Clock, Duration};
use bittide_sys::uart::log::LOGGER;
use bittide_sys::uart::Uart;
use log::{debug, info, LevelFilter};

#[cfg(not(test))]
use riscv_rt::entry;

use core::cmp::min;
use smoltcp::iface::{Config, Interface, SocketSet, SocketStorage};
use smoltcp::phy::Medium;
use smoltcp::socket::dhcpv4;
use smoltcp::socket::tcp::{Socket, SocketBuffer};
use smoltcp::wire::{EthernetAddress, IpCidr};
use ufmt::uwriteln;

const CLOCK_ADDR: *const () = (0b0011 << 28) as *const ();
const DNA_ADDR: *const DnaValue = (0b0111 << 28) as *const DnaValue;
const MAC_ADDR: *const MacStatus = (0b1001 << 28) as *const MacStatus;
const RX_AXI_ADDR: *const () = (0b0101 << 28) as *const ();
const TX_AXI_ADDR: *const () = (0b0110 << 28) as *const ();
const UART_ADDR: *const () = (0b0010 << 28) as *const ();

const RX_BUFFER_SIZE: usize = 2048;
const ETH_MTU: usize = RX_BUFFER_SIZE;
const TCP_MTU: usize = ETH_MTU - 40;
const SOFT_BUFFER_SIZE: usize = 1024 * 8;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize and test UART.
    let mut uart = unsafe { Uart::new(UART_ADDR) };

    uwriteln!(uart, "Starting smoltcp-echo").unwrap();
    unsafe {
        LOGGER.set_logger(uart.clone());
        log::set_logger_racy(&LOGGER).ok();
        log::set_max_level_racy(log::LevelFilter::Trace);
    }

    // Initialize and test clock
    let mut clock = unsafe { Clock::new(CLOCK_ADDR) };

    // Create interface
    let dna = unsafe { dna_to_u128(*DNA_ADDR) };
    let mut eth_addr = EthernetAddress::from_bytes(&dna.to_le_bytes()[0..6]);
    set_unicast(&mut eth_addr);
    set_local(&mut eth_addr);
    let mut config = Config::new(eth_addr.into());

    let axi_tx = unsafe { AxiTx::new(TX_AXI_ADDR) };
    let axi_rx: AxiRx<RX_BUFFER_SIZE> = unsafe { AxiRx::new(RX_AXI_ADDR) };
    let mut eth: AxiEthernet<ETH_MTU> = AxiEthernet::new(Medium::Ethernet, axi_rx, axi_tx, None);
    let now = clock.elapsed().into();
    let mut iface = Interface::new(config, &mut eth, now);

    // Create sockets
    let mut dhcp_socket = dhcpv4::Socket::new();
    let server_socket = {
        // It is not strictly necessary to use a `static mut` and unsafe code here, but
        // on embedded systems that smoltcp targets it is far better to allocate the data
        // statically to verify that it fits into RAM rather than get undefined behavior
        // when stack overflows.
        static mut TCP_SERVER_RX_DATA: [u8; SOFT_BUFFER_SIZE] = [0; SOFT_BUFFER_SIZE];
        static mut TCP_SERVER_TX_DATA: [u8; SOFT_BUFFER_SIZE] = [0; SOFT_BUFFER_SIZE];
        let tcp_rx_buffer = SocketBuffer::new(unsafe { &mut TCP_SERVER_RX_DATA[..] });
        let tcp_tx_buffer = SocketBuffer::new(unsafe { &mut TCP_SERVER_TX_DATA[..] });
        Socket::new(tcp_rx_buffer, tcp_tx_buffer)
    };

    let mut sockets: [SocketStorage; 2] = Default::default();
    let mut sockets = SocketSet::new(&mut sockets[..]);
    let server_handle = sockets.add(server_socket);
    let dhcp_handle = sockets.add(dhcp_socket);

    let mut mac_status = unsafe { MAC_ADDR.read_volatile() };

    loop {
        let elapsed = clock.elapsed().into();
        iface.poll(elapsed, &mut eth, &mut sockets);
        let dhcp_socket = sockets.get_mut::<dhcpv4::Socket>(dhcp_handle);
        update_dhcp(&mut iface, dhcp_socket);
        if iface.ip_addrs().is_empty() {
            continue;
        }

        let mut socket = sockets.get_mut::<Socket>(server_handle);
        if !socket.is_active() && !socket.is_listening() {
            mac_status = unsafe { MAC_ADDR.read_volatile() };
            uwriteln!(uart, "listening").unwrap();
            socket.listen(7).unwrap();
        }
        if socket.is_open() && socket.may_send() && !socket.may_recv() {
            socket.close();
            uwriteln!(uart, "DNA: {:X}", dna).unwrap();
            let new_mac_status = unsafe { MAC_ADDR.read_volatile() };
            uwriteln!(uart, "{:?}", new_mac_status - mac_status).unwrap();
            uwriteln!(uart, "Closing socket").unwrap();
        }
        if socket.can_recv() {
            let mut buf = [0; TCP_MTU];
            let mut slice: &[u8] = &[];
            socket
                .recv(|buffer| {
                    let elements = min(TCP_MTU, buffer.len());
                    buf[..elements].copy_from_slice(&buffer[..elements]);
                    slice = &buf[0..elements];
                    (elements, ())
                })
                .unwrap();

            debug!("Echoing {} bytes", slice.len());
            socket.send_slice(slice).unwrap();
        }

        match iface.poll_delay(clock.elapsed().into(), &sockets) {
            Some(smoltcp::time::Duration::ZERO) => {}
            Some(delay) => {
                debug!("sleeping for {} ms", delay);
                clock.wait(Duration::from(delay));
            }
            None => {}
        }
    }
}

#[export_name = "ExceptionHandler"]
fn exception_handler(_trap_frame: &riscv_rt::TrapFrame) -> ! {
    let mut uart = unsafe { Uart::new(UART_ADDR) };
    riscv::interrupt::free(|| {
        uwriteln!(uart, "... caught an exception. Looping forever now.\n").unwrap();
    });
    loop {
        continue;
    }
}

#[panic_handler]
fn panic_handler(info: &core::panic::PanicInfo) -> ! {
    let mut uart = unsafe { Uart::new(UART_ADDR) };

    uwriteln!(uart, "Panicked!").unwrap();
    debug!("{}", info);
    uwriteln!(uart, "Looping forever now").unwrap();
    loop {
        continue;
    }
}

fn update_dhcp(iface: &mut Interface, socket: &mut dhcpv4::Socket) {
    let event = socket.poll();
    match event {
        None => {}
        Some(dhcpv4::Event::Configured(config)) => {
            debug!("DHCP config acquired!");

            debug!("IP address:      {}", config.address);
            iface.update_ip_addrs(|addrs| {
                addrs.clear();
                addrs.push(IpCidr::Ipv4(config.address)).unwrap();
            });

            if let Some(router) = config.router {
                debug!("Default gateway: {}", router);
                iface.routes_mut().add_default_ipv4_route(router).unwrap();
            } else {
                debug!("Default gateway: None");
                iface.routes_mut().remove_default_ipv4_route();
            }

            for (i, s) in config.dns_servers.iter().enumerate() {
                debug!("DNS server {}:    {}", i, s);
            }
        }
        Some(dhcpv4::Event::Deconfigured) => {
            debug!("DHCP lost config!");
            iface.update_ip_addrs(|addrs| addrs.clear());
            iface.routes_mut().remove_default_ipv4_route();
        }
    }
}
