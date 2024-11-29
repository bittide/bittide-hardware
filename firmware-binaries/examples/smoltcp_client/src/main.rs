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
use bittide_sys::time::{Clock, Duration, Instant};
use bittide_sys::uart::Uart;
use core::fmt::Write;

#[cfg(not(test))]
use riscv_rt::entry;

use riscv::register::{mcause, mepc, mtval};
use smoltcp::iface::{Config, Interface, SocketSet, SocketStorage};
use smoltcp::phy::Medium;
use smoltcp::socket::dhcpv4;
use smoltcp::socket::tcp::{Socket, SocketBuffer};
use smoltcp::wire::{EthernetAddress, IpAddress, IpCidr};
use ufmt::uwriteln;
const CLOCK_ADDR: *const () = (0b0011 << 28) as *const ();
const DNA_ADDR: *const DnaValue = (0b0111 << 28) as *const DnaValue;
const MAC_ADDR: *const MacStatus = (0b1001 << 28) as *const MacStatus;
const RX_AXI_ADDR: *const () = (0b0101 << 28) as *const ();
const TX_AXI_ADDR: *const () = (0b0110 << 28) as *const ();
const UART_ADDR: *const () = (0b0010 << 28) as *const ();

const RX_BUFFER_SIZE: usize = 2048;
const ETH_MTU: usize = RX_BUFFER_SIZE;
const SOFT_BUFFER_SIZE: usize = 1024 * 8;
const CHUNK_SIZE: usize = 4096;

const SERVER_IP: IpAddress = IpAddress::v4(10, 0, 0, 1);
const SERVER_PORT: u16 = 1234;

// See https://github.com/bittide/bittide-hardware/issues/681
#[allow(static_mut_refs)]
#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals
    let mut uart = unsafe { Uart::new(UART_ADDR) };
    let mut clock = unsafe { Clock::new(CLOCK_ADDR) };
    let axi_tx = unsafe { AxiTx::new(TX_AXI_ADDR) };
    let axi_rx: AxiRx<RX_BUFFER_SIZE> = unsafe { AxiRx::new(RX_AXI_ADDR) };
    let dna = unsafe { dna_to_u128(*DNA_ADDR) };

    uwriteln!(uart, "Starting TCP Client").unwrap();
    // Configure interface
    let mut eth_addr = EthernetAddress::from_bytes(&dna.to_le_bytes()[0..6]);
    set_unicast(&mut eth_addr);
    set_local(&mut eth_addr);
    let mut config = Config::new(eth_addr.into());
    let mut eth: AxiEthernet<ETH_MTU> = AxiEthernet::new(Medium::Ethernet, axi_rx, axi_tx, None);
    let now = clock.now().into();
    let mut iface = Interface::new(config, &mut eth, now);

    // Create sockets
    let mut dhcp_socket = dhcpv4::Socket::new();
    let client_socket = {
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
    let client_handle = sockets.add(client_socket);
    let dhcp_handle = sockets.add(dhcp_socket);

    let mut mac_status = unsafe { MAC_ADDR.read_volatile() };
    let mut my_ip = None;

    let stress_test_duration = Duration::from_secs(30);
    let mut stress_test_end = Instant::end_of_time();
    uwriteln!(
        uart,
        "TCP Server send chunks of {} bytes for {}",
        CHUNK_SIZE,
        stress_test_duration
    )
    .unwrap();
    loop {
        let elapsed = clock.now().into();
        iface.poll(elapsed, &mut eth, &mut sockets);

        let dhcp_socket = sockets.get_mut::<dhcpv4::Socket>(dhcp_handle);
        update_dhcp(&mut iface, dhcp_socket);
        if iface.ip_addrs().is_empty() {
            continue;
        }

        if my_ip.is_none() {
            my_ip = iface.ipv4_addr();
            writeln!(uart, "IP address: {}", my_ip.unwrap()).unwrap();
            let now = clock.now();
            stress_test_end = now + stress_test_duration;
            uwriteln!(uart, "Stress test will end at {}", stress_test_end).unwrap();
        }

        let mut socket = sockets.get_mut::<Socket>(client_handle);
        let cx = iface.context();
        if !socket.is_open() {
            if !socket.is_active() {
                mac_status = unsafe { MAC_ADDR.read_volatile() };
                writeln!(
                    uart,
                    "Connecting from {:?}:{} to {}:{}",
                    my_ip.unwrap().as_bytes(),
                    1234,
                    SERVER_IP,
                    SERVER_PORT
                )
                .unwrap();
                match socket.connect(cx, (SERVER_IP, SERVER_PORT), 1234) {
                    Ok(_) => writeln!(uart, "Connected to {SERVER_IP}:{SERVER_PORT}").unwrap(),
                    Err(e) => writeln!(uart, "Error connecting: {:?}", e).unwrap(),
                }
            }
        }
        if socket.can_send() {
            let now = clock.now();
            if now > stress_test_end {
                uwriteln!(uart, "Stress test complete").unwrap();
                socket.close();
                let new_mac_status = unsafe { MAC_ADDR.read_volatile() };
                uwriteln!(uart, "{:?}", new_mac_status - mac_status).unwrap();
            }
        }
        match iface.poll_delay(clock.now().into(), &sockets) {
            Some(smoltcp::time::Duration::ZERO) => {}
            Some(delay) => {
                let smoltcp_delay = Duration::from(delay);
                uwriteln!(uart, "sleeping for {} ms", smoltcp_delay).unwrap();
                clock.wait(smoltcp_delay);
                uwriteln!(uart, "done sleeping").unwrap();
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
        writeln!(uart, "mcause: {:?}\n", mcause::read()).unwrap();
        writeln!(uart, "mepc: {:?}\n", mepc::read()).unwrap();
        writeln!(uart, "mtval: {:?}\n", mtval::read()).unwrap();
    });
    loop {
        continue;
    }
}

#[panic_handler]
fn panic_handler(info: &core::panic::PanicInfo) -> ! {
    let mut uart = unsafe { Uart::new(UART_ADDR) };

    uwriteln!(uart, "Panicked!").unwrap();
    writeln!(uart, "{}\n", info).unwrap();
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
            iface.update_ip_addrs(|addrs| {
                addrs.clear();
                addrs.push(IpCidr::Ipv4(config.address)).unwrap();
            });

            if let Some(router) = config.router {
                iface.routes_mut().add_default_ipv4_route(router).unwrap();
            } else {
                iface.routes_mut().remove_default_ipv4_route();
            }
        }
        Some(dhcpv4::Event::Deconfigured) => {
            iface.update_ip_addrs(|addrs| addrs.clear());
            iface.routes_mut().remove_default_ipv4_route();
        }
    }
}
