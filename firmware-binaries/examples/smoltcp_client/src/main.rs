// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]
#![no_main]
#![allow(unused_mut)]
#![allow(clippy::collapsible_if)]
#![feature(sync_unsafe_cell)]

use bittide_hal::manual_additions::timer::{Duration, Instant};
use bittide_hal::shared::devices::timer::Timer;
use bittide_hal::shared::devices::uart::Uart;
use bittide_sys::axi::{AxiRx, AxiTx};
use bittide_sys::dna_port_e2::{dna_to_u128, DnaValue};
use bittide_sys::mac::MacStatus;
use bittide_sys::smoltcp::axi::AxiEthernet;
use bittide_sys::smoltcp::{set_local, set_unicast};
use bittide_sys::uart::log::LOGGER;
use log::{debug, info, LevelFilter};

#[cfg(not(test))]
use riscv_rt::entry;

use riscv::register::{mcause, mepc, mtval};
use smoltcp::iface::{Config, Interface, SocketSet, SocketStorage};
use smoltcp::phy::Medium;
use smoltcp::socket::dhcpv4;
use smoltcp::socket::tcp::{Socket, SocketBuffer};
use smoltcp::wire::{EthernetAddress, IpAddress, IpCidr};
use ufmt::uwriteln;
const TIMER_ADDR: *mut u8 = (0b0011 << 28) as *mut u8;
const DNA_ADDR: *const DnaValue = (0b0111 << 28) as *const DnaValue;
const MAC_ADDR: *const MacStatus = (0b1001 << 28) as *const MacStatus;
const RX_AXI_ADDR: *const () = (0b0101 << 28) as *const ();
const TX_AXI_ADDR: *const () = (0b0110 << 28) as *const ();
const UART_ADDR: *mut u8 = (0b0010 << 28) as *mut u8;

const RX_BUFFER_SIZE: usize = 2048;
const ETH_MTU: usize = RX_BUFFER_SIZE;
const SOFT_BUFFER_SIZE: usize = 1024 * 8;
const CHUNK_SIZE: usize = 4096;

const SERVER_IP: IpAddress = IpAddress::v4(10, 0, 0, 1);
const SERVER_PORT: u16 = 1234;

gdb_trace::gdb_panic! {
    unsafe { Uart::new(UART_ADDR) }
}

#[allow(dead_code)]
fn to_smoltcp_duration(duration: Duration) -> smoltcp::time::Duration {
    smoltcp::time::Duration::from_micros(duration.micros())
}

fn from_smoltcp_duration(smoltcp_duration: smoltcp::time::Duration) -> Duration {
    Duration::from_micros(smoltcp_duration.micros())
}

fn to_smoltcp_instant(instant: Instant) -> smoltcp::time::Instant {
    smoltcp::time::Instant::from_micros(instant.micros() as i64)
}

#[allow(dead_code)]
fn from_smoltcp_instant(smoltcp_instant: smoltcp::time::Instant) -> Instant {
    Instant::from_micros(smoltcp_instant.micros() as u64)
}

// See https://github.com/bittide/bittide-hardware/issues/681
#[allow(static_mut_refs)]
#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize peripherals
    let mut uart = unsafe { Uart::new(UART_ADDR) };
    let mut timer = unsafe { Timer::new(TIMER_ADDR) };
    let axi_tx = unsafe { AxiTx::new(TX_AXI_ADDR) };
    let axi_rx: AxiRx<RX_BUFFER_SIZE> = unsafe { AxiRx::new(RX_AXI_ADDR) };
    let dna = unsafe { dna_to_u128(*DNA_ADDR) };

    uwriteln!(uart, "Starting TCP Client").unwrap();
    unsafe {
        let logger = &mut (*LOGGER.get());
        logger.set_logger(uart.clone());
        let mut log_timer = Timer::new(TIMER_ADDR);
        logger.set_timer(log_timer);
        logger.display_source = LevelFilter::Warn;
        log::set_logger_racy(logger).ok();
        log::set_max_level_racy(LevelFilter::Trace);
    }

    // Configure interface
    let mut eth_addr = EthernetAddress::from_bytes(&dna.to_le_bytes()[0..6]);
    set_unicast(&mut eth_addr);
    set_local(&mut eth_addr);
    let mut config = Config::new(eth_addr.into());
    let mut eth: AxiEthernet<ETH_MTU> = AxiEthernet::new(Medium::Ethernet, axi_rx, axi_tx, None);
    let now = to_smoltcp_instant(timer.now());
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
    info!(
        "{}, TCP Server send chunks of {} bytes for {}",
        timer.now(),
        CHUNK_SIZE,
        stress_test_duration
    );
    loop {
        let elapsed = to_smoltcp_instant(timer.now());
        iface.poll(elapsed, &mut eth, &mut sockets);
        let dhcp_socket = sockets.get_mut::<dhcpv4::Socket>(dhcp_handle);
        update_dhcp(&mut iface, dhcp_socket);
        if iface.ip_addrs().is_empty() {
            continue;
        }

        if my_ip.is_none() {
            my_ip = iface.ipv4_addr();
            info!("{}, IP address: {}", timer.now(), my_ip.unwrap());
            let now = timer.now();
            stress_test_end = now + stress_test_duration;
            info!("{}, Stress test will end at {}", now, stress_test_end);
        }

        let mut socket = sockets.get_mut::<Socket>(client_handle);
        let cx = iface.context();
        if !socket.is_open() {
            debug!("{}, Opening socket", timer.now());
            if !socket.is_active() {
                mac_status = unsafe { MAC_ADDR.read_volatile() };
                debug!(
                    "Connecting from {:?}:{} to {}:{}",
                    my_ip.unwrap().octets(),
                    1234,
                    SERVER_IP,
                    SERVER_PORT
                );
                match socket.connect(cx, (SERVER_IP, SERVER_PORT), 1234) {
                    Ok(_) => debug!("Connected to {SERVER_IP}:{SERVER_PORT}"),
                    Err(e) => debug!("Error connecting: {:?}", e),
                }
            }
        }
        if socket.can_send() {
            debug!("Sending data");
            match socket.send_slice(&[0; CHUNK_SIZE]) {
                Ok(n) => debug!("Sent {n} bytes"),
                Err(e) => debug!("Error sending data: {:?}", e),
            }
            let now = timer.now();
            if now > stress_test_end {
                info!("{}, Stress test complete", now);
                socket.close();
                let new_mac_status = unsafe { MAC_ADDR.read_volatile() };
                uwriteln!(uart, "{:?}", new_mac_status - mac_status).unwrap();
            }
        }
        match iface.poll_delay(to_smoltcp_instant(timer.now()), &sockets) {
            Some(smoltcp::time::Duration::ZERO) => {}
            Some(smoltcp_delay) => {
                let delay = from_smoltcp_duration(smoltcp_delay);
                debug!("sleeping for {} ms", delay);
                timer.wait(delay);
                debug!("done sleeping");
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
        info!("mcause: {:?}\n", mcause::read());
        info!("mepc: {:?}\n", mepc::read());
        info!("mtval: {:?}\n", mtval::read());
    });
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
