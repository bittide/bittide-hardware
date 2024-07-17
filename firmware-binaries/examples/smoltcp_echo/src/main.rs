// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![cfg_attr(not(feature = "std"), no_std)]
#![allow(unused_mut)]
#![allow(clippy::collapsible_if)]
#![no_main]

use bittide_sys::axi::{AxiRx, AxiTx};
use bittide_sys::dna_port_e2::{dna_to_u128, DnaValue};
use bittide_sys::mac::Mac;
use bittide_sys::smoltcp::axi::AxiEthernet;
use bittide_sys::smoltcp::{set_local, set_unicast};
use bittide_sys::time::{Clock, Duration};
use bittide_sys::uart::Uart;
use log::{self, debug};
#[cfg(not(test))]
use riscv_rt::entry;

use smoltcp::iface::{Config, Interface, SocketSet};
use smoltcp::phy::Medium;
use smoltcp::socket::tcp::{Socket, SocketBuffer};
use smoltcp::wire::{EthernetAddress, IpAddress, IpCidr};
use ufmt::uwriteln;

const CLOCK_ADDR: usize = 0b0011 << 28;
const DNA_ADDR: usize = 0b0111 << 28;
const MAC_ADDR: usize = 0b1001 << 28;
const RX_AXI_ADDR: usize = 0b0101 << 28;
const TX_AXI_ADDR: usize = 0b0110 << 28;
const UART_ADDR: usize = 0b0010 << 28;

const MTU: usize = 1500;
const RX_BUFFER_SIZE: usize = 2048;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize and test UART.
    let mut uart = unsafe { Uart::new(UART_ADDR as *mut u8) };

    uwriteln!(uart, "Starting smoltcp-echo").unwrap();
    // Initialize and test clock
    let mut clock = unsafe { Clock::new(CLOCK_ADDR as *const u32) };

    // Create interface
    let dna = unsafe { dna_to_u128(*(DNA_ADDR as *const DnaValue)) };
    let mut eth_addr = EthernetAddress::from_bytes(&dna.to_be_bytes()[0..6]);

    set_unicast(&mut eth_addr);
    set_local(&mut eth_addr);
    let mut config = Config::new(eth_addr.into());
    let axi_tx = AxiTx::new(TX_AXI_ADDR as *mut u8);
    let axi_rx = unsafe { AxiRx::new(RX_AXI_ADDR as *mut usize, RX_BUFFER_SIZE) };
    let mac = Mac::new(MAC_ADDR);
    let mut device: AxiEthernet<MTU> = AxiEthernet::new(Medium::Ethernet, axi_rx, axi_tx);
    let now = clock.elapsed().into();
    let mut iface = Interface::new(config, &mut device, now);
    iface.update_ip_addrs(|ip_addrs| {
        ip_addrs
            .push(IpCidr::new(IpAddress::v4(100, 100, 100, 100), 8))
            .unwrap();
    });

    // Create sockets
    let server_socket = {
        // It is not strictly necessary to use a `static mut` and unsafe code here, but
        // on embedded systems that smoltcp targets it is far better to allocate the data
        // statically to verify that it fits into RAM rather than get undefined behavior
        // when stack overflows.
        static mut TCP_SERVER_RX_DATA: [u8; 2048] = [0; 2048];
        static mut TCP_SERVER_TX_DATA: [u8; 2048] = [0; 2048];
        let tcp_rx_buffer = SocketBuffer::new(unsafe { &mut TCP_SERVER_RX_DATA[..] });
        let tcp_tx_buffer = SocketBuffer::new(unsafe { &mut TCP_SERVER_TX_DATA[..] });
        Socket::new(tcp_rx_buffer, tcp_tx_buffer)
    };

    let mut sockets: [_; 1] = Default::default();
    let mut sockets = SocketSet::new(&mut sockets[..]);
    let server_handle = sockets.add(server_socket);

    let mut mac_status = mac.read();
    loop {
        let elapsed = clock.elapsed().into();
        iface.poll(elapsed, &mut device, &mut sockets);

        let mut socket = sockets.get_mut::<Socket>(server_handle);
        if !socket.is_active() && !socket.is_listening() {
            mac_status = mac.read();
            uwriteln!(uart, "listening").unwrap();
            uwriteln!(uart, "{} ", clock.elapsed()).unwrap();
            socket.listen(7).unwrap();
        }
        if socket.is_open() && socket.may_send() && !socket.may_recv() {
            uwriteln!(uart, "{}", clock.elapsed()).unwrap();
            uwriteln!(uart, "{:?}", mac.read() - mac_status).unwrap();
            uwriteln!(uart, "Closing socket").unwrap();

            socket.close();
        }
        if socket.can_recv() {
            let mut buf = [0; RX_BUFFER_SIZE];
            let mut len = 0;
            socket
                .recv(|buffer| {
                    len = buffer.len();
                    buf[..len].copy_from_slice(buffer);
                    (buffer.len(), ())
                })
                .unwrap();
            socket.send_slice(&buf[0..len]).unwrap();
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
    let mut uart = unsafe { Uart::new(UART_ADDR as *mut u8) };
    riscv::interrupt::free(|| {
        uwriteln!(uart, "... caught an exception. Looping forever now.\n").unwrap();
    });
    loop {
        continue;
    }
}

#[panic_handler]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    let mut uart = unsafe { Uart::new(UART_ADDR as *mut u8) };
    uwriteln!(uart, "Panicked, looping forever now").unwrap();
    loop {
        continue;
    }
}
