// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![cfg_attr(not(feature = "std"), no_std)]
#![allow(unused_mut)]
#![allow(clippy::collapsible_if)]
#![no_main]

use bittide_sys::axi::{AxiRx, AxiTx};
use bittide_sys::smoltcp::axi::AxiEthernet;
use bittide_sys::time::{Clock, Duration};
use bittide_sys::uart::Uart;
use core::fmt::Write;
use log::{self, debug};
#[cfg(not(test))]
use riscv_rt::entry;

use smoltcp::iface::{Config, Interface, SocketSet};
use smoltcp::phy::Medium;
use smoltcp::socket::tcp::{Socket, SocketBuffer};
use smoltcp::wire::{EthernetAddress, IpAddress, IpCidr};
use ufmt::uwriteln;

const UART_ADDR: usize = 0b010 << 29;
const CLOCK_ADDR: usize = 0b011 << 29;
const RX_AXI_ADDR: usize = 0b101 << 29;
const TX_AXI_ADDR: usize = 0b110 << 29;
const RX_BUFFER_SIZE: usize = 2048;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    // Initialize and test UART.
    let mut uart = unsafe { Uart::new(UART_ADDR as *mut u8) };

    uwriteln!(uart, "Starting smoltcp-echo").unwrap();
    // Initialize and test clock
    let mut clock = unsafe { Clock::new(CLOCK_ADDR as *const u32) };

    // Create interface
    let eth_addr = EthernetAddress([0x02, 0x00, 0x00, 0x00, 0x00, 0x01]);
    let mut config = Config::new(eth_addr.into());
    let axi_tx = AxiTx::new(TX_AXI_ADDR as *mut u8);
    let axi_rx = unsafe { AxiRx::new(RX_AXI_ADDR as *mut usize, RX_BUFFER_SIZE) };

    let mut device: AxiEthernet<2048> = AxiEthernet::new(Medium::Ethernet, axi_rx, axi_tx);
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

    let mut last_print = clock.elapsed();
    loop {
        let now = clock.elapsed();
        if now > last_print + Duration::from_secs(1) {
            last_print = now;
            uwriteln!(uart, "time: {}", now).unwrap();
            let socket = sockets.get::<Socket>(server_handle);
            writeln!(uart, "socket: {:?}", socket.state()).unwrap();
        }
        let elapsed = clock.elapsed().into();
        iface.poll(elapsed, &mut device, &mut sockets);

        let mut socket = sockets.get_mut::<Socket>(server_handle);
        if !socket.is_active() && !socket.is_listening() {
            uwriteln!(uart, "listening").unwrap();
            socket.listen(7).unwrap();
        }
        if socket.is_open() && socket.may_send() && !socket.may_recv() {
            uwriteln!(uart, "Closing socket").unwrap();
            socket.close();
        }
        if socket.can_recv() {
            let mut buf = [0; 2048];
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
            None => clock.wait(Duration::from_millis(1)),
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
