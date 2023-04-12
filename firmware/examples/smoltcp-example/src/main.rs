#![cfg_attr(not(feature = "std"), no_std)]
#![allow(unused_mut)]
#![allow(clippy::collapsible_if)]
#![no_main]

// #![feature(alloc_error_handler)]

use core::fmt::Write;
#[cfg(not(test))]
use riscv_rt::entry;

mod loopback;
mod time;
mod uart;
use uart::Uart;

mod panic_handler;
use panic_handler::set_panic_handler_uart;
const UART_ADDR:usize = 0x4000_0000;
#[cfg(feature = "std")]
#[allow(dead_code)]
mod utils;

use core::str;

use loopback::Loopback;
use smoltcp::iface::{Config, Interface, SocketSet};
use smoltcp::phy::Medium;
use smoltcp::socket::tcp;
use smoltcp::time::{Duration, Instant};
use smoltcp::wire::{EthernetAddress, IpAddress, IpCidr};
use time::Clock;

#[cfg_attr(not(test), entry)]
fn main() -> !{

    // Initialize and test UART.
    let mut uart = unsafe { Uart::new(UART_ADDR as *mut u8) };
    let mut panic_uart = unsafe { Uart::new(UART_ADDR as *mut u8) };
    unsafe{ set_panic_handler_uart(panic_uart)};
    writeln!(uart, "Hello!").unwrap();

    // Initialize and test clock
    let mut clock = time::Clock::new(0xa000_0000, 125*10^6);
    clock.elapsed_ticks();
    writeln!(uart, "Clock: {:?}", clock).unwrap();

    // Create interface
    let mut config = Config::new();
    let mut device = Loopback::new(Medium::Ethernet);
    config.hardware_addr = Some(EthernetAddress([0x02, 0x00, 0x00, 0x00, 0x00, 0x01]).into());

    let mut iface = Interface::new(config, &mut device);
    iface.update_ip_addrs(|ip_addrs| {
        ip_addrs
            .push(IpCidr::new(IpAddress::v4(127, 0, 0, 1), 8))
            .unwrap();
    });

    // Create sockets
    let server_socket = {
        // It is not strictly necessary to use a `static mut` and unsafe code here, but
        // on embedded systems that smoltcp targets it is far better to allocate the data
        // statically to verify that it fits into RAM rather than get undefined behavior
        // when stack overflows.
        static mut TCP_SERVER_RX_DATA: [u8; 1024] = [0; 1024];
        static mut TCP_SERVER_TX_DATA: [u8; 1024] = [0; 1024];
        let tcp_rx_buffer = tcp::SocketBuffer::new(unsafe { &mut TCP_SERVER_RX_DATA[..] });
        let tcp_tx_buffer = tcp::SocketBuffer::new(unsafe { &mut TCP_SERVER_TX_DATA[..] });
        tcp::Socket::new(tcp_rx_buffer, tcp_tx_buffer)
    };

    let client_socket = {
        static mut TCP_CLIENT_RX_DATA: [u8; 1024] = [0; 1024];
        static mut TCP_CLIENT_TX_DATA: [u8; 1024] = [0; 1024];
        let tcp_rx_buffer = tcp::SocketBuffer::new(unsafe { &mut TCP_CLIENT_RX_DATA[..] });
        let tcp_tx_buffer = tcp::SocketBuffer::new(unsafe { &mut TCP_CLIENT_TX_DATA[..] });
        tcp::Socket::new(tcp_rx_buffer, tcp_tx_buffer)
    };

    let mut sockets: [_; 2] = Default::default();
    let mut sockets = SocketSet::new(&mut sockets[..]);
    let server_handle = sockets.add(server_socket);
    let client_handle = sockets.add(client_socket);

    let mut did_listen = false;
    let mut did_connect = false;
    let mut done = false;
    while !done && clock.elapsed() < Instant::from_millis(1_000) {
        iface.poll(clock.elapsed(), &mut device, &mut sockets);

        let mut socket = sockets.get_mut::<tcp::Socket>(server_handle);
        if !socket.is_active() && !socket.is_listening() {
            if !did_listen {
                _ = writeln!(uart,"listening");
                socket.listen(1234).unwrap();
                did_listen = true;
            }
        }

        if socket.can_recv() {
            _ = writeln!(uart,
                "got {:?}",
                socket.recv(|buffer| { (buffer.len(), str::from_utf8(buffer).unwrap()) })
            );
            socket.close();
            done = true;
        }

        let mut socket = sockets.get_mut::<tcp::Socket>(client_handle);
        let cx = iface.context();
        if !socket.is_open() {
            if !did_connect {
                _ = writeln!(uart,"connecting");
                socket
                    .connect(cx, (IpAddress::v4(127, 0, 0, 1), 1234), 65000)
                    .unwrap();
                did_connect = true;
            }
        }

        if socket.can_send() {
            _ = writeln!(uart,"sending");
            socket.send_slice(b"0123456789abcdef").unwrap();
            socket.close();
        }

        match iface.poll_delay(clock.elapsed(), &sockets) {
            Some(Duration::ZERO) => _ = writeln!(uart,"resuming"),
            Some(delay) => {
                _ = writeln!(uart,"sleeping for {} us", delay.micros());
                clock.advance(delay)
            }
            None => clock.advance(Duration::from_millis(1)),
        }
    }

    if done {
        _ = writeln!(uart,"done");
    } else {
        _ = writeln!(uart,"this is taking too long, bailing out");
    }
    loop {}
}

#[export_name = "ExceptionHandler"]
fn exception_handler(_trap_frame: &riscv_rt::TrapFrame) -> ! {
    let mut uart = unsafe { Uart::new(UART_ADDR as *mut u8) };
    riscv::interrupt::free(|| {
        writeln!(uart,"... caught an exception. Looping forever now.\n");
    });
    loop {
        continue;
    }
}
