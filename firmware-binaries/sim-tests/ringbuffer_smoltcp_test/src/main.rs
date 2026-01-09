// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]
#![feature(sync_unsafe_cell)]

use bittide_hal::manual_additions::aligned_ringbuffer::{
    find_alignment_offset, ReceiveRingbuffer, TransmitRingbuffer,
};
use bittide_hal::manual_additions::timer::Instant;
use bittide_hal::scatter_gather_pe::DeviceInstances;
use bittide_sys::smoltcp::ringbuffer::RingbufferDevice;
use core::fmt::Write;
use log::LevelFilter;
use smoltcp::iface::{Config, Interface, SocketSet, SocketStorage};
use smoltcp::socket::tcp;
use smoltcp::wire::{HardwareAddress, IpAddress, IpCidr};

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

const SERVER_PORT: u16 = 8080;
const CLIENT_PORT: u16 = 49152;

fn to_smoltcp_instant(instant: Instant) -> smoltcp::time::Instant {
    smoltcp::time::Instant::from_micros(instant.micros() as i64)
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    let timer = INSTANCES.timer;

    writeln!(uart, "\n=== Ringbuffer smoltcp Loopback Test ===").ok();

    // Set up logging
    unsafe {
        use bittide_sys::uart::log::LOGGER;
        let logger = &mut (*LOGGER.get());
        logger.set_logger(uart.clone());
        logger.set_timer(INSTANCES.timer);
        logger.display_source = LevelFilter::Debug;
        log::set_logger_racy(logger).ok();
        log::set_max_level_racy(LevelFilter::Trace);
    }

    // Step 1: Find alignment offset
    writeln!(uart, "Step 1: Finding ringbuffer alignment...").ok();
    let rx_offset = {
        let scatter = INSTANCES.scatter_unit;
        let gather = INSTANCES.gather_unit;
        let hal_tx_temp = TransmitRingbuffer::new(gather);
        let hal_rx_temp = ReceiveRingbuffer::new(scatter, 0);
        find_alignment_offset(&hal_tx_temp, &hal_rx_temp)
    };
    writeln!(uart, "  Alignment offset: {}", rx_offset).ok();

    // Step 2: Create smoltcp device
    writeln!(uart, "Step 2: Creating RingbufferDevice...").ok();
    let scatter = INSTANCES.scatter_unit;
    let gather = INSTANCES.gather_unit;
    let rx_buffer = ReceiveRingbuffer::new(scatter, rx_offset);
    let tx_buffer = TransmitRingbuffer::new(gather);
    let mut device = RingbufferDevice::new(rx_buffer, tx_buffer);
    let mtu = device.mtu();
    writeln!(uart, "  MTU: {} bytes", mtu).ok();

    // Step 3: Configure interface with static IP
    writeln!(uart, "Step 3: Configuring network interface...").ok();
    let hw_addr = HardwareAddress::Ip;
    let ip_addr = IpCidr::new(IpAddress::v4(127, 0, 0, 1), 8); // Loopback address
    let config = Config::new(hw_addr);
    let now = to_smoltcp_instant(timer.now());
    let mut iface = Interface::new(config, &mut device, now);
    iface.update_ip_addrs(|addrs| {
        addrs.push(ip_addr).unwrap();
    });
    writeln!(uart, "  IP: {}", ip_addr).ok();

    // Step 4: Create TCP sockets
    writeln!(uart, "Step 4: Creating TCP sockets...").ok();

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

    // Step 5: Set up server to listen
    writeln!(
        uart,
        "Step 5: Starting TCP server on port {}...",
        SERVER_PORT
    )
    .ok();
    let server = sockets.get_mut::<tcp::Socket>(server_handle);
    server.listen(SERVER_PORT).unwrap();
    writeln!(uart, "  Server listening").ok();

    // Step 6: Connect client to server
    writeln!(uart, "Step 6: Connecting client to server...").ok();
    let client = sockets.get_mut::<tcp::Socket>(client_handle);
    let cx = iface.context();
    client
        .connect(cx, (IpAddress::v4(127, 0, 0, 1), SERVER_PORT), CLIENT_PORT)
        .unwrap();
    writeln!(uart, "  Connection initiated").ok();

    // Step 7: Poll until connection established
    writeln!(uart, "Step 7: Waiting for connection...").ok();
    let mut connection_established = false;
    for _ in 0..100 {
        let timestamp = to_smoltcp_instant(timer.now());
        iface.poll(timestamp, &mut device, &mut sockets);

        let client = sockets.get::<tcp::Socket>(client_handle);
        if client.is_active() && !connection_established {
            writeln!(uart, "  Connection established!").ok();
            connection_established = true;
            break;
        }
    }

    if !connection_established {
        writeln!(uart, "  FAILURE: Connection timeout!").ok();
        loop {
            unsafe { riscv::asm::wfi() };
        }
    }

    // Step 8: Send data from client
    writeln!(uart, "Step 8: Sending test data...").ok();
    let test_data = b"Hello from smoltcp!";
    for _ in 0..50 {
        let timestamp = to_smoltcp_instant(timer.now());
        iface.poll(timestamp, &mut device, &mut sockets);

        let client = sockets.get_mut::<tcp::Socket>(client_handle);
        if client.can_send() {
            match client.send_slice(test_data) {
                Ok(sent) => {
                    writeln!(uart, "  Sent {} bytes", sent).ok();
                    break;
                }
                Err(_) => continue,
            }
        }
    }

    // Step 9: Receive data on server
    writeln!(uart, "Step 9: Receiving data on server...").ok();
    let mut received_data = [0u8; 64];
    let mut received_len = 0;

    for _ in 0..50 {
        let timestamp = to_smoltcp_instant(timer.now());
        iface.poll(timestamp, &mut device, &mut sockets);

        let server = sockets.get_mut::<tcp::Socket>(server_handle);
        if server.can_recv() {
            match server.recv_slice(&mut received_data) {
                Ok(len) => {
                    received_len = len;
                    writeln!(uart, "  Received {} bytes", len).ok();
                    break;
                }
                Err(_) => continue,
            }
        }
    }

    // Step 10: Verify data
    writeln!(uart, "Step 10: Verifying data...").ok();
    let received_slice = &received_data[..received_len];

    if received_len == test_data.len() && received_slice == test_data {
        writeln!(uart, "  SUCCESS: Data matches!").ok();
        writeln!(
            uart,
            "  Sent:     {:?}",
            core::str::from_utf8(test_data).unwrap()
        )
        .ok();
        writeln!(
            uart,
            "  Received: {:?}",
            core::str::from_utf8(received_slice).unwrap()
        )
        .ok();
    } else {
        writeln!(uart, "  FAILURE: Data mismatch!").ok();
        writeln!(
            uart,
            "  Expected {} bytes: {:?}",
            test_data.len(),
            test_data
        )
        .ok();
        writeln!(uart, "  Got {} bytes: {:?}", received_len, received_slice).ok();
    }

    writeln!(uart, "\n=== Test Complete ===").ok();

    loop {
        unsafe { riscv::asm::wfi() };
    }
}

#[cfg(not(test))]
#[panic_handler]
fn panic(info: &core::panic::PanicInfo) -> ! {
    let mut uart = INSTANCES.uart;
    writeln!(uart, "PANIC: {}", info).ok();
    loop {
        unsafe { riscv::asm::wfi() };
    }
}
