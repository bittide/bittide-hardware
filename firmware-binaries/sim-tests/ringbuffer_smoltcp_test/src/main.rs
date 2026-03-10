// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]
#![feature(sync_unsafe_cell)]

use bittide_hal::manual_additions::ringbuffer_test::ringbuffers::AlignedReceiveBuffer;
use bittide_hal::manual_additions::timer::Instant;
use bittide_hal::ringbuffer_test::DeviceInstances;
use bittide_sys::net_state::{Manager, Subordinate, UgnEdge, UgnReport};
use bittide_sys::smoltcp::ringbuffer::RingbufferDevice;
use core::fmt::Write;
use log::{info, trace, LevelFilter};
use smoltcp::iface::{Config, Interface, SocketSet, SocketStorage};
use smoltcp::socket::tcp;
use smoltcp::wire::HardwareAddress;
use ufmt::uwriteln;

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

fn to_smoltcp_instant(instant: Instant) -> smoltcp::time::Instant {
    smoltcp::time::Instant::from_micros(instant.micros() as i64)
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
        log::set_max_level_racy(LevelFilter::Info);
    }

    info!("=== Ringbuffer smoltcp Loopback Test ===");

    // Set up ringbuffers
    info!("Step 1: Finding ringbuffer alignment...");
    let tx_buffer0 = INSTANCES.transmit_ringbuffer_0;
    let rx_buffer0 = INSTANCES.receive_ringbuffer_0;

    let tx_buffer1 = INSTANCES.transmit_ringbuffer_1;
    let rx_buffer1 = INSTANCES.receive_ringbuffer_1;
    let mut rx_aligned0 = AlignedReceiveBuffer::new(rx_buffer0);
    let mut rx_aligned1 = AlignedReceiveBuffer::new(rx_buffer1);

    rx_aligned0.align(&tx_buffer0);
    rx_aligned1.align(&tx_buffer1);

    let rx_offset0 = rx_aligned0
        .get_alignment_offset()
        .expect("Failed to find RX buffer alignment");
    let rx_offset1 = rx_aligned1
        .get_alignment_offset()
        .expect("Failed to find RX buffer alignment");
    trace!("  Alignment offset 0: {}", rx_offset0);
    trace!("  Alignment offset 1: {}", rx_offset1);

    // Step 2: Create smoltcp device
    info!("Step 2: Creating RingbufferDevice...");
    let mut device0 = RingbufferDevice::new(rx_aligned0, tx_buffer1);
    let mut device1 = RingbufferDevice::new(rx_aligned1, tx_buffer0);
    let mtu = device0.mtu();
    trace!("  MTU: {} bytes", mtu);

    // Step 3: Configure network interface
    info!("Step 3: Configuring network interfaces...");
    let hw_addr = HardwareAddress::Ip;
    let config0 = Config::new(hw_addr);
    let config1 = Config::new(hw_addr);
    let now = to_smoltcp_instant(timer.now());
    let mut iface0 = Interface::new(config0, &mut device0, now);
    let mut iface1 = Interface::new(config1, &mut device1, now);
    let server_ip = [100, 100, 100, 100];
    let client_ip = [100, 100, 100, 101];
    iface0.update_ip_addrs(|addrs| {
        addrs
            .push(smoltcp::wire::IpCidr::new(
                smoltcp::wire::IpAddress::v4(
                    client_ip[0],
                    client_ip[1],
                    client_ip[2],
                    client_ip[3],
                ),
                24,
            ))
            .unwrap();
    });
    iface1.update_ip_addrs(|addrs| {
        addrs
            .push(smoltcp::wire::IpCidr::new(
                smoltcp::wire::IpAddress::v4(
                    server_ip[0],
                    server_ip[1],
                    server_ip[2],
                    server_ip[3],
                ),
                24,
            ))
            .unwrap();
    });

    // Step 4: Create TCP sockets
    info!("Step 4: Creating TCP sockets...");

    // Server socket - reduced buffer sizes to fit in memory
    static mut SERVER_RX_BUF0: [u8; 256] = [0; 256];
    static mut SERVER_TX_BUF0: [u8; 256] = [0; 256];

    let server_rx_buffer = tcp::SocketBuffer::new(unsafe { &mut SERVER_RX_BUF0[..] });
    let server_tx_buffer = tcp::SocketBuffer::new(unsafe { &mut SERVER_TX_BUF0[..] });
    let server_socket = tcp::Socket::new(server_rx_buffer, server_tx_buffer);

    // Client socket - reduced buffer sizes to fit in memory
    static mut CLIENT_RX_BUF: [u8; 256] = [0; 256];
    static mut CLIENT_TX_BUF: [u8; 256] = [0; 256];
    let client_rx_buffer = tcp::SocketBuffer::new(unsafe { &mut CLIENT_RX_BUF[..] });
    let client_tx_buffer = tcp::SocketBuffer::new(unsafe { &mut CLIENT_TX_BUF[..] });
    let client_socket = tcp::Socket::new(client_rx_buffer, client_tx_buffer);

    let mut server_sockets_storage: [SocketStorage; 1] = Default::default();
    let mut client_sockets_storage: [SocketStorage; 1] = Default::default();
    let mut server_sockets = SocketSet::new(&mut server_sockets_storage[..]);
    let mut client_sockets = SocketSet::new(&mut client_sockets_storage[..]);

    let server_handle = server_sockets.add(server_socket);
    let client_handle = client_sockets.add(client_socket);

    // Step 5: Initialize link state machines
    info!("Step 5: Initializing link state machines...");

    // Main event loop
    info!("Step 7: Running main event loop...");
    let mut done_logged = false;

    let mut manager = Manager::new(iface0, client_handle, 0, server_ip);
    let dna: [u8; 12] = core::array::from_fn(|i| i as u8);
    let mut subordinate = Subordinate::new(iface1, server_handle, 0, dna);
    subordinate.set_report(build_placeholder_report());

    for _ in 0..1000 {
        let timestamp = to_smoltcp_instant(timer.now());
        manager.poll(timestamp, &mut device0, &mut client_sockets);
        subordinate.poll(timestamp, &mut device1, &mut server_sockets);

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
    } else {
        let report = manager.report();
        info!("  SUCCESS: Manager received {} UGN edges", report.count);
        for (idx, edge) in report.edges.iter().enumerate() {
            if idx >= report.count as usize {
                break;
            }
            if let Some(edge) = edge {
                info!(
                    "  Edge {}: {}:{} -> {}:{}, ugn={}",
                    idx, edge.src_node, edge.src_port, edge.dst_node, edge.dst_port, edge.ugn
                );
            } else {
                info!("  Edge {}: missing", idx);
            }
        }
    }

    uwriteln!(uart, "=== Test Complete ===").unwrap();

    loop {
        continue;
    }
}

fn build_placeholder_report() -> UgnReport {
    let mut report = UgnReport::new();
    report.count = 8;
    for idx in 0..8 {
        report.edges[idx] = Some(UgnEdge {
            src_node: idx as u32,
            src_port: idx as u32,
            dst_node: (idx as u32).saturating_add(1),
            dst_port: idx as u32,
            ugn: 100 + idx as i64,
        });
    }
    report
}

#[cfg(not(test))]
#[panic_handler]
fn panic(info: &core::panic::PanicInfo) -> ! {
    let mut uart = INSTANCES.uart;
    writeln!(uart, "PANIC: {}", info).ok();
    loop {}
}
