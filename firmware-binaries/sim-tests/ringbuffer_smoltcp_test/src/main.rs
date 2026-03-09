// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]
#![feature(sync_unsafe_cell)]

use bittide_hal::manual_additions::ringbuffer_test::ringbuffers::AlignedReceiveBuffer;
use bittide_hal::manual_additions::timer::Instant;
use bittide_hal::ringbuffer_test::DeviceInstances;
use bittide_sys::net_state::{Manager, SmoltcpLink, Subordinate, UgnEdge, UgnReport};
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

        {
            let mut link =
                SmoltcpLink::new(&mut iface, &mut sockets, client_handle, 0, true, false);
            manager.step(&mut link);
        }
        {
            let mut link =
                SmoltcpLink::new(&mut iface, &mut sockets, server_handle, 0, true, false);
            subordinate.step(&mut link);
        }

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
            if let Some(edge) = edge {
                info!(
                    "  Edge {}: {}:{} -> {}:{}, ugn={}",
                    idx, edge.src_node, edge.src_port, edge.dst_node, edge.dst_port, edge.ugn
                );
            } else {
                info!("  Edge {}: missing", idx);
            }
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
