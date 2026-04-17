// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]
#![feature(sync_unsafe_cell)]

use bittide_hal::manual_additions::ringbuffer::AlignedReceiveBuffer;
use bittide_hal::ringbuffer_test::DeviceInstances;
use bittide_sys::net_state::{UgnEdge, UgnReport};
use bittide_sys::smoltcp::link_interface::{LinkBuffers, LinkInterface};
use bittide_sys::smoltcp::link_protocol::{Command, CommandWire, UgnEdgeWire};
use core::cell::SyncUnsafeCell;
use core::fmt::Write;
use log::{info, LevelFilter};
use smoltcp::iface::SocketStorage;
use ufmt::uwriteln;

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

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

    info!("=== Ringbuffer smoltcp Loopback Test (V2) ===");

    // Step 1: Align ringbuffers
    info!("Step 1: Finding ringbuffer alignment...");
    let tx_buffer0 = INSTANCES.transmit_ringbuffer_0;
    let rx_buffer0 = INSTANCES.receive_ringbuffer_0;
    let tx_buffer1 = INSTANCES.transmit_ringbuffer_1;
    let rx_buffer1 = INSTANCES.receive_ringbuffer_1;

    let mut rx_aligned0 = AlignedReceiveBuffer::new(rx_buffer0);
    let mut rx_aligned1 = AlignedReceiveBuffer::new(rx_buffer1);
    rx_aligned0.align(&tx_buffer0);
    rx_aligned1.align(&tx_buffer1);

    // Step 2: Create LinkInterfaces with simultaneous open
    info!("Step 2: Creating LinkInterfaces with TCP simultaneous open...");

    // Manager side
    static MANAGER_SOCKET_RX: SyncUnsafeCell<[u8; 256]> = SyncUnsafeCell::new([0; 256]);
    static MANAGER_SOCKET_TX: SyncUnsafeCell<[u8; 256]> = SyncUnsafeCell::new([0; 256]);
    static MANAGER_STORAGE: SyncUnsafeCell<[SocketStorage; 1]> =
        SyncUnsafeCell::new([SocketStorage::EMPTY; 1]);

    let mut link_manager = LinkInterface::new(
        rx_aligned0,
        tx_buffer1,
        LinkBuffers {
            socket_storage: unsafe { &mut *MANAGER_STORAGE.get() },
            tcp_rx_buffer: unsafe { &mut *MANAGER_SOCKET_RX.get() },
            tcp_tx_buffer: unsafe { &mut *MANAGER_SOCKET_TX.get() },
        },
        timer,
    );

    // Subordinate side
    static SUBORDINATE_SOCKET_RX: SyncUnsafeCell<[u8; 256]> = SyncUnsafeCell::new([0; 256]);
    static SUBORDINATE_SOCKET_TX: SyncUnsafeCell<[u8; 256]> = SyncUnsafeCell::new([0; 256]);
    static SUBORDINATE_STORAGE: SyncUnsafeCell<[SocketStorage; 1]> =
        SyncUnsafeCell::new([SocketStorage::EMPTY; 1]);

    let mut link_subordinate = LinkInterface::new(
        rx_aligned1,
        tx_buffer0,
        LinkBuffers {
            socket_storage: unsafe { &mut *SUBORDINATE_STORAGE.get() },
            tcp_rx_buffer: unsafe { &mut *SUBORDINATE_SOCKET_RX.get() },
            tcp_tx_buffer: unsafe { &mut *SUBORDINATE_SOCKET_TX.get() },
        },
        unsafe { bittide_hal::shared_devices::Timer::new(INSTANCES.timer.0) },
    );

    // Step 3: Wait for connection establishment
    info!("Step 3: Waiting for connection establishment...");
    for _ in 0..1000 {
        link_manager.poll();
        link_subordinate.poll();

        if link_manager.is_established() && link_subordinate.is_established() {
            info!("  Both links established!");
            break;
        }
    }

    if !link_manager.is_established() || !link_subordinate.is_established() {
        info!("  FAILURE: Connection timeout!");
        uwriteln!(uart, "=== Test Failed ===").unwrap();
        loop {
            continue;
        }
    }

    // Step 4: Exchange DNA
    info!("Step 4: Exchanging DNA...");
    let manager_dna: [u8; 12] = core::array::from_fn(|i| (i + 100) as u8);
    let subordinate_dna: [u8; 12] = core::array::from_fn(|i| i as u8);

    // Both sides send their DNA
    link_manager.send(&manager_dna);
    link_subordinate.send(&subordinate_dna);

    // Both sides receive partner DNA using try_recv_bytes in polling loop
    let mut manager_partner_buf = [0u8; 12];
    let mut subordinate_partner_buf = [0u8; 12];
    let mut manager_received = false;
    let mut subordinate_received = false;

    for _ in 0..500 {
        link_manager.poll();
        link_subordinate.poll();

        // Manager: try to receive partner DNA
        if !manager_received {
            match link_manager.try_recv_bytes(&mut manager_partner_buf) {
                Ok(12) => {
                    info!(
                        "  Manager received partner DNA: {:02X?}",
                        &manager_partner_buf
                    );
                    manager_received = true;
                }
                Ok(n) if n > 0 => {
                    info!("  FAILURE: Manager received partial DNA: {} bytes", n);
                    uwriteln!(uart, "=== Test Failed ===").unwrap();
                    loop {
                        continue;
                    }
                }
                _ => {}
            }
        }

        // Subordinate: try to receive partner DNA
        if !subordinate_received {
            match link_subordinate.try_recv_bytes(&mut subordinate_partner_buf) {
                Ok(12) => {
                    info!(
                        "  Subordinate received partner DNA: {:02X?}",
                        &subordinate_partner_buf
                    );
                    subordinate_received = true;
                }
                Ok(n) if n > 0 => {
                    info!("  FAILURE: Subordinate received partial DNA: {} bytes", n);
                    uwriteln!(uart, "=== Test Failed ===").unwrap();
                    loop {
                        continue;
                    }
                }
                _ => {}
            }
        }

        // Break when both sides received
        if manager_received && subordinate_received {
            break;
        }
    }

    if !manager_received || !subordinate_received {
        info!(
            "  FAILURE: DNA exchange failed! Manager={}, Subordinate={}",
            manager_received, subordinate_received
        );
        uwriteln!(uart, "=== Test Failed ===").unwrap();
        loop {
            continue;
        }
    }

    info!("  DNA exchange complete!");

    // Step 4b: Exchange port numbers
    info!("Step 4b: Exchanging port numbers...");
    let manager_port: u32 = 0;
    let subordinate_port: u32 = 1;

    // Both sides send their port number
    link_manager.send(&manager_port.to_le_bytes());
    link_subordinate.send(&subordinate_port.to_le_bytes());

    // Both sides receive partner port number
    let mut manager_partner_port = [0u8; 4];
    let mut subordinate_partner_port = [0u8; 4];
    let mut manager_port_received = false;
    let mut subordinate_port_received = false;

    for _ in 0..500 {
        link_manager.poll();
        link_subordinate.poll();

        // Manager: try to receive partner port
        if !manager_port_received {
            if let Ok(4) = link_manager.try_recv_bytes(&mut manager_partner_port) {
                let port = u32::from_le_bytes(manager_partner_port);
                info!("  Manager received partner port: {}", port);
                manager_port_received = true;
            }
        }

        // Subordinate: try to receive partner port
        if !subordinate_port_received {
            if let Ok(4) = link_subordinate.try_recv_bytes(&mut subordinate_partner_port) {
                let port = u32::from_le_bytes(subordinate_partner_port);
                info!("  Subordinate received partner port: {}", port);
                subordinate_port_received = true;
            }
        }

        // Break when both sides received
        if manager_port_received && subordinate_port_received {
            break;
        }
    }

    if !manager_port_received || !subordinate_port_received {
        info!(
            "  FAILURE: Port exchange failed! Manager={}, Subordinate={}",
            manager_port_received, subordinate_port_received
        );
        uwriteln!(uart, "=== Test Failed ===").unwrap();
        loop {
            continue;
        }
    }

    info!("  Port exchange complete!");

    // Step 5: Manager requests UGN report
    info!("Step 5: Manager requesting UGN report...");
    let subordinate_report = build_placeholder_report();

    // Phase 1: Manager sends command, subordinate receives it
    info!("  Phase 1: Command exchange");

    // Send command using zerocopy with try_send
    let cmd_wire: CommandWire = Command::RequestUgnReport.into();
    let mut command_sent = false;
    let mut command_received = false;

    for _ in 0..100 {
        link_manager.poll();
        link_subordinate.poll();

        // Manager: try to send command
        if !command_sent {
            if let Ok(()) = link_manager.try_send(&cmd_wire) {
                info!("    Manager: sent RequestUgnReport command");
                command_sent = true;
            }
        }

        // Subordinate: try to receive command
        if !command_received {
            if let Ok(wire) = link_subordinate.try_recv::<CommandWire>() {
                if let Ok(Command::RequestUgnReport) = Command::try_from(wire) {
                    info!("    Subordinate: received RequestUgnReport command");
                    command_received = true;
                }
            }
        }

        // Break when both sides are done
        if command_sent && command_received {
            break;
        }
    }

    if !command_sent || !command_received {
        info!(
            "  FAILURE: Command exchange failed! Sent={}, Received={}",
            command_sent, command_received
        );
        uwriteln!(uart, "=== Test Failed ===").unwrap();
        loop {
            continue;
        }
    }

    // Phase 2: Subordinate sends count, manager receives it
    info!("  Phase 2: Count exchange");
    let count_byte = subordinate_report.count.to_le_bytes();
    let mut count_sent = false;
    let mut count_received = false;
    let mut manager_count: u8 = 0;

    for _ in 0..100 {
        link_manager.poll();
        link_subordinate.poll();

        // Subordinate: try to send count
        if !count_sent {
            count_sent = link_subordinate.try_send_bytes(&count_byte).is_ok();
            if count_sent {
                info!("    Subordinate: sent count={}", subordinate_report.count);
            }
        }

        // Manager: try to receive count
        if !count_received {
            let mut count_buf = [0u8; 1];
            if let Ok(1) = link_manager.try_recv_bytes(&mut count_buf) {
                manager_count = count_buf[0];
                info!("    Manager: received count={}", manager_count);
                count_received = true;
            }
        }

        // Break when both sides are done
        if count_sent && count_received {
            break;
        }
    }

    if !count_sent || !count_received {
        info!(
            "  FAILURE: Count exchange failed! Sent={}, Received={}",
            count_sent, count_received
        );
        uwriteln!(uart, "=== Test Failed ===").unwrap();
        loop {
            continue;
        }
    }

    // Phase 3: Subordinate sends edges, manager receives them using zerocopy
    info!("  Phase 3: Edges exchange");
    let mut received_edges: [Option<UgnEdge>; 64] = [None; 64];
    let mut subordinate_sent_edges = 0;
    let mut manager_received_edges = 0;

    for _ in 0..500 {
        link_manager.poll();
        link_subordinate.poll();

        // Subordinate: send next edge if any remain using zerocopy
        if subordinate_sent_edges < subordinate_report.count as usize {
            if let Some(edge) = subordinate_report.edges[subordinate_sent_edges] {
                let wire: UgnEdgeWire = edge.into();
                if let Ok(()) = link_subordinate.try_send(&wire) {
                    info!("    Subordinate: sent edge {}", subordinate_sent_edges);
                    subordinate_sent_edges += 1;
                }
            }
        }

        // Manager: try to receive next edge using zerocopy
        if manager_received_edges < manager_count as usize {
            if let Ok(wire) = link_manager.try_recv::<UgnEdgeWire>() {
                let edge: UgnEdge = wire.into();
                info!("    Manager: received edge {}", manager_received_edges);
                received_edges[manager_received_edges] = Some(edge);
                manager_received_edges += 1;
            }
        }

        // Break when both sides are done
        if subordinate_sent_edges >= subordinate_report.count as usize
            && manager_received_edges >= manager_count as usize
        {
            info!("  Edges exchange complete!");
            break;
        }
    }

    if manager_received_edges != manager_count as usize {
        info!(
            "  FAILURE: Manager received {} edges but expected {}!",
            manager_received_edges, manager_count
        );
        uwriteln!(uart, "=== Test Failed ===").unwrap();
        loop {
            continue;
        }
    }

    // Step 6: Verify results
    info!("Step 6: Verifying results...");

    if manager_count == 0 {
        info!("  FAILURE: No UGN edges received!");
        info!("  Manager link state: {:?}", link_manager.state());
        info!("  Subordinate link state: {:?}", link_subordinate.state());
    } else {
        info!("  SUCCESS: Manager received {} UGN edges", manager_count);
        for (idx, edge_opt) in received_edges
            .iter()
            .enumerate()
            .take(manager_count as usize)
        {
            if let Some(edge) = edge_opt {
                info!(
                    "  Edge {}: {:02X?}:{} -> {:02X?}:{}, ugn={}",
                    idx, edge.src_node, edge.src_port, edge.dst_node, edge.dst_port, edge.ugn
                );
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
        let mut src_dna = [0u8; 12];
        src_dna[0] = idx as u8;
        let mut dst_dna = [0u8; 12];
        dst_dna[0] = (idx + 1) as u8;
        report.edges[idx] = Some(UgnEdge {
            src_node: src_dna,
            src_port: idx as u32,
            dst_node: dst_dna,
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
