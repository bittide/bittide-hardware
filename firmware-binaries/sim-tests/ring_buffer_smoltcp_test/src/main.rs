// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]
#![feature(sync_unsafe_cell)]

use bittide_hal::manual_additions::ringbuffer::AlignedReceiveBuffer;
use bittide_hal::ringbuffer_test::DeviceInstances;
use bittide_sys::smoltcp::link_interface::LinkInterface;
use bittide_sys::smoltcp::link_protocol::{Command, UgnEdge, UgnReport};
use core::fmt::Write;
use log::{debug, error, info, LevelFilter};
use ufmt::uwriteln;

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    let timer = INSTANCES.timer;

    unsafe {
        use bittide_sys::uart::log::LOGGER;
        let logger = &mut (*LOGGER.get());
        logger.set_logger(uart.clone());
        logger.set_timer(INSTANCES.timer);
        logger.display_source = LevelFilter::Warn;
        log::set_logger_racy(logger).ok();
        log::set_max_level_racy(LevelFilter::Info);
    }

    info!("=== RingBuffer smoltcp Loopback Test (V2) ===");

    // Step 1: Align ring_buffers
    info!("Step 1: Finding ring_buffer alignment...");
    let tx_buffer0 = INSTANCES.transmit_ring_buffer_0;
    let rx_buffer0 = INSTANCES.receive_ring_buffer_0;
    let tx_buffer1 = INSTANCES.transmit_ring_buffer_1;
    let rx_buffer1 = INSTANCES.receive_ring_buffer_1;

    let mut rx_aligned0 = AlignedReceiveBuffer::new(rx_buffer0);
    let mut rx_aligned1 = AlignedReceiveBuffer::new(rx_buffer1);
    while !(rx_aligned0.is_aligned() & rx_aligned1.is_aligned()) {
        rx_aligned0.align_step(&tx_buffer0);
        rx_aligned1.align_step(&tx_buffer1);
    }

    tx_buffer0.set_enable(true);
    tx_buffer1.set_enable(true);
    rx_aligned0.buffer.set_enable(true);
    rx_aligned1.buffer.set_enable(true);

    // Step 2: Create LinkInterfaces with simultaneous open
    info!("Step 2: Creating LinkInterfaces with TCP simultaneous open...");

    let mut link_manager = LinkInterface::<_, _, 512>::new(rx_aligned0, tx_buffer1, timer);
    link_manager.connect();

    let mut link_subordinate = LinkInterface::<_, _, 512>::new(rx_aligned1, tx_buffer0, unsafe {
        bittide_hal::shared_devices::Timer::new(INSTANCES.timer.0)
    });
    link_subordinate.connect();

    // Step 3: Wait for connection establishment
    info!("Step 3: Waiting for connection establishment...");
    for _ in 0..1000 {
        link_manager.poll();
        link_subordinate.poll();

        if link_manager.is_established() && link_subordinate.is_established() {
            debug!("  Both links established!");
            break;
        }
    }

    if !link_manager.is_established() || !link_subordinate.is_established() {
        error!("  FAILURE: Connection timeout!");
        uwriteln!(uart, "=== Test Failed ===").unwrap();
        loop {
            continue;
        }
    }

    // Step 4: Exchange DNA and port numbers
    info!("Step 4: Exchanging DNA and port numbers...");
    let manager_dna: [u8; 12] = core::array::from_fn(|i| (i + 100) as u8);
    let subordinate_dna: [u8; 12] = core::array::from_fn(|i| i as u8);
    let manager_port = 0u32;
    let subordinate_port = 1u32;

    let (mut mgr_dna_sent, mut mgr_port_sent) = (false, false);
    let (mut sub_dna_sent, mut sub_port_sent) = (false, false);
    let mut mgr_partner_dna = None::<[u8; 12]>;
    let mut sub_partner_dna = None::<[u8; 12]>;
    let mut mgr_partner_port = None::<u32>;
    let mut sub_partner_port = None::<u32>;

    for _ in 0..1000 {
        link_manager.poll();
        link_subordinate.poll();

        // Send DNA, then port (ordering matters on the TCP stream)
        if !mgr_dna_sent {
            mgr_dna_sent = link_manager.try_send_bytes(&manager_dna).is_ok();
        }
        if mgr_dna_sent && !mgr_port_sent {
            mgr_port_sent = link_manager.try_send(&manager_port).is_ok();
        }
        if !sub_dna_sent && link_subordinate.try_send_bytes(&subordinate_dna).is_ok() {
            sub_dna_sent = true;
        }
        if sub_dna_sent && !sub_port_sent {
            sub_port_sent = link_subordinate.try_send(&subordinate_port).is_ok();
        }

        // Receive DNA, then port (must match send order)
        if mgr_partner_dna.is_none() {
            let mut buf = [0u8; 12];
            if let Ok(12) = link_manager.try_recv_bytes(&mut buf) {
                debug!("  Manager received partner DNA: {:02X?}", buf);
                mgr_partner_dna = Some(buf);
            }
        }
        if mgr_partner_dna.is_some() && mgr_partner_port.is_none() {
            if let Ok(port) = link_manager.try_recv() {
                info!("  Manager received partner port: {}", port);
                mgr_partner_port = Some(port);
            }
        }
        if sub_partner_dna.is_none() {
            let mut buf = [0u8; 12];
            if let Ok(12) = link_subordinate.try_recv_bytes(&mut buf) {
                debug!("  Subordinate received partner DNA: {:02X?}", buf);
                sub_partner_dna = Some(buf);
            }
        }
        if sub_partner_dna.is_some() && sub_partner_port.is_none() {
            if let Ok(port) = link_subordinate.try_recv() {
                info!("  Subordinate received partner port: {}", port);
                sub_partner_port = Some(port);
            }
        }

        if mgr_partner_dna.is_some()
            && mgr_partner_port.is_some()
            && sub_partner_dna.is_some()
            && sub_partner_port.is_some()
        {
            break;
        }
    }

    if mgr_partner_dna.is_none()
        || sub_partner_dna.is_none()
        || mgr_partner_port.is_none()
        || sub_partner_port.is_none()
    {
        error!("  FAILURE: DNA/port exchange incomplete!");
        uwriteln!(uart, "=== Test Failed ===").unwrap();
        loop {
            continue;
        }
    }
    info!("  DNA and port exchange complete!");

    // Step 5: Manager requests UGN report
    info!("Step 5: Manager requesting UGN report...");
    let subordinate_report = build_placeholder_report();

    // Phase 1: Manager sends command, subordinate receives it
    info!("  Phase 1: Command exchange");

    let mut command_sent = false;
    let mut command_received = false;

    for _ in 0..100 {
        link_manager.poll();
        link_subordinate.poll();

        if !command_sent && link_manager.try_send(&Command::RequestUgnReport).is_ok() {
            debug!("    Manager: sent RequestUgnReport command");
            command_sent = true;
        }

        if !command_received {
            if let Ok(wire) = link_subordinate.try_recv::<CommandWire>() {
                if let Ok(Command::RequestUgnReport) = link.try_recv::<Command>() {
                    info!("  Received RequestUgnReport on link {}", i);
                    command_received = true;
                    break;
                    } else {
                        warn!("  Link {}: received unknown command: {:?}", i, wire);
                    }
                }
            }
        }

        if command_sent && command_received {
            break;
        }
    }

    if !command_sent || !command_received {
        error!(
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
    let mut count_sent = false;
    let mut count_received = false;
    let mut manager_count: u32 = 0;

    for _ in 0..100 {
        link_manager.poll();
        link_subordinate.poll();

        if !count_sent {
            count_sent = link_subordinate.try_send(&subordinate_report.count).is_ok();
            if count_sent {
                debug!("    Subordinate: sent count={}", subordinate_report.count);
            }
        }

        if !count_received {
            if let Ok(count) = link_manager.try_recv::<u32>() {
                manager_count = count;
                debug!("    Manager: received count={}", manager_count);
                count_received = true;
            }
        }

        if count_sent && count_received {
            break;
        }
    }

    if !count_sent || !count_received {
        error!(
            "  FAILURE: Count exchange failed! Sent={}, Received={}",
            count_sent, count_received
        );
        uwriteln!(uart, "=== Test Failed ===").unwrap();
        loop {
            continue;
        }
    }

    info!("  Phase 3: Edge exchange");
    let mut received_edges: [Option<UgnEdge>; 64] = [None; 64];
    let mut subordinate_sent_edges = 0;
    let mut manager_received_edges = 0;

    for _ in 0..500 {
        link_manager.poll();
        link_subordinate.poll();

        if subordinate_sent_edges < subordinate_report.count as usize {
            if let Some(edge) = subordinate_report.edges[subordinate_sent_edges] {
                if link_subordinate.try_send(&edge).is_ok() {
                    debug!("    Subordinate: sent edge {}", subordinate_sent_edges);
                    subordinate_sent_edges += 1;
                }
            }
        }

        if manager_received_edges < manager_count as usize {
            if let Ok(edge) = link_manager.try_recv::<UgnEdge>() {
                debug!("    Manager: received edge {}", manager_received_edges);
                received_edges[manager_received_edges] = Some(edge);
                manager_received_edges += 1;
            }
        }

        if subordinate_sent_edges >= subordinate_report.count as usize
            && manager_received_edges >= manager_count as usize
        {
            info!("  Edges exchange complete!");
            break;
        }
    }

    if manager_received_edges != manager_count as usize {
        error!(
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
        error!("  FAILURE: No UGN edges received!");
        error!("  Manager link state: {:?}", link_manager.state());
        error!("  Subordinate link state: {:?}", link_subordinate.state());
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
