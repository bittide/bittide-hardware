#![no_std]
#![cfg_attr(not(test), no_main)]
#![feature(sync_unsafe_cell)]

// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::hals::async_comms_demo_management_unit::devices::CaptureUgns;
use bittide_hal::hals::async_comms_demo_management_unit::DeviceInstances;
use bittide_hal::manual_additions::ring_buffer::AlignedReceiveBuffer;
use bittide_hal::manual_additions::timer::Duration;
use bittide_sys::hitl::{FPGA_DNAS, LINK_COUNT, LINK_NEIGHBORS};
use bittide_sys::link_startup::LinkStartup;
use bittide_sys::net_state::{UgnEdge, UgnReport};
use bittide_sys::smoltcp::link_interface::LinkInterface;
use bittide_sys::smoltcp::link_protocol::{Command, CommandWire, UgnEdgeWire};
use core::fmt::Write;
use log::{debug, error, info, warn, LevelFilter};
use riscv::register::{mcause, mepc, mtval};
use smoltcp::iface::SocketStorage;
use ufmt::uwriteln;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

/// Size of the smoltcp TCP socket buffers (in bytes). Must be large enough to
/// hold the largest message exchanged in this demo (a `UgnEdgeWire` is 40 bytes).
const TCP_BUFFER_SIZE: usize = 256;

/// The FPGA designated as manager: rig position 6 (FPGA id 210308B3A22D).
const MANAGER_DNA: [u8; 12] = FPGA_DNAS[6];

#[cfg(not(test))]
use riscv_rt::entry;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    unsafe {
        use bittide_sys::uart::log::LOGGER;
        let logger = &mut (*LOGGER.get());
        logger.set_logger(uart.clone());
        logger.set_timer(INSTANCES.timer);
        logger.display_source = LevelFilter::Warn;
        log::set_logger_racy(logger).ok();
        log::set_max_level_racy(LevelFilter::Debug);
    }

    info!("=== Async Comms Demo MU ===");
    let transceivers = &INSTANCES.transceivers;
    let timer = INSTANCES.timer;
    let handshakes = &INSTANCES.handshakes;
    let cc = INSTANCES.clock_control;
    let elastic_buffers = [
        &INSTANCES.elastic_buffer_0,
        &INSTANCES.elastic_buffer_1,
        &INSTANCES.elastic_buffer_2,
        &INSTANCES.elastic_buffer_3,
        &INSTANCES.elastic_buffer_4,
        &INSTANCES.elastic_buffer_5,
        &INSTANCES.elastic_buffer_6,
    ];
    let capture_ugns = &INSTANCES.capture_ugns;
    // Pseudocode setup:
    // 1) Initialize MU peripherals and scatter/gather calendars for ring_buffers.
    // 2) Run LinkStartup per port to bring up physical links and capture UGNs.
    // 3) Wait for clock stability; stop auto-centering and record EB deltas.
    // 4) Align ring_buffers on all ports (two-phase protocol).
    // 5) For each port, create RingBufferDevice + smoltcp Interface with static IP.
    // 6) Exchange DNAs and ports with all neighbors over TCP.
    // 7) Collect UGN edges over TCP and aggregate at the manager.

    info!("Bringing up links...");
    let mut link_startups = [LinkStartup::new(); LINK_COUNT];
    let timeout = timer.now() + Duration::from_secs(10);
    while timer.now() <= timeout && !link_startups.iter().all(|ls| ls.is_done()) {
        for (i, link_startup) in link_startups.iter_mut().enumerate() {
            link_startup.next(transceivers, handshakes, i, elastic_buffers[i]);
        }
    }

    let mut unstarted_links = 0;
    for (i, ls) in link_startups.iter().enumerate() {
        if !ls.is_done() {
            error!("Link {} did not start", i);
            unstarted_links += 1;
        }
    }
    if unstarted_links > 0 {
        panic!("Some links did not start")
    }
    info!("Waiting for stability...");
    let all_links_stable: u8 = (1 << LINK_COUNT) - 1;
    let timeout = timer.now() + Duration::from_secs(10);
    loop {
        let stable = cc.links_stable()[0];
        if stable == all_links_stable {
            break;
        }
        if timer.now() > timeout {
            panic!("Links did not become stable in time.")
        }
    }

    // Auto-centering is disabled from here on and never re-enabled, making this demo a
    // "happy path": it only works while the clock frequencies stay close together. In a
    // real system we want to do async comms while the system is still synchronizing,
    // which requires either hardware accelerated alignment or continuously polling and
    // accounting for auto-center activity. That is planned for a follow-up, see
    // https://github.com/bittide/bittide-hardware/issues/1173.
    info!("Stopping auto-centering...");
    elastic_buffers
        .iter()
        .for_each(|eb| eb.set_auto_center_enable(false));
    elastic_buffers
        .iter()
        .for_each(|eb| eb.wait_auto_center_idle());
    let eb_deltas = elastic_buffers
        .iter()
        .map(|eb| eb.auto_center_total_adjustments());

    for (i, eb_delta) in eb_deltas.enumerate() {
        capture_ugns.set_elastic_buffer_delta(i, eb_delta).unwrap();
    }

    info!("Captured hardware UGNs");
    for i in 0..LINK_COUNT {
        debug!(
            "  UGN {}: local={}, remote={}",
            i,
            capture_ugns.local_counter(i).unwrap(),
            capture_ugns.remote_counter(i).unwrap()
        );
    }

    // Create rx/tx buffer arrays
    let rx_buffers = [
        INSTANCES.receive_ring_buffer_0,
        INSTANCES.receive_ring_buffer_1,
        INSTANCES.receive_ring_buffer_2,
        INSTANCES.receive_ring_buffer_3,
        INSTANCES.receive_ring_buffer_4,
        INSTANCES.receive_ring_buffer_5,
        INSTANCES.receive_ring_buffer_6,
    ];
    let tx_buffers = [
        INSTANCES.transmit_ring_buffer_0,
        INSTANCES.transmit_ring_buffer_1,
        INSTANCES.transmit_ring_buffer_2,
        INSTANCES.transmit_ring_buffer_3,
        INSTANCES.transmit_ring_buffer_4,
        INSTANCES.transmit_ring_buffer_5,
        INSTANCES.transmit_ring_buffer_6,
    ];

    let dna = INSTANCES.dna.dna();
    let is_manager = dna == MANAGER_DNA;
    writeln!(
        uart,
        "DNA: {dna:?}, Role: {}",
        if is_manager { "manager" } else { "subordinate" }
    )
    .unwrap();

    // Create aligned receive buffers for all links and align them
    let start_time = INSTANCES.timer.now();
    let mut rx_aligned = rx_buffers.map(AlignedReceiveBuffer::new);
    while !rx_aligned
        .iter_mut()
        .zip(tx_buffers.iter())
        .all(|(rx, tx)| rx.align_step(tx))
    {}
    let stop_time = INSTANCES.timer.now();
    uwriteln!(
        uart,
        "Aligning all buffers took {}.",
        (stop_time - start_time)
    )
    .unwrap();

    // Create all LinkInterfaces
    info!("Creating LinkInterfaces");
    let make_timer = || unsafe { bittide_hal::shared_devices::Timer::new(INSTANCES.timer.0) };

    // TCP buffers, borrowed by the LinkInterfaces for their whole lifetime
    let mut tcp_rx_buffers = [[0u8; TCP_BUFFER_SIZE]; LINK_COUNT];
    let mut tcp_tx_buffers = [[0u8; TCP_BUFFER_SIZE]; LINK_COUNT];
    let mut socket_storage = [const { [SocketStorage::EMPTY; 1] }; LINK_COUNT];
    let mut links: heapless::Vec<LinkInterface<_, _>, { LINK_COUNT }> = rx_aligned
        .into_iter()
        .zip(tx_buffers)
        .zip(tcp_rx_buffers.iter_mut())
        .zip(tcp_tx_buffers.iter_mut())
        .zip(socket_storage.iter_mut())
        .map(|((((rx, tx), rx_buf), tx_buf), storage)| {
            LinkInterface::new(rx, tx, make_timer(), rx_buf, tx_buf, storage)
        })
        .collect();

    // Step 1: Wait for all connections to establish
    uwriteln!(uart, "Step 1: Waiting for all connections to establish...").unwrap();
    loop {
        links.iter_mut().for_each(|l| l.poll());
        if links.iter().all(|l| l.is_established()) {
            break;
        }
    }
    uwriteln!(uart, "  All {} links established", LINK_COUNT).unwrap();

    // Step 2: Exchange DNA with all neighbors
    info!("Step 2: Exchanging DNA and ports with all neighbors...");
    debug!("  Our DNA: {:02X?}", dna);

    // Prepare DNA and port data to send
    let port_bytes: [[u8; 4]; LINK_COUNT] = core::array::from_fn(|i| (i as u32).to_le_bytes());

    // Track what we've sent and received
    let mut dna_sent = [false; LINK_COUNT];
    let mut port_sent = [false; LINK_COUNT];
    let mut partner_dnas: [Option<[u8; 12]>; LINK_COUNT] = [None; LINK_COUNT];
    let mut partner_ports = [0u32; LINK_COUNT];
    let mut port_received = [false; LINK_COUNT];

    uwriteln!(
        uart,
        "  Waiting to receive DNA and ports from all neighbors..."
    )
    .unwrap();

    let deadline = timer.now() + Duration::from_secs(1);
    while timer.now() < deadline {
        for (i, link) in links.iter_mut().enumerate() {
            link.poll();

            // Try receiving partner DNA if we haven't already
            if partner_dnas[i].is_none() {
                let mut dna_buf = [0u8; 12];
                match link.try_recv_bytes(&mut dna_buf) {
                    Ok(12) => {
                        partner_dnas[i] = Some(dna_buf);
                        debug!("  Link {}: received partner DNA: {:02X?}", i, dna_buf);
                    }
                    Ok(n) if n > 0 => {
                        error!("  Link {}: partial DNA recv: {} bytes", i, n);
                    }
                    _ => {}
                }
            }

            // Try receiving partner port if we received their DNA and haven't received their port yet
            if partner_dnas[i].is_some() && !port_received[i] {
                if let Ok(port) = link.try_recv::<u32>() {
                    partner_ports[i] = port;
                    port_received[i] = true;
                    debug!("  Link {}: received partner port: {}", i, partner_ports[i]);
                }
            }

            // Try sending our DNA if we haven't already
            if !dna_sent[i] && link.try_send_bytes(&dna).is_ok() {
                debug!("  Link {}: sent our DNA", i);
                dna_sent[i] = true;
            }

            // Try sending our port if we sent our DNA and haven't sent our port yet.
            if dna_sent[i] && !port_sent[i] && link.try_send_bytes(&port_bytes[i]).is_ok() {
                debug!("  Link {}: sent our port {}", i, i);
                port_sent[i] = true;
            }
        }
        if partner_dnas.iter().all(|dna| dna.is_some()) && port_received.iter().all(|&r| r) {
            info!("  DNA and port exchange complete for all links!");
            break;
        }
    }

    let dna_success_count = partner_dnas.iter().filter(|dna| dna.is_some()).count();
    if dna_success_count == LINK_COUNT {
        info!(
            "  DNA and port exchange complete for all {} links",
            LINK_COUNT
        );
    } else {
        error!(
            "  Only received DNA from {} of {} links",
            dna_success_count, LINK_COUNT
        );
        for (i, dna) in partner_dnas.iter().enumerate() {
            if dna.is_none() {
                error!("    Link {}: no DNA received", i);
            }
        }
    }

    // Step 2b: Verify the exchanged DNAs and ports against the known demo rig topology
    info!("Step 2b: Verifying partner DNAs against demo rig topology...");
    verify_partner_dnas(&dna, &partner_dnas, &partner_ports);

    // Step 3: Build complete UGN report from all neighbor information
    info!("Step 3: Building UGN report...");

    let partner_info: [Option<(&[u8; 12], u32)>; LINK_COUNT] =
        core::array::from_fn(|i| partner_dnas[i].as_ref().map(|dna| (dna, partner_ports[i])));

    let mut report: UgnReport = build_complete_report(&dna, &partner_info, capture_ugns);
    info!("  Built report with {} edges", report.count);

    // Step 4: Execute role-specific protocol
    if is_manager {
        info!("Step 4: Manager collecting reports from subordinates...");

        for (i, link) in links.iter_mut().enumerate() {
            let wire_command: CommandWire = Command::RequestUgnReport.into();
            if let Err(e) = link.send_blocking(&wire_command, Duration::from_secs(1)) {
                error!("  Link {}: failed to send command: {:?}", i, e);
                continue;
            }
            let count: u32 = match link.recv_blocking(Duration::from_secs(1)) {
                Ok(c) => c,
                Err(e) => {
                    error!("  Link {}: failed to receive count: {:?}", i, e);
                    continue;
                }
            };
            debug!("  Link {}: expecting {} edges", i, count);
            for j in 0..count {
                let edge: UgnEdgeWire = match link.recv_blocking(Duration::from_secs(1)) {
                    Ok(e) => e,
                    Err(e) => {
                        error!("  Link {}: failed to receive edge {}: {:?}", i, j, e);
                        break;
                    }
                };
                report.insert_edge(edge.into());
            }
        }
        info!("  Complete UGN graph: {} edges", report.count);
        writeln!(uart, "  Final UGN Report: {:?}", report).unwrap();
    } else {
        // Subordinate role: wait for manager command, then send report
        info!("Step 4: Subordinate waiting for manager command...");
        let mut manager_idx = None;

        while manager_idx.is_none() {
            for (i, link) in links.iter_mut().enumerate() {
                link.poll();
                if let Ok(wire) = link.try_recv::<CommandWire>() {
                    if let Ok(Command::RequestUgnReport) = Command::try_from(wire) {
                        info!("  Received RequestUgnReport on link {}", i);
                        manager_idx = Some(i);
                        break;
                    } else {
                        warn!("  Link {}: received unknown command: {:?}", i, wire);
                    }
                }
            }
        }

        let manager_link = &mut links[manager_idx.unwrap()];
        manager_link
            .send_blocking(&report.count, Duration::from_secs(1))
            .expect("Failed to send count");

        let mut edges_sent = 0;
        let mut edge_idx = 0;
        while edges_sent < report.count as usize && edge_idx < report.edges.len() {
            manager_link.poll();
            if let Some(edge) = report.edges[edge_idx] {
                let wire: UgnEdgeWire = edge.into();
                if manager_link.try_send(&wire).is_ok() {
                    edges_sent += 1;
                    edge_idx += 1;
                }
            } else {
                edge_idx += 1;
            }
        }
        info!("  Sent {} edges to manager", edges_sent);
    }

    uwriteln!(uart, "Demo complete.").unwrap();
    // Keep polling links to ensure all transmissions complete
    loop {
        links.iter_mut().for_each(|l| l.poll());
    }
}

#[panic_handler]
fn panic_handler(info: &core::panic::PanicInfo) -> ! {
    let mut uart = INSTANCES.uart;
    writeln!(uart, "Panicked! #{info}").unwrap();
    loop {
        continue;
    }
}

#[export_name = "ExceptionHandler"]
fn exception_handler(_trap_frame: &riscv_rt::TrapFrame) -> ! {
    let mut uart = INSTANCES.uart;
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

/// Check the DNAs and ports received from our neighbors against the known demo
/// rig topology (`FPGA_DNAS` / `LINK_NEIGHBORS`), so wiring or alignment problems
/// show up immediately instead of producing a silently wrong UGN graph.
fn verify_partner_dnas(
    own_dna: &[u8; 12],
    partner_dnas: &[Option<[u8; 12]>; LINK_COUNT],
    partner_ports: &[u32; LINK_COUNT],
) {
    let Some(own_idx) = FPGA_DNAS.iter().position(|d| d == own_dna) else {
        error!("  Own DNA {:02X?} not found in demo rig table", own_dna);
        return;
    };
    let mut ok = true;
    for (link, partner_dna) in partner_dnas.iter().enumerate() {
        let Some(partner_dna) = partner_dna else {
            ok = false;
            continue;
        };
        let neighbor = LINK_NEIGHBORS[own_idx][link];
        if *partner_dna != FPGA_DNAS[neighbor] {
            error!(
                "  Link {}: expected DNA of rig FPGA {} ({:02X?}), got {:02X?}",
                link, neighbor, FPGA_DNAS[neighbor], partner_dna
            );
            ok = false;
        }
        // The partner should have reported the port that connects back to us.
        let expected_port = LINK_NEIGHBORS[neighbor]
            .iter()
            .position(|&n| n == own_idx)
            .unwrap() as u32;
        if partner_ports[link] != expected_port {
            error!(
                "  Link {}: expected partner port {}, got {}",
                link, expected_port, partner_ports[link]
            );
            ok = false;
        }
    }
    if ok {
        info!("  All partner DNAs and ports match the demo rig topology");
    } else {
        error!("  Partner DNA/port verification FAILED");
    }
}

fn build_complete_report(
    dna: &[u8; 12],
    partner_info: &[Option<(&[u8; 12], u32)>; LINK_COUNT],
    capture_ugns: &CaptureUgns,
) -> UgnReport {
    let mut report = UgnReport::new();
    for (link_idx, partner) in partner_info.iter().enumerate() {
        if let Some((partner_dna, partner_port)) = *partner {
            report.count += 1;
            let ugn = capture_ugns.local_counter(link_idx).unwrap().into_inner() as i64
                - capture_ugns.remote_counter(link_idx).unwrap().into_inner() as i64;
            report.edges[link_idx] = Some(UgnEdge {
                src_node: *dna,
                src_port: link_idx as u32,
                dst_node: *partner_dna,
                dst_port: partner_port,
                ugn,
            });
        }
    }

    report
}
