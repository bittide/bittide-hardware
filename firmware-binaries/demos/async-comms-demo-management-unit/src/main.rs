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
use bittide_sys::link_startup::LinkStartup;
use bittide_sys::net_state::{UgnEdge, UgnReport};
use bittide_sys::smoltcp::link_interface::LinkInterface;
use bittide_sys::smoltcp::link_protocol::{Command, CommandWire, NodeCorrectionWire, UgnEdgeWire};
use bittide_sys::ugn_grooming::RelabelResult;
use core::fmt::Write;
use log::{debug, error, info, warn, LevelFilter};
use riscv::register::{mcause, mepc, mtval};
use ufmt::uwriteln;

mod grooming;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };
const LINK_COUNT: usize = 7;
const MANAGER_DNA: [u8; 12] = [133, 129, 48, 4, 64, 192, 105, 1, 1, 0, 2, 64];

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
    // 2) Align ring_buffers on all ports (two-phase protocol).
    // 3) Run LinkStartup per port to bring up physical links and capture UGNs.
    // 4) Wait for clock stability; stop auto-centering and record EB deltas.
    // 5) For each port, create RingBufferDevice + smoltcp Interface with static IP.
    // 6) Run manager state machines to connect to neighbors and request UGNs.
    // 7) Collect UGN edges over TCP and aggregate locally.

    info!("Bringing up links...");
    let mut link_startups = [LinkStartup::new(); LINK_COUNT];
    while !link_startups.iter().all(|ls| ls.is_done()) {
        for (i, link_startup) in link_startups.iter_mut().enumerate() {
            link_startup.next(transceivers, handshakes, i, elastic_buffers[i]);
        }
    }

    info!("Waiting for stability...");
    loop {
        let stable = cc.links_stable()[0];
        if stable != 0 {
            break;
        }
    }

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
    let timer = INSTANCES.timer;
    let make_timer = || unsafe { bittide_hal::shared_devices::Timer::new(INSTANCES.timer.0) };

    let mut links: heapless::Vec<LinkInterface<_, _, 256>, LINK_COUNT> = rx_aligned
        .into_iter()
        .zip(tx_buffers)
        .map(|(rx, tx)| LinkInterface::new(rx, tx, make_timer()))
        .collect();
    for link in links.iter_mut() {
        link.connect();
    }

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

        // Step 5: groom the full graph in-band and distribute each node's correction.
        info!("Step 5: Grooming UGN graph in-band...");
        match grooming::groom_report(&report, dna) {
            RelabelResult::Infeasible(cycle) => {
                error!(
                    "  UGN grooming infeasible: negative slack cycle through {} nodes",
                    cycle.len()
                );
            }
            RelabelResult::Feasible(plan) => {
                // Relabel reference in the local-counter domain: now + ~1s headroom, which
                // dominates inter-node boot offsets and the distribution time so no node's
                // release lands in the past.
                INSTANCES.timer.freeze();
                let now: u64 = INSTANCES.timer.scratchpad().into();
                let headroom: u64 = INSTANCES.timer.frequency().into();
                let shared_base = now + headroom;

                for (i, link) in links.iter_mut().enumerate() {
                    let Some(node) = partner_dnas[i] else {
                        continue;
                    };
                    let correction = grooming::node_correction(&plan, &node, shared_base);
                    let cmd: CommandWire = Command::ApplyCorrection.into();
                    if link.send_blocking(&cmd, Duration::from_secs(1)).is_err()
                        || link
                            .send_blocking(&correction, Duration::from_secs(1))
                            .is_err()
                    {
                        error!("  Link {}: failed to send correction", i);
                        continue;
                    }
                    writeln!(
                        uart,
                        "  Correction sent to {:02X?}: release_cycle={}",
                        node,
                        correction.release_cycle()
                    )
                    .unwrap();
                }

                // Apply the manager's own correction.
                let own = grooming::node_correction(&plan, &dna, shared_base);
                grooming::apply(&elastic_buffers, &INSTANCES.timed_reset, &own);
                writeln!(
                    uart,
                    "  Correction applied locally: release_cycle={}",
                    own.release_cycle()
                )
                .unwrap();
                uwriteln!(uart, "Grooming applied.").unwrap();
            }
        }
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

        // Step 5: receive the manager's correction and apply it.
        info!("Step 5: Waiting for grooming correction from manager...");
        loop {
            manager_link.poll();
            if let Ok(wire) = manager_link.try_recv::<CommandWire>() {
                match Command::try_from(wire) {
                    Ok(Command::ApplyCorrection) => break,
                    other => warn!(
                        "  Unexpected command while awaiting correction: {:?}",
                        other
                    ),
                }
            }
        }
        let correction: NodeCorrectionWire = manager_link
            .recv_blocking(Duration::from_secs(1))
            .expect("Failed to receive correction");
        grooming::apply(&elastic_buffers, &INSTANCES.timed_reset, &correction);
        writeln!(
            uart,
            "  Correction applied: release_cycle={}",
            correction.release_cycle()
        )
        .unwrap();
        uwriteln!(uart, "Grooming applied.").unwrap();
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
