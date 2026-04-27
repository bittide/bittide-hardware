#![no_std]
#![cfg_attr(not(test), no_main)]
#![feature(sync_unsafe_cell)]

// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::hals::soft_ugn_demo_mu::DeviceInstances;
use bittide_hal::manual_additions::ring_buffer::AlignedReceiveBuffer;
use bittide_hal::manual_additions::timer::Duration;
use bittide_sys::link_startup::LinkStartup;
use bittide_sys::smoltcp::link_interface::LinkInterface;
use bittide_sys::smoltcp::link_protocol::{Command, UgnEdge, UgnReport};
use core::fmt::Write;
use log::{debug, error, info, warn, LevelFilter};
use riscv::register::{mcause, mepc, mtval};
use ufmt::uwriteln;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };
const LINKS: [usize; 3] = [0, 1, 6];
const LINK_COUNT: usize = LINKS.len();
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

    info!("=== Soft UGN Demo MU2 ===");
    let transceivers = &INSTANCES.transceivers;
    let cc = INSTANCES.clock_control;
    let elastic_buffers = [
        &INSTANCES.elastic_buffer_0,
        &INSTANCES.elastic_buffer_1,
        &INSTANCES.elastic_buffer_2,
    ];
    let capture_ugns = [
        INSTANCES.capture_ugn_0,
        INSTANCES.capture_ugn_1,
        INSTANCES.capture_ugn_2,
    ];
    info!("Bringing up links...");
    let mut link_startups = [LinkStartup::new(); LINK_COUNT];
    while !link_startups.iter().all(|ls| ls.is_done()) {
        for (i, link_startup) in link_startups.iter_mut().enumerate() {
            link_startup.next(
                transceivers,
                LINKS[i],
                elastic_buffers[i],
                capture_ugns[i].has_captured(),
            );
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

    for (capture_ugn, eb_delta) in capture_ugns.iter().zip(eb_deltas) {
        capture_ugn.set_elastic_buffer_delta(eb_delta);
    }

    info!("Captured hardware UGNs");
    for (i, capture_ugn) in capture_ugns.iter().enumerate() {
        debug!(
            "  UGN {}: local={}, remote={}",
            i,
            capture_ugn.local_counter(),
            capture_ugn.remote_counter()
        );
    }

    let rx_buffers = [
        INSTANCES.receive_ring_buffer_0,
        INSTANCES.receive_ring_buffer_1,
        INSTANCES.receive_ring_buffer_2,
    ];
    let tx_buffers = [
        INSTANCES.transmit_ring_buffer_0,
        INSTANCES.transmit_ring_buffer_1,
        INSTANCES.transmit_ring_buffer_2,
    ];

    let dna = INSTANCES.dna.dna();
    let is_manager = dna == MANAGER_DNA;
    writeln!(
        uart,
        "DNA: {dna:?}, Role: {}",
        if is_manager { "manager" } else { "subordinate" }
    )
    .unwrap();

    // Align ring_buffers on all links
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

            // Try to receive partner DNA
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

            // Try to receive partner port (only after DNA)
            if partner_dnas[i].is_some() && !port_received[i] {
                if let Ok(port) = link.try_recv() {
                    partner_ports[i] = port;
                    port_received[i] = true;
                    debug!("  Link {}: received partner port: {}", i, partner_ports[i]);
                }
            }

            // Try to send our DNA
            if !dna_sent[i] && link.try_send_bytes(&dna).is_ok() {
                debug!("  Link {}: sent our DNA", i);
                dna_sent[i] = true;
            }

            // Try to send our port (only after DNA)
            if dna_sent[i] && !port_sent[i] {
                debug!("  Link {}: sent our port {}", i, i);
                port_sent[i] = link.try_send(&i).is_ok();
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

    info!("Step 3: Building UGN report...");

    let partner_info: [Option<(&[u8; 12], u32)>; LINK_COUNT] =
        core::array::from_fn(|i| partner_dnas[i].as_ref().map(|dna| (dna, partner_ports[i])));

    let mut report: UgnReport = build_complete_report(&dna, &partner_info, &capture_ugns);
    info!("  Built report with {} edges", report.count);

    // Execute role-specific UGN collection protocol
    if is_manager {
        info!("Step 4: Manager collecting reports from subordinates...");

        for (i, link) in links.iter_mut().enumerate() {
            link.send_blocking(&Command::RequestUgnReport, Duration::from_secs(1))
                .expect("Failed to send RequestUgnReport command");
            let count: u32 = match link.recv_blocking(Duration::from_secs(1)) {
                Ok(c) => c,
                Err(e) => {
                    error!("  Link {}: failed to receive count: {:?}", i, e);
                    continue;
                }
            };
            debug!("  Link {}: expecting {} edges", i, count);
            for j in 0..count {
                match link.recv_blocking::<UgnEdge>(Duration::from_secs(1)) {
                    Ok(edge) => report.insert_edge(edge),
                    Err(e) => {
                        error!("  Link {}: failed to receive edge {}: {:?}", i, j, e);
                        break;
                    }
                };
            }
        }
        info!("  Complete UGN graph: {} edges", report.count);
        writeln!(uart, "  Final UGN Report: {:?}", report).unwrap();
    } else {
        info!("Step 4: Subordinate waiting for manager command...");
        let mut manager_idx = None;

        while manager_idx.is_none() {
            for (i, link) in links.iter_mut().enumerate() {
                link.poll();
                match link.try_recv::<Command>() {
                    Ok(Command::RequestUgnReport) => {
                        info!("  Received RequestUgnReport on link {}", i);
                        manager_idx = Some(i);
                        break;
                    }
                    Err(e) => {
                        warn!("  Link {}: received unknown command: {:?}", i, e);
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
                if manager_link.try_send(&edge).is_ok() {
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
    // Keep polling to flush remaining TCP transmissions
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
    capture_ugns: &[bittide_hal::shared_devices::CaptureUgn; LINK_COUNT],
) -> UgnReport {
    let mut report = UgnReport::new();
    for link_idx in 0..LINK_COUNT {
        if let Some((partner_dna, partner_port)) = partner_info[link_idx] {
            report.count += 1;
            let ugn = capture_ugns[link_idx].local_counter() as i64
                - capture_ugns[link_idx].remote_counter() as i64;
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
