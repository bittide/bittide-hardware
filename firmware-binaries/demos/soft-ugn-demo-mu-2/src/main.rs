#![no_std]
#![cfg_attr(not(test), no_main)]
#![feature(sync_unsafe_cell)]

// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::hals::soft_ugn_demo_mu::DeviceInstances;
use bittide_hal::manual_additions::ringbuffer::AlignedReceiveBuffer;
use bittide_hal::manual_additions::timer::Duration;
use bittide_sys::link_startup::LinkStartup;
use bittide_sys::net_state::{UgnEdge, UgnReport};
use bittide_sys::smoltcp::link_interface::{LinkBuffers, LinkConfig, LinkInterface};
use bittide_sys::smoltcp::link_protocol::{Command, CommandWire, UgnEdgeWire};
use core::cell::SyncUnsafeCell;
use core::fmt::Write;
use log::{info, error, LevelFilter};
use riscv::register::{mcause, mepc, mtval};
use smoltcp::iface::SocketStorage;
use smoltcp::wire::IpAddress;
use ufmt::uwriteln;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };
const LINK_COUNT: usize = 7;
const MANAGER_DNA: [u8; 12] = [133, 129, 48, 4, 64, 192, 105, 1, 1, 0, 2, 64];
const PORT: u16 = 8080;

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
        &INSTANCES.elastic_buffer_3,
        &INSTANCES.elastic_buffer_4,
        &INSTANCES.elastic_buffer_5,
        &INSTANCES.elastic_buffer_6,
    ];
    let capture_ugns = [
        INSTANCES.capture_ugn_0,
        INSTANCES.capture_ugn_1,
        INSTANCES.capture_ugn_2,
        INSTANCES.capture_ugn_3,
        INSTANCES.capture_ugn_4,
        INSTANCES.capture_ugn_5,
        INSTANCES.capture_ugn_6,
    ];
    // Pseudocode setup:
    // 1) Initialize MU peripherals and scatter/gather calendars for ringbuffers.
    // 2) Align ringbuffers on all ports (two-phase protocol).
    // 3) Run LinkStartup per port to bring up physical links and capture UGNs.
    // 4) Wait for clock stability; stop auto-centering and record EB deltas.
    // 5) For each port, create RingbufferDevice + smoltcp Interface with static IP.
    // 6) Run manager state machines to connect to neighbors and request UGNs.
    // 7) Collect UGN edges over TCP and aggregate locally.

    info!("Bringing up links...");
    let mut link_startups = [LinkStartup::new(); LINK_COUNT];
    while !link_startups.iter().all(|ls| ls.is_done()) {
        for (i, link_startup) in link_startups.iter_mut().enumerate() {
            link_startup.next(
                transceivers,
                i,
                elastic_buffers[i],
                capture_ugns[i].has_captured(),
            );
        }
    }

    info!("Waiting for stability...");
    let _cc = INSTANCES.clock_control;
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
        info!(
            "Capture UGN {}: local = {}, remote = {}",
            i,
            capture_ugn.local_counter(),
            capture_ugn.remote_counter()
        );
    }

    // Create rx/tx buffer arrays
    let rx_buffers = [
        INSTANCES.receive_ringbuffer_0,
        INSTANCES.receive_ringbuffer_1,
        INSTANCES.receive_ringbuffer_2,
        INSTANCES.receive_ringbuffer_3,
        INSTANCES.receive_ringbuffer_4,
        INSTANCES.receive_ringbuffer_5,
        INSTANCES.receive_ringbuffer_6,
    ];
    let tx_buffers = [
        INSTANCES.transmit_ringbuffer_0,
        INSTANCES.transmit_ringbuffer_1,
        INSTANCES.transmit_ringbuffer_2,
        INSTANCES.transmit_ringbuffer_3,
        INSTANCES.transmit_ringbuffer_4,
        INSTANCES.transmit_ringbuffer_5,
        INSTANCES.transmit_ringbuffer_6,
    ];

    // Create socket storage (one socket per link)
    static SOCKET_STORAGE_0: SyncUnsafeCell<[SocketStorage; 1]> =
        SyncUnsafeCell::new([SocketStorage::EMPTY; 1]);
    static SOCKET_STORAGE_1: SyncUnsafeCell<[SocketStorage; 1]> =
        SyncUnsafeCell::new([SocketStorage::EMPTY; 1]);
    static SOCKET_STORAGE_2: SyncUnsafeCell<[SocketStorage; 1]> =
        SyncUnsafeCell::new([SocketStorage::EMPTY; 1]);
    static SOCKET_STORAGE_3: SyncUnsafeCell<[SocketStorage; 1]> =
        SyncUnsafeCell::new([SocketStorage::EMPTY; 1]);
    static SOCKET_STORAGE_4: SyncUnsafeCell<[SocketStorage; 1]> =
        SyncUnsafeCell::new([SocketStorage::EMPTY; 1]);
    static SOCKET_STORAGE_5: SyncUnsafeCell<[SocketStorage; 1]> =
        SyncUnsafeCell::new([SocketStorage::EMPTY; 1]);
    static SOCKET_STORAGE_6: SyncUnsafeCell<[SocketStorage; 1]> =
        SyncUnsafeCell::new([SocketStorage::EMPTY; 1]);

    static TCP_RX_0: SyncUnsafeCell<[u8; 256]> = SyncUnsafeCell::new([0; 256]);
    static TCP_TX_0: SyncUnsafeCell<[u8; 256]> = SyncUnsafeCell::new([0; 256]);
    static TCP_RX_1: SyncUnsafeCell<[u8; 256]> = SyncUnsafeCell::new([0; 256]);
    static TCP_TX_1: SyncUnsafeCell<[u8; 256]> = SyncUnsafeCell::new([0; 256]);
    static TCP_RX_2: SyncUnsafeCell<[u8; 256]> = SyncUnsafeCell::new([0; 256]);
    static TCP_TX_2: SyncUnsafeCell<[u8; 256]> = SyncUnsafeCell::new([0; 256]);
    static TCP_RX_3: SyncUnsafeCell<[u8; 256]> = SyncUnsafeCell::new([0; 256]);
    static TCP_TX_3: SyncUnsafeCell<[u8; 256]> = SyncUnsafeCell::new([0; 256]);
    static TCP_RX_4: SyncUnsafeCell<[u8; 256]> = SyncUnsafeCell::new([0; 256]);
    static TCP_TX_4: SyncUnsafeCell<[u8; 256]> = SyncUnsafeCell::new([0; 256]);
    static TCP_RX_5: SyncUnsafeCell<[u8; 256]> = SyncUnsafeCell::new([0; 256]);
    static TCP_TX_5: SyncUnsafeCell<[u8; 256]> = SyncUnsafeCell::new([0; 256]);
    static TCP_RX_6: SyncUnsafeCell<[u8; 256]> = SyncUnsafeCell::new([0; 256]);
    static TCP_TX_6: SyncUnsafeCell<[u8; 256]> = SyncUnsafeCell::new([0; 256]);

    let dna = INSTANCES.dna.dna();
    info!("My DNA: {:?}", dna);
    let is_manager = dna == MANAGER_DNA;
    info!(
        "Role: {}",
        if is_manager { "manager" } else { "subordinate" }
    );

    let ip = IpAddress::v4(100, 100, 100, 100);

    // Create aligned receive buffers for all links and align them
    let mut rx_aligned = rx_buffers.map(AlignedReceiveBuffer::new);
    for (rx_aligned, tx) in rx_aligned.iter_mut().zip(tx_buffers.iter()) {
        rx_aligned.align(tx);
    }

    // Helper function to get static buffers for a given link index
    let get_buffers = |idx: usize| -> LinkBuffers<'static> {
        match idx {
            0 => LinkBuffers {
                socket_storage: unsafe { &mut *SOCKET_STORAGE_0.get() },
                tcp_rx_buffer: unsafe { &mut *TCP_RX_0.get() },
                tcp_tx_buffer: unsafe { &mut *TCP_TX_0.get() },
            },
            1 => LinkBuffers {
                socket_storage: unsafe { &mut *SOCKET_STORAGE_1.get() },
                tcp_rx_buffer: unsafe { &mut *TCP_RX_1.get() },
                tcp_tx_buffer: unsafe { &mut *TCP_TX_1.get() },
            },
            2 => LinkBuffers {
                socket_storage: unsafe { &mut *SOCKET_STORAGE_2.get() },
                tcp_rx_buffer: unsafe { &mut *TCP_RX_2.get() },
                tcp_tx_buffer: unsafe { &mut *TCP_TX_2.get() },
            },
            3 => LinkBuffers {
                socket_storage: unsafe { &mut *SOCKET_STORAGE_3.get() },
                tcp_rx_buffer: unsafe { &mut *TCP_RX_3.get() },
                tcp_tx_buffer: unsafe { &mut *TCP_TX_3.get() },
            },
            4 => LinkBuffers {
                socket_storage: unsafe { &mut *SOCKET_STORAGE_4.get() },
                tcp_rx_buffer: unsafe { &mut *TCP_RX_4.get() },
                tcp_tx_buffer: unsafe { &mut *TCP_TX_4.get() },
            },
            5 => LinkBuffers {
                socket_storage: unsafe { &mut *SOCKET_STORAGE_5.get() },
                tcp_rx_buffer: unsafe { &mut *TCP_RX_5.get() },
                tcp_tx_buffer: unsafe { &mut *TCP_TX_5.get() },
            },
            6 => LinkBuffers {
                socket_storage: unsafe { &mut *SOCKET_STORAGE_6.get() },
                tcp_rx_buffer: unsafe { &mut *TCP_RX_6.get() },
                tcp_tx_buffer: unsafe { &mut *TCP_TX_6.get() },
            },
            _ => panic!("Invalid link index"),
        }
    };

    // Destructure arrays to allow moving individual elements
    let [rx0, rx1, rx2, rx3, rx4, rx5, rx6] = rx_aligned;
    let [tx0, tx1, tx2, tx3, tx4, tx5, tx6] = tx_buffers;

    // Create all LinkInterfaces
    info!("Creating LinkInterfaces with unique ports (8080-8086)...");
    let timer = INSTANCES.timer;
    let make_timer = || unsafe { bittide_hal::shared_devices::Timer::new(INSTANCES.timer.0) };

    let mut link0 = LinkInterface::new(
        rx0,
        tx0,
        LinkConfig::simultaneous_open(ip, ip, PORT),
        get_buffers(0),
        timer,
    );
    let mut link1 = LinkInterface::new(
        rx1,
        tx1,
        LinkConfig::simultaneous_open(ip, ip, PORT + 1),
        get_buffers(1),
        make_timer(),
    );
    let mut link2 = LinkInterface::new(
        rx2,
        tx2,
        LinkConfig::simultaneous_open(ip, ip, PORT + 2),
        get_buffers(2),
        make_timer(),
    );
    let mut link3 = LinkInterface::new(
        rx3,
        tx3,
        LinkConfig::simultaneous_open(ip, ip, PORT + 3),
        get_buffers(3),
        make_timer(),
    );
    let mut link4 = LinkInterface::new(
        rx4,
        tx4,
        LinkConfig::simultaneous_open(ip, ip, PORT + 4),
        get_buffers(4),
        make_timer(),
    );
    let mut link5 = LinkInterface::new(
        rx5,
        tx5,
        LinkConfig::simultaneous_open(ip, ip, PORT + 5),
        get_buffers(5),
        make_timer(),
    );
    let mut link6 = LinkInterface::new(
        rx6,
        tx6,
        LinkConfig::simultaneous_open(ip, ip, PORT + 6),
        get_buffers(6),
        make_timer(),
    );

    // Step 1: Wait for all connections to establish
    info!("Step 1: Waiting for all connections to establish...");
    let mut loop_count = 0;
    loop {
        // Poll all links
        link0.poll();
        link1.poll();
        link2.poll();
        link3.poll();
        link4.poll();
        link5.poll();
        link6.poll();

        loop_count += 1;
        if loop_count % 1000000 == 0 {
            let established = [
                link0.is_established(),
                link1.is_established(),
                link2.is_established(),
                link3.is_established(),
                link4.is_established(),
                link5.is_established(),
                link6.is_established(),
            ];
            let count = established.iter().filter(|&&e| e).count();
            info!("    Progress: {}/{} links established", count, LINK_COUNT);
            let link_refs = [&link0, &link1, &link2, &link3, &link4, &link5, &link6];
            for (idx, (&is_est, link)) in established.iter().zip(link_refs.iter()).enumerate() {
                if !is_est {
                    info!("      Link {}: state={:?}", idx, link.state());
                }
            }
        }

        if link0.is_established()
            && link1.is_established()
            && link2.is_established()
            && link3.is_established()
            && link4.is_established()
            && link5.is_established()
            && link6.is_established()
        {
            info!("  All {} links established!", LINK_COUNT);
            break;
        }
    }

    // Step 2: Exchange DNA with all neighbors
    info!("Step 2: Exchanging DNA and ports with all neighbors...");
    info!("  Our DNA: {:02X?}", dna);

    // Prepare DNA and port data to send
    let port_bytes: [[u8; 4]; LINK_COUNT] = core::array::from_fn(|i| (i as u32).to_le_bytes());

    // Track what we've sent and received
    let mut dna_sent = [false; LINK_COUNT];
    let mut port_sent = [false; LINK_COUNT];
    let mut partner_dnas: [Option<[u8; 12]>; LINK_COUNT] = [None; LINK_COUNT];
    let mut partner_ports = [0u32; LINK_COUNT];
    let mut port_received = [false; LINK_COUNT];

    info!("  Waiting to receive DNA and ports from all neighbors...");

    for _ in 0..10000 {
        for (i, link) in [
            &mut link0, &mut link1, &mut link2, &mut link3, &mut link4, &mut link5, &mut link6,
        ]
        .iter_mut()
        .enumerate()
        {
            link.poll();

            // Try receiving partner DNA if we haven't already
            let mut dna_buf = [0u8; 12];
            if partner_dnas[i].is_none() && link.try_recv_bytes(&mut dna_buf).is_ok() {
                partner_dnas[i] = Some(dna_buf);
                info!("  Link {}: received partner DNA: {:02X?}", i, dna_buf);
            }

            // Try receiving partner port if we received their DNA and haven't received their port yet
            if partner_dnas[i].is_some() && !port_received[i] {
                let mut port_buf = [0u8; 4];
                if link.try_recv_bytes(&mut port_buf).is_ok() {
                    partner_ports[i] = u32::from_le_bytes(port_buf);
                    port_received[i] = true;
                    info!("  Link {}: received partner port: {}", i, partner_ports[i]);
                }
            }

            // Try sending our DNA if we haven't already
            if !dna_sent[i] && link.try_send_bytes(&dna).is_ok() {
                info!("  Link {}: sent DNA", i);
                dna_sent[i] = true;
            }

            // Try sending our port if we sent our DNA and haven't sent our port yet.
            if dna_sent[i] && !port_sent[i] && link.try_send_bytes(&port_bytes[i]).is_ok() {
                info!("  Link {}: sent port={}", i, i);
                port_sent[i] = true;
            }
        }
        if partner_dnas.iter().all(|dna| dna.is_some()) {
            info!("  DNA and port exchange complete for all links!");
            break;
        }
    }

    let dna_success_count = partner_dnas.iter().filter(|dna| dna.is_some()).count();
    if dna_success_count != LINK_COUNT {
        info!(
            "  WARNING: Only received DNA from {} of {} links",
            dna_success_count,
            LINK_COUNT
        );
        for (i, dna) in partner_dnas.iter().enumerate() {
            if dna.is_none() {
                error!("    Link {}: no DNA received", i);
            }
        }
    }

    // Step 3: Build complete UGN report from all neighbor information
    info!("Step 3: Building complete UGN report from all neighbor information...");

    // Create partner_info array from collected data
    let partner_info: [Option<(&[u8; 12], u32)>; LINK_COUNT] =
        core::array::from_fn(|i| partner_dnas[i].as_ref().map(|dna| (dna, partner_ports[i])));

    let mut report: UgnReport = build_complete_report(&dna, &partner_info, &capture_ugns);

    info!("  Built report with {} edges for node", report.count);

    // Step 4: Execute role-specific protocol
    if is_manager {
        // Manager: Send commands to all links
        info!("Step 4a: Manager sending commands to all links...");

        for (i, partner_dna) in partner_dnas.iter().enumerate() {
            let subordinate_link = match i {
                0 => &mut link0,
                1 => &mut link1,
                2 => &mut link2,
                3 => &mut link3,
                4 => &mut link4,
                5 => &mut link5,
                6 => &mut link6,
                _ => unreachable!(),
            };
            info!(" Processing link {i} with DNA {:02X?}...", partner_dna);
            let wire_command: CommandWire = Command::RequestUgnReport.into();
            subordinate_link
                .send_blocking(&wire_command, Duration::from_secs(1))
                .expect("Failed to send command");
            let count: u32 = subordinate_link
                .recv_blocking(Duration::from_secs(1))
                .expect("Failed to receive count");
            info!("  Link {}: received count={}", i, count);
            for _ in 0..count {
                let edge: UgnEdgeWire = subordinate_link
                    .recv_blocking(Duration::from_secs(1))
                    .expect("Failed to receive edge");
                info!(
                    "  Link {}: received edge: {:02X?}:{} -> {:02X?}:{}, ugn={}",
                    i, edge.src_node, edge.src_port, edge.dst_node, edge.dst_port, edge.ugn
                );
                report.insert_edge(edge.into());
            }
        }
        info!(
            "  Manager received all reports and built complete UGN graph with {} edges!",
            report.count
        );
        info!("  Final UGN Report: {:?}", report);
    } else {
        // Subordinate: Wait for commands and respond with reports
        info!("Step 4a: Subordinate has to wait for command from manager...");
        let mut manager_idx = None;

        // Wait for commands on all links
        while manager_idx.is_none() {
            for i in 0..LINK_COUNT {
                let potential_manager_link = match i {
                    0 => &mut link0,
                    1 => &mut link1,
                    2 => &mut link2,
                    3 => &mut link3,
                    4 => &mut link4,
                    5 => &mut link5,
                    6 => &mut link6,
                    _ => unreachable!(),
                };

                potential_manager_link.poll();
                if let Ok(wire) = potential_manager_link.try_recv::<CommandWire>() {
                    if let Ok(Command::RequestUgnReport) = Command::try_from(wire) {
                        info!("  Link {}: received RequestUgnReport command", i);
                        manager_idx = Some(i);
                        break;
                    }
                }
            }
        }
        info!(
            "  Received command on link {}, preparing to send report...",
            manager_idx.unwrap()
        );

        // Step 4b: Send counts to all links
        info!("Step 4b: Sending UGN edge counts to all links...");
        let manager_link = match manager_idx.unwrap() {
            0 => &mut link0,
            1 => &mut link1,
            2 => &mut link2,
            3 => &mut link3,
            4 => &mut link4,
            5 => &mut link5,
            6 => &mut link6,
            _ => panic!("Invalid manager link index"),
        };
        manager_link
            .send_blocking(&report.count, Duration::from_secs(1))
            .expect("Failed to send count");

        info!(
            "  Sent count to manager link {}, now sending edges...",
            manager_idx.unwrap()
        );

        // Step 4c: Send edges to manager
        let mut edges_sent = 0;
        let mut edge_idx = 0;
        while edges_sent < report.count as usize && edge_idx < report.edges.len() {
            manager_link.poll();
            if let Some(edge) = report.edges[edge_idx] {
                let wire: UgnEdgeWire = edge.into();
                if manager_link.try_send(&wire).is_ok() {
                    info!(
                        "  Link {}: sent edge {}: {:02X?}:{} -> {:02X?}:{}, ugn={}",
                        manager_idx.unwrap(),
                        edges_sent,
                        edge.src_node,
                        edge.src_port,
                        edge.dst_node,
                        edge.dst_port,
                        edge.ugn
                    );
                    edges_sent += 1;
                    edge_idx += 1;
                }
            } else {
                edge_idx += 1;
            }
        }
    }

    uwriteln!(uart, "Demo complete.").unwrap();
    // Keep polling links to ensure all transmissions complete
    loop {
        link0.poll();
        link1.poll();
        link2.poll();
        link3.poll();
        link4.poll();
        link5.poll();
        link6.poll();
        continue;
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
        info!("mcause: {:?}\n", mcause::read());
        info!("mepc: {:?}\n", mepc::read());
        info!("mtval: {:?}\n", mtval::read());
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
