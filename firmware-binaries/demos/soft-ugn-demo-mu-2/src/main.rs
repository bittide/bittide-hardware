#![no_std]
#![cfg_attr(not(test), no_main)]
#![feature(sync_unsafe_cell)]

// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::hals::soft_ugn_demo_mu::devices::{ReceiveRingbuffer, TransmitRingbuffer};
use bittide_hal::hals::soft_ugn_demo_mu::DeviceInstances;
use bittide_hal::manual_additions::timer::Instant;
use bittide_sys::link_startup::LinkStartup;
use bittide_sys::net_state::{Manager, SmoltcpLink, Subordinate, UgnEdge, UgnReport};
use bittide_sys::smoltcp::soft_ugn_ringbuffer::{AlignedReceiveBuffer, RingbufferDevice};
use bittide_sys::stability_detector::Stability;
use core::panic::PanicInfo;
use log::{info, trace, warn, LevelFilter};
use smoltcp::iface::{Config, Interface, SocketHandle, SocketSet, SocketStorage};
use smoltcp::socket::tcp;
use smoltcp::wire::HardwareAddress;
use ufmt::uwriteln;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };
const LINK_COUNT: usize = 7;
const TCP_BUF_SIZE: usize = 256;
const MANAGER_DNA: [u8; 12] = [133, 129, 48, 4, 64, 192, 105, 1, 1, 0, 2, 64];

static mut TCP_RX_BUFS: [[u8; TCP_BUF_SIZE]; LINK_COUNT] = [[0; TCP_BUF_SIZE]; LINK_COUNT];
static mut TCP_TX_BUFS: [[u8; TCP_BUF_SIZE]; LINK_COUNT] = [[0; TCP_BUF_SIZE]; LINK_COUNT];

#[cfg(not(test))]
use riscv_rt::entry;

fn to_smoltcp_instant(instant: Instant) -> smoltcp::time::Instant {
    smoltcp::time::Instant::from_micros(instant.micros() as i64)
}

fn socket_set<'a>(storage: &'a mut [SocketStorage<'static>]) -> SocketSet<'a> {
    // SAFETY: Socket buffers are backed by static memory, and SocketSet does not
    // outlive the borrow of storage in this scope.
    let storage: &'a mut [SocketStorage<'a>] = unsafe { core::mem::transmute(storage) };
    SocketSet::new(storage)
}

fn make_device(rx: ReceiveRingbuffer, tx: TransmitRingbuffer) -> RingbufferDevice {
    let mut rx_aligned = AlignedReceiveBuffer::new(rx);
    rx_aligned.align(&tx);
    RingbufferDevice::new(rx_aligned, tx)
}

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
        log::set_max_level_racy(LevelFilter::Trace);
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
    loop {
        let stability = Stability {
            stable: cc.links_stable()[0],
            settled: 0,
        };
        if stability.all_stable() {
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

    let mut devices: [RingbufferDevice; LINK_COUNT] = [
        make_device(
            INSTANCES.receive_ringbuffer_0,
            INSTANCES.transmit_ringbuffer_0,
        ),
        make_device(
            INSTANCES.receive_ringbuffer_1,
            INSTANCES.transmit_ringbuffer_1,
        ),
        make_device(
            INSTANCES.receive_ringbuffer_2,
            INSTANCES.transmit_ringbuffer_2,
        ),
        make_device(
            INSTANCES.receive_ringbuffer_3,
            INSTANCES.transmit_ringbuffer_3,
        ),
        make_device(
            INSTANCES.receive_ringbuffer_4,
            INSTANCES.transmit_ringbuffer_4,
        ),
        make_device(
            INSTANCES.receive_ringbuffer_5,
            INSTANCES.transmit_ringbuffer_5,
        ),
        make_device(
            INSTANCES.receive_ringbuffer_6,
            INSTANCES.transmit_ringbuffer_6,
        ),
    ];
    let mut ifaces: [Interface; LINK_COUNT] = core::array::from_fn(|idx| {
        let now = to_smoltcp_instant(INSTANCES.timer.now());
        let mut iface = Interface::new(Config::new(HardwareAddress::Ip), &mut devices[idx], now);
        iface.update_ip_addrs(|addrs| {
            addrs.clear();
        });
        iface
    });
    let mut sockets_storage: [[SocketStorage<'static>; 1]; LINK_COUNT] =
        core::array::from_fn(|_| Default::default());
    let socket_handles: [SocketHandle; LINK_COUNT] = core::array::from_fn(|idx| {
        let rx_buf = unsafe { &mut TCP_RX_BUFS[idx][..] };
        let tx_buf = unsafe { &mut TCP_TX_BUFS[idx][..] };
        let socket = tcp::Socket::new(
            tcp::SocketBuffer::new(rx_buf),
            tcp::SocketBuffer::new(tx_buf),
        );
        let mut sockets = socket_set(&mut sockets_storage[idx][..]);
        sockets.add(socket)
    });

    let dna = INSTANCES.dna.dna();
    info!("My dna: {:?}", dna);
    let is_manager = dna == MANAGER_DNA;
    info!(
        "Role: {}",
        if is_manager { "manager" } else { "subordinate" }
    );

    if is_manager {
        info!("Starting manager state machines...");
        let mut managers: [Manager; LINK_COUNT] = core::array::from_fn(|_| Manager::new());
        let mut done = [false; LINK_COUNT];

        loop {
            let now = to_smoltcp_instant(INSTANCES.timer.now());
            for link in 0..LINK_COUNT {
                let mut sockets = socket_set(&mut sockets_storage[link][..]);
                let _ = ifaces[link].poll(now, &mut devices[link], &mut sockets);
                let mut smoltcp_link = SmoltcpLink::new(
                    &mut ifaces[link],
                    &mut sockets,
                    socket_handles[link],
                    link,
                    true,
                    false,
                );
                managers[link].step(&mut smoltcp_link);
                if managers[link].is_done() {
                    done[link] = true;
                }
            }

            if done.iter().all(|v| *v) {
                break;
            }
        }

        info!("UGN reports from subordinates:");
        for (idx, manager) in managers.iter().enumerate() {
            if let Some(report) = manager.report() {
                info!("Link {}: {} edges", idx, report.count);
                for (edge_idx, edge) in report.edges.iter().enumerate() {
                    if edge_idx >= report.count as usize {
                        break;
                    }
                    if let Some(edge) = edge {
                        info!(
                            "  Edge {}: {}:{} -> {}:{}, ugn={}",
                            edge_idx,
                            edge.src_node,
                            edge.src_port,
                            edge.dst_node,
                            edge.dst_port,
                            edge.ugn
                        );
                    } else {
                        warn!("  Edge {}: missing", edge_idx);
                    }
                }
            } else {
                warn!("Link {}: no report", idx);
            }
        }
    } else {
        info!("Starting subordinate state machines...");
        let mut subordinates: [Subordinate; LINK_COUNT] =
            core::array::from_fn(|_| Subordinate::new());
        for link in 0..LINK_COUNT {
            subordinates[link].set_report(build_report_for_link(link, &capture_ugns[link], &dna));
        }

        loop {
            let now = to_smoltcp_instant(INSTANCES.timer.now());
            for link in 0..LINK_COUNT {
                let mut sockets = socket_set(&mut sockets_storage[link][..]);
                let _ = ifaces[link].poll(now, &mut devices[link], &mut sockets);
                let mut smoltcp_link = SmoltcpLink::new(
                    &mut ifaces[link],
                    &mut sockets,
                    socket_handles[link],
                    link,
                    true,
                    false,
                );
                subordinates[link].step(&mut smoltcp_link);
            }
        }
    }

    uwriteln!(uart, "Demo complete.").unwrap();
    loop {
        continue;
    }
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    loop {
        continue;
    }
}

fn build_report_for_link(
    link: usize,
    capture_ugn: &bittide_hal::shared_devices::CaptureUgn,
    dna: &[u8; 12],
) -> UgnReport {
    let mut report = UgnReport::new();
    report.count = 1;
    report.edges[0] = Some(UgnEdge {
        src_node: dna[0] as u32,
        src_port: link as u32,
        dst_node: 0,
        dst_port: link as u32,
        ugn: capture_ugn.local_counter() as i64,
    });
    trace!("Prepared report for link {}", link);
    report
}
