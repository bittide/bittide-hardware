// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]
#![feature(sync_unsafe_cell)]

use bittide_hal::manual_additions::ringbuffer::AlignedReceiveBuffer;
use bittide_hal::ringbuffer_test::DeviceInstances;
use bittide_sys::smoltcp::link_interface::{LinkInterface, RecvError};
use core::fmt::Write;
use log::{info, LevelFilter};
use riscv::register::{mcause, mepc, mtval};
#[cfg(not(test))]
use riscv_rt::entry;
use ufmt::uwriteln;
use zerocopy::byteorder::{I64, LE, U16, U32};
use zerocopy::{AsBytes, FromBytes, FromZeroes, Unaligned};

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

// Test data structures for structured data exchange
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TestMessage {
    pub id: u32,
    pub value: i64,
    pub flags: u16,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Default, FromZeroes, FromBytes, AsBytes, Unaligned)]
struct TestMessageWire {
    id: U32<LE>,
    value: I64<LE>,
    flags: U16<LE>,
}

impl From<TestMessage> for TestMessageWire {
    fn from(msg: TestMessage) -> Self {
        Self {
            id: U32::new(msg.id),
            value: I64::new(msg.value),
            flags: U16::new(msg.flags),
        }
    }
}

impl From<TestMessageWire> for TestMessage {
    fn from(msg: TestMessageWire) -> Self {
        Self {
            id: msg.id.get(),
            value: msg.value.get(),
            flags: msg.flags.get(),
        }
    }
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
        log::set_max_level_racy(LevelFilter::Trace);
    }

    info!("=== LinkInterface Example ===");

    // Align ringbuffers
    let tx_buffer0 = INSTANCES.transmit_ringbuffer_0;
    let rx_buffer0 = INSTANCES.receive_ringbuffer_0;
    let tx_buffer1 = INSTANCES.transmit_ringbuffer_1;
    let rx_buffer1 = INSTANCES.receive_ringbuffer_1;

    let mut rx_aligned0 = AlignedReceiveBuffer::new(rx_buffer0);
    let mut rx_aligned1 = AlignedReceiveBuffer::new(rx_buffer1);
    while !(rx_aligned0.is_aligned() & rx_aligned1.is_aligned()) {
        rx_aligned0.align_step(&tx_buffer0);
        rx_aligned1.align_step(&tx_buffer1);
    }

    // Create two links with TCP simultaneous open
    let mut link0 = LinkInterface::<_, _, 512>::new(rx_aligned0, tx_buffer1, timer);
    link0.connect();

    let mut link1 = LinkInterface::<_, _, 512>::new(rx_aligned1, tx_buffer0, unsafe {
        bittide_hal::shared_devices::Timer::new(INSTANCES.timer.0)
    });
    link1.connect();

    info!("Links created, waiting for connection...");
    info!("Link0 initial state: {:?}", link0.state());
    info!("Link1 initial state: {:?}", link1.state());

    // Poll until both links are established
    for _ in 0..10000 {
        link0.poll();
        link1.poll();

        if link0.is_established() && link1.is_established() {
            info!("Both links established!");
            info!("Link0 state: {:?}", link0.state());
            info!("Link1 state: {:?}", link1.state());
            break;
        }
    }

    if !link0.is_established() || !link1.is_established() {
        info!("Connection timeout!");
        uwriteln!(uart, "=== Test Failed ===").unwrap();
        uwriteln!(uart, "=== Test Complete ===").unwrap();
        loop {
            continue;
        }
    }

    // Exchange data - demonstrating the new try_recv_bytes pattern
    info!("Exchanging data with try_recv_bytes pattern...");

    let msg_0_to_1 = b"Hello from Node 0";
    let msg_1_to_0 = b"Hello from Node 1";

    link0.send(msg_0_to_1);
    link1.send(msg_1_to_0);

    // Pattern 1: Using try_recv_bytes with manual polling
    let mut node0_received = false;
    let mut node1_received = false;

    for _ in 0..5000 {
        link0.poll();
        link1.poll();

        if !node0_received {
            let mut buffer = [0u8; 64];
            match link0.try_recv_bytes(&mut buffer) {
                Ok(n) if &buffer[..n] == msg_1_to_0 => {
                    info!("Node 0 received correct data (try_recv_bytes)");
                    node0_received = true;
                }
                Ok(n) => {
                    info!("Node 0 received {} bytes but data mismatch", n);
                }
                Err(RecvError::WouldBlock) => { /* No data yet, continue polling */ }
                Err(e) => {
                    info!("Node 0 recv error: {:?}", e);
                    break;
                }
            }
        }

        if !node1_received {
            let mut buffer = [0u8; 64];
            match link1.try_recv_bytes(&mut buffer) {
                Ok(n) if &buffer[..n] == msg_0_to_1 => {
                    info!("Node 1 received correct data (try_recv_bytes)");
                    node1_received = true;
                }
                Ok(n) => {
                    info!("Node 1 received {} bytes but data mismatch", n);
                }
                Err(RecvError::WouldBlock) => { /* No data yet, continue polling */ }
                Err(e) => {
                    info!("Node 1 recv error: {:?}", e);
                    break;
                }
            }
        }

        if node0_received && node1_received {
            break;
        }
    }

    // Exchange structured data using the new typed API
    info!("Exchanging structured data with typed try_recv...");

    let struct_0_to_1 = TestMessage {
        id: 42,
        value: -123456789,
        flags: 0xABCD,
    };
    let struct_1_to_0 = TestMessage {
        id: 99,
        value: 987654321,
        flags: 0x1234,
    };

    let wire_0: TestMessageWire = struct_0_to_1.into();
    let wire_1: TestMessageWire = struct_1_to_0.into();

    link0.send(wire_0.as_bytes());
    link1.send(wire_1.as_bytes());

    let mut struct0_received = false;
    let mut struct1_received = false;

    for _ in 0..5000 {
        link0.poll();
        link1.poll();

        if !struct0_received {
            if let Ok(wire_msg) = link0.try_recv::<TestMessageWire>() {
                let received: TestMessage = wire_msg.into();
                if received == struct_1_to_0 {
                    info!(
                        "Node 0 received correct struct (typed): id={}, value={}, flags={:#x}",
                        received.id, received.value, received.flags
                    );
                    struct0_received = true;
                } else {
                    info!("Node 0 received struct but data mismatch");
                }
            }
        }

        if !struct1_received {
            if let Ok(wire_msg) = link1.try_recv::<TestMessageWire>() {
                let received: TestMessage = wire_msg.into();
                if received == struct_0_to_1 {
                    info!(
                        "Node 1 received correct struct (typed): id={}, value={}, flags={:#x}",
                        received.id, received.value, received.flags
                    );
                    struct1_received = true;
                } else {
                    info!("Node 1 received struct but data mismatch");
                }
            }
        }

        if struct0_received && struct1_received {
            break;
        }
    }

    // Report results
    if node0_received && node1_received && struct0_received && struct1_received {
        info!("=== All tests passed! ===");
        uwriteln!(uart, "=== Simultaneous Open Success ===").unwrap();
    } else {
        info!("=== Test failed ===");
        uwriteln!(uart, "=== Test Failed ===").unwrap();
    }

    // Demonstrate graceful shutdown
    info!("Closing connections...");
    link0.close();
    link1.close();

    for _ in 0..5000 {
        link0.poll();
        link1.poll();

        if link0.is_closing_or_closed() && link1.is_closing_or_closed() {
            info!("Both links initiated close handshake");
            info!("Link0 final state: {:?}", link0.state());
            info!("Link1 final state: {:?}", link1.state());

            if link0.is_closed() && link1.is_closed() {
                info!("Both links reached Closed state");
            }
            break;
        }
    }

    if !link0.is_closing_or_closed() || !link1.is_closing_or_closed() {
        info!("Warning: Links did not initiate close");
    }

    uwriteln!(uart, "=== Test Complete ===").unwrap();
    loop {
        continue;
    }
}

#[panic_handler]
fn panic(info: &core::panic::PanicInfo) -> ! {
    let mut uart = INSTANCES.uart;
    writeln!(uart, "PANIC: {}", info).ok();
    loop {}
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
