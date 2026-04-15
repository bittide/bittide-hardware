// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use bittide_hal::shared_devices::TransmitRingbuffer;
use bittide_hal::{
    manual_additions::{
        ringbuffer::{
            AlignedReceiveBuffer, ReceiveRingbufferInterface, TransmitRingbufferInterface,
        },
        timer::Duration,
    },
    ringbuffer_test::DeviceInstances,
};
use core::fmt::Write;
#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };
const LEN: usize = TransmitRingbuffer::DATA_LEN;
const PROPAGATION_CYCLES: u32 = 2000;

fn wait(timer: &bittide_hal::shared_devices::Timer) {
    timer.wait(Duration::from_cycles(PROPAGATION_CYCLES, timer.frequency()));
}

fn read_rx(rx: &bittide_hal::shared_devices::ReceiveRingbuffer) -> [[u8; 8]; LEN] {
    let mut buf = [[0u8; 8]; LEN];
    rx.read_slice(&mut buf, 0);
    buf
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    let timer = INSTANCES.timer;
    let tx = INSTANCES.transmit_ringbuffer;
    let rx = INSTANCES.receive_ringbuffer;

    let mut all_passed = true;

    // Both enables default to false; turn them on.
    tx.set_enable(true);
    rx.set_enable(true);

    // Test 1: Alignment — after alignment, TX[i] should arrive at RX[i].
    let mut rx_aligned = AlignedReceiveBuffer::new(rx);
    rx_aligned.align(&tx);

    tx.clear();
    let pattern_a: [[u8; 8]; LEN] = core::array::from_fn(|i| (0x1000 + i as u64).to_le_bytes());
    tx.write_slice(&pattern_a, 0);
    wait(&timer);
    let rx_data = read_rx(&rx_aligned.buffer);

    if pattern_a != rx_data {
        all_passed = false;
        writeln!(uart, "FAIL: aligned loopback").unwrap();
        for i in 0..LEN {
            if pattern_a[i] != rx_data[i] {
                writeln!(
                    uart,
                    "  mismatch at {}: expected {:02x?}, got {:02x?}",
                    i, pattern_a[i], rx_data[i]
                )
                .unwrap();
            }
        }
    }

    // Test 2: receive_enable=false freezes the aligned RX buffer.
    rx_aligned.buffer.set_enable(false);
    let pattern_b: [[u8; 8]; LEN] = core::array::from_fn(|i| (0x2000 + i as u64).to_le_bytes());
    tx.write_slice(&pattern_b, 0);
    wait(&timer);
    let rx_after_disable = read_rx(&rx_aligned.buffer);

    if rx_after_disable != rx_data {
        all_passed = false;
        writeln!(uart, "FAIL: receive_enable=false did not freeze RX").unwrap();
    }

    // Test 3: receive_enable=true resumes updates.
    rx_aligned.buffer.set_enable(true);
    wait(&timer);
    let rx_after_enable = read_rx(&rx_aligned.buffer);

    if rx_after_enable != pattern_b {
        all_passed = false;
        writeln!(uart, "FAIL: receive_enable=true did not resume RX").unwrap();
    }

    if all_passed {
        writeln!(uart, "*** ALL TESTS PASSED ***").unwrap();
    } else {
        writeln!(uart, "*** TEST FAILED ***").unwrap();
    }

    writeln!(uart, "Test done").unwrap();
    loop {
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
