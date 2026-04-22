// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use bittide_hal::ringbuffer_test::devices::TransmitRingbuffer;
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
use ufmt::{uwrite, uwriteln};

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };
const LEN: usize = TransmitRingbuffer::DATA_LEN;
const PROPAGATION_CYCLES: u32 = 2000;

fn make_pattern(base: u8) -> [[u8; 8]; LEN] {
    core::array::from_fn(|frame| {
        core::array::from_fn(|byte| ((frame as u8 + base) << 4) | byte as u8)
    })
}

fn wait(timer: &bittide_hal::shared_devices::Timer) {
    timer.wait(Duration::from_cycles(PROPAGATION_CYCLES, timer.frequency()));
}

fn read_rx(rx: &bittide_hal::ringbuffer_test::devices::ReceiveRingbuffer) -> [[u8; 8]; LEN] {
    let mut buf = [[0u8; 8]; LEN];
    rx.read_slice(&mut buf, 0);
    buf
}

/// Check that rx contains the same data as tx (modulo a frame offset from the loopback).
fn verify_loopback(tx: &[[u8; 8]; LEN], rx: &[[u8; 8]; LEN]) -> bool {
    let offset = (0..LEN).find(|&i| rx[i] == tx[0]);
    offset.is_some_and(|off| (0..LEN).all(|frame| rx[(off + frame) % LEN] == tx[frame]))
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    let timer = INSTANCES.timer;
    let tx = INSTANCES.transmit_ringbuffer_0;
    let rx = INSTANCES.receive_ringbuffer_0;

    let mut all_passed = true;

    // Both enables default to false; turn them on for the loopback tests.
    tx.set_enable(true);
    rx.set_enable(true);

    // Test 1: Basic loopback — data written to TX arrives at RX (possibly rotated).
    let pattern_a = make_pattern(0);
    tx.write_slice(&pattern_a, 0);
    wait(&timer);
    let rx_data = read_rx(&rx);
    if !verify_loopback(&pattern_a, &rx_data) {
        all_passed = false;
        uwriteln!(uart, "FAIL: loopback").unwrap();
        dump_buffers(&mut uart, &pattern_a, &rx_data);
    }

    // Test 2: receive_enable=false freezes the RX buffer.
    rx.set_enable(false);
    let pattern_b = make_pattern(0x80);
    tx.write_slice(&pattern_b, 0);
    wait(&timer);
    let rx_after_rx_disable = read_rx(&rx);
    if rx_after_rx_disable != rx_data {
        all_passed = false;
        uwriteln!(uart, "FAIL: receive_enable=false did not freeze RX").unwrap();
        dump_buffers(&mut uart, &pattern_b, &rx_after_rx_disable);
    }

    // Test 3: receive_enable=true resumes RX updates.
    rx.set_enable(true);
    wait(&timer);
    let rx_after_rx_enable = read_rx(&rx);
    if !verify_loopback(&pattern_b, &rx_after_rx_enable) {
        all_passed = false;
        uwriteln!(uart, "FAIL: receive_enable=true did not resume RX").unwrap();
        dump_buffers(&mut uart, &pattern_b, &rx_after_rx_enable);
    }

    // Test 4: write_enable=false sends zeros over the network.
    tx.set_enable(false);
    let pattern_c = make_pattern(0x40);
    tx.write_slice(&pattern_c, 0);
    wait(&timer);
    let rx_after_tx_disable = read_rx(&rx);
    let all_zero = rx_after_tx_disable.iter().all(|w| *w == [0u8; 8]);
    if !all_zero {
        all_passed = false;
        uwriteln!(uart, "FAIL: write_enable=false did not send zeros").unwrap();
        dump_buffers(&mut uart, &[[0u8; 8]; LEN], &rx_after_tx_disable);
    }

    // Test 5: write_enable=true resumes transmission.
    tx.set_enable(true);
    wait(&timer);
    let rx_after_tx_enable = read_rx(&rx);
    if !verify_loopback(&pattern_c, &rx_after_tx_enable) {
        all_passed = false;
        uwriteln!(uart, "FAIL: write_enable=true did not resume TX").unwrap();
        dump_buffers(&mut uart, &pattern_c, &rx_after_tx_enable);
    }

    // Test 6: AlignedReceiveBuffer::align — after alignment, TX[i] arrives at RX[i] exactly.
    let mut rx_aligned = AlignedReceiveBuffer::new(rx);
    while !rx_aligned.align_step(&tx) {}
    tx.clear();
    rx_aligned.buffer.set_enable(true);
    tx.set_enable(true);
    let pattern_d: [[u8; 8]; LEN] = core::array::from_fn(|i| (0x1000 + i as u64).to_le_bytes());
    tx.write_slice(&pattern_d, 0);
    wait(&timer);
    let rx_aligned_data = read_rx(&rx_aligned.buffer);
    if pattern_d != rx_aligned_data {
        all_passed = false;
        uwriteln!(uart, "FAIL: aligned loopback").unwrap();
        dump_buffers(&mut uart, &pattern_d, &rx_aligned_data);
    }

    if all_passed {
        uwriteln!(uart, "*** TEST PASSED ***").unwrap();
    } else {
        uwriteln!(uart, "*** TEST FAILED ***").unwrap();
    }
    uwriteln!(uart, "=== Test Complete ===").unwrap();

    loop {
        continue;
    }
}

fn dump_buffers(
    uart: &mut bittide_hal::shared_devices::Uart,
    expected: &[[u8; 8]],
    actual: &[[u8; 8]],
) {
    uwriteln!(uart, "  expected:").unwrap();
    for (i, word) in expected.iter().enumerate() {
        uwrite!(uart, "    [{}]: ", i).unwrap();
        for &b in word {
            uwrite!(uart, "{:02x} ", b).unwrap();
        }
        uwriteln!(uart, "").unwrap();
    }
    uwriteln!(uart, "  actual:").unwrap();
    for (i, word) in actual.iter().enumerate() {
        uwrite!(uart, "    [{}]: ", i).unwrap();
        for &b in word {
            uwrite!(uart, "{:02x} ", b).unwrap();
        }
        uwriteln!(uart, "").unwrap();
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
