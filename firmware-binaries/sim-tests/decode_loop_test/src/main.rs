// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

//! Exercises the decode demo's software engines (variants C and A′ from
//! `bittide_sys::decode_demo`) against the `DecodeLoopTest` DUT: one CPU,
//! two ring-buffer pairs looped back through delay lines — a one-node ring.
//! The node's own contribution returns as the "accumulated" vector, so the
//! grand-total checksum equals the node's own window checksum.

use bittide_hal::decode_loop_test::DeviceInstances;
use bittide_hal::manual_additions::ring_buffer::AlignedReceiveBuffer;
use bittide_sys::decode_demo::{
    measure_frame_service_cycles, now_cycles, run_variant_a_prime, run_variant_c, DecodeBuffers,
    DecodeConfig, DecodeResults, MODE_A_PRIME, MODE_C,
};
use core::fmt::Write;
use core::ptr::addr_of_mut;
#[cfg(not(test))]
use riscv_rt::entry;
use ufmt::uwriteln;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

const VECTOR_WORDS: u32 = 4;
const LAYERS: u32 = 2;
const TOKENS: u32 = 2;
// The A' run is trimmed to one token: simulated cycles are expensive, and
// its windows must be sized to the measured per-frame service time (~9k
// cycles on this CPU).
const APRIME_TOKENS: u32 = 1;
const APRIME_LAP: u32 = 12_000;

static mut RESULTS: DecodeResults = DecodeResults::new();

/// The one-node ring's grand total is the node's own contribution.
fn expected_window(pattern: u64) -> u64 {
    let mut sum: u64 = 0;
    for i in 0..VECTOR_WORDS as u64 {
        sum = sum.wrapping_add(pattern.wrapping_add(i));
    }
    sum
}

fn base_config() -> DecodeConfig {
    let mut cfg = DecodeConfig::new();
    cfg.is_injector = 1;
    cfg.layers_per_token = LAYERS;
    cfg.token_count = TOKENS;
    cfg.vector_words = VECTOR_WORDS;
    cfg.compute_cycles = 0;
    cfg.poll_timeout = 200_000;
    cfg.local_pattern = 0x1000;
    cfg.expected_window_a = expected_window(0x1000);
    cfg.expected_window_b = expected_window(0x1000);
    cfg
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    let timer = INSTANCES.timer;

    let bufs = DecodeBuffers {
        up_rx: &INSTANCES.receive_ring_buffer_0,
        down_tx: &INSTANCES.transmit_ring_buffer_0,
        down_ack_rx: &INSTANCES.receive_ring_buffer_1,
        up_ack_tx: &INSTANCES.transmit_ring_buffer_1,
    };
    let results = unsafe { &mut *addr_of_mut!(RESULTS) };
    let mut all_passed = true;

    // Align both loops so TX slot i lands in RX slot i.
    let mut rx0_aligned = AlignedReceiveBuffer::new(INSTANCES.receive_ring_buffer_0);
    while !rx0_aligned.align_step(&INSTANCES.transmit_ring_buffer_0) {}
    let mut rx1_aligned = AlignedReceiveBuffer::new(INSTANCES.receive_ring_buffer_1);
    while !rx1_aligned.align_step(&INSTANCES.transmit_ring_buffer_1) {}
    uwriteln!(uart, "Aligned").unwrap();

    // Measurement 1: per-frame service time. Purely informative: the
    // transmit ring retransmits its buffer contents every wrap, so a frame
    // persists at the receiver until its region is rewritten — reuse is
    // guarded by the ack protocol, not by the wrap window.
    let cfg = base_config();
    let service = measure_frame_service_cycles(&cfg, &timer, &bufs);
    uwriteln!(uart, "Frame service cycles: {}", service).unwrap();

    // Test 1: variant C, injector on the one-node ring.
    let mut cfg = base_config();
    cfg.go = MODE_C;
    cfg.first_cycle = now_cycles(&timer) + 1024;
    run_variant_c(&cfg, &timer, &mut uart, &bufs, results);
    uwriteln!(
        uart,
        "C: tokens {} fails {} lost {} min {} max {}",
        results.tokens_done,
        results.checksum_fails,
        results.lost_frames,
        results.min_latency,
        results.max_latency
    )
    .unwrap();
    if results.tokens_done != TOKENS
        || results.checksum_fails != 0
        || results.lost_frames != 0
        || results.mode != MODE_C
    {
        all_passed = false;
        uwriteln!(uart, "FAIL: variant C").unwrap();
    }

    // Test 2: variant A' (timer-fired). Windows must exceed the per-frame
    // service time; the start gate must absorb the engine's buffer clears.
    let mut cfg = base_config();
    cfg.go = MODE_A_PRIME;
    cfg.token_count = APRIME_TOKENS;
    cfg.first_cycle = now_cycles(&timer) + 30_000;
    cfg.lap_offset = APRIME_LAP;
    cfg.layer_period = 3 * APRIME_LAP + APRIME_LAP;
    cfg.token_period = LAYERS * (3 * APRIME_LAP + APRIME_LAP) + APRIME_LAP;
    run_variant_a_prime(&cfg, &timer, &mut uart, &bufs, results);
    uwriteln!(
        uart,
        "A': tokens {} fails {} missed {} min {} max {}",
        results.tokens_done,
        results.checksum_fails,
        results.deadlines_missed,
        results.min_latency,
        results.max_latency
    )
    .unwrap();
    if results.tokens_done != APRIME_TOKENS
        || results.checksum_fails != 0
        || results.deadlines_missed != 0
        || results.mode != MODE_A_PRIME
    {
        all_passed = false;
        uwriteln!(uart, "FAIL: variant A'").unwrap();
    }

    // Test 3: the lost-frame path — a relay whose upstream never sends. Every
    // layer's first poll times out; the token is abandoned and counted.
    let mut cfg = base_config();
    cfg.go = MODE_C;
    cfg.is_injector = 0;
    cfg.token_count = 1;
    cfg.poll_timeout = 2_000;
    cfg.first_cycle = now_cycles(&timer) + 1024;
    run_variant_c(&cfg, &timer, &mut uart, &bufs, results);
    uwriteln!(
        uart,
        "Relay-timeout: tokens {} lost {}",
        results.tokens_done,
        results.lost_frames
    )
    .unwrap();
    if results.tokens_done != 0 || results.lost_frames != 1 {
        all_passed = false;
        uwriteln!(uart, "FAIL: lost-frame path").unwrap();
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

#[panic_handler]
fn panic_handler(info: &core::panic::PanicInfo) -> ! {
    let mut uart = INSTANCES.uart;
    writeln!(uart, "Panicked! #{info}").unwrap();
    loop {
        continue;
    }
}
