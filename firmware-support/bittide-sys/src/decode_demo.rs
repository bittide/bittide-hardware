// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Software transport engines for the decode demo.
//!
//! Two engines share one code path over the ring buffers (the management
//! unit's only access to link data):
//!
//! * **Variant C** ([`run_variant_c`]): a conventional rendezvous protocol —
//!   the sender writes payload + trailer and polls for the receiver's ack;
//!   the receiver polls for the trailer. This is the software-transport
//!   (NIC/NCCL-class) baseline. No artificial delays: all cost is real
//!   polling granularity, copy cost and ack round trips.
//! * **Variant A′** ([`run_variant_a_prime`]): the same frame operations
//!   fired by timer deadlines derived from the UGN-based schedule — no polls,
//!   no acks. A′ vs C isolates the protocol cost with the engine held
//!   constant; A′ vs the hardware PE isolates the engine cost with the
//!   discipline held constant.
//!
//! The workload is the same systolic two-lap ring all-reduce as the hardware
//! processing element: the injector streams `local_pattern + i`, relays add
//! their contribution flow-through, lap 2 broadcasts the grand total, and the
//! injector sinks it, latching the token latency.
//!
//! Frame layout in the 512-slot ring buffers: four regions of 128 slots,
//! `region = seq % 4`. Payload words at the region base, the trailer
//! (`seq << 32 | checksum32`) one word past the payload area — written last,
//! so a matching trailer implies the payload is complete (the hardware sweeps
//! slots in cycle order). Acks travel on the reverse direction of the same
//! neighbor link: `ACK_MAGIC << 32 | seq` at the region's ack slot.
//!
//! A frame that is not observed in time is *lost*, not late: the poll either
//! times out or observes a later sequence number in the same region
//! (overwritten). Both sides then abandon the token and resynchronize at the
//! next token's known sequence numbers.

use bittide_hal::manual_additions::ring_buffer::{
    ReceiveRingBufferInterface, TransmitRingBufferInterface,
};
use bittide_hal::manual_additions::timer::WaitResult;
use bittide_hal::shared_devices::timer::Timer;
use bittide_hal::shared_devices::Uart;
use ufmt::uwriteln;

pub const DECODE_CONFIG_MAGIC: u32 = 0xDEC0_C0F6;
pub const DECODE_RESULTS_MAGIC: u32 = 0xDEC0_DE01;
pub const ACK_MAGIC: u32 = 0xACC0_0001;

/// `DecodeResults::mode` / `DecodeConfig::go` values. 0/1 are reserved for
/// the hardware variants (A/B) so dumps are self-describing across variants.
pub const MODE_IDLE: u32 = 0;
pub const MODE_C: u32 = 2;
pub const MODE_A_PRIME: u32 = 3;

pub const REGION_COUNT: usize = 4;
pub const REGION_STRIDE: usize = 128;
pub const TRAILER_OFFSET: usize = 64;
pub const ACK_OFFSET: usize = 96;
pub const MAX_VECTOR_WORDS: usize = 64;

pub const HIST_BINS: usize = 256;
pub const RAW_SAMPLES: usize = 512;

/// Host-written run configuration. The host driver locates this struct
/// through an address the firmware prints at boot, writes it over GDB while
/// the management unit is halted, and sets `go` last.
#[repr(C)]
pub struct DecodeConfig {
    pub magic: u32,
    /// [`MODE_C`] or [`MODE_A_PRIME`]; cleared by firmware when the run is
    /// done.
    pub go: u32,
    pub is_injector: u32,
    /// Ring upstream link index (which ring-buffer pair receives forward
    /// frames and sends acks).
    pub read_link: u32,
    /// Ring downstream link index (which ring-buffer pair sends forward
    /// frames and receives acks).
    pub write_link: u32,
    pub layers_per_token: u32,
    pub token_count: u32,
    /// Payload words per frame; at most [`MAX_VECTOR_WORDS`].
    pub vector_words: u32,
    /// Emulated per-layer compute gap, in cycles (variant C; A′ bakes it
    /// into `layer_period`).
    pub compute_cycles: u32,
    /// Cycles before a poll declares a frame or ack lost (variant C).
    pub poll_timeout: u32,
    /// Variant C: start gate. Variant A′: schedule origin (this node's first
    /// window, UGN-derived).
    pub first_cycle: u64,
    /// A′ schedule: local cycles between a node's lap-1 and lap-2 windows.
    pub lap_offset: u32,
    /// A′ schedule: local cycles between consecutive layers.
    pub layer_period: u32,
    /// A′ schedule: local cycles between consecutive tokens.
    pub token_period: u32,
    pub hist_base: u32,
    pub hist_shift: u32,
    pub local_pattern: u64,
    /// Expected checksum of incoming lap-1 windows (relays).
    pub expected_window_a: u64,
    /// Expected checksum of incoming lap-2 windows (the grand total).
    pub expected_window_b: u64,
}

impl DecodeConfig {
    pub const fn new() -> Self {
        DecodeConfig {
            magic: DECODE_CONFIG_MAGIC,
            go: MODE_IDLE,
            is_injector: 0,
            read_link: 0,
            write_link: 0,
            layers_per_token: 1,
            token_count: 0,
            vector_words: 64,
            compute_cycles: 0,
            poll_timeout: 10_000_000,
            first_cycle: 0,
            lap_offset: 0,
            layer_period: 0,
            token_period: 0,
            hist_base: 0,
            hist_shift: 0,
            local_pattern: 0,
            expected_window_a: 0,
            expected_window_b: 0,
        }
    }
}

impl Default for DecodeConfig {
    fn default() -> Self {
        Self::new()
    }
}

/// Results, dumped by the host driver with GDB's `dump binary memory` from
/// the address the firmware prints at boot.
#[repr(C)]
pub struct DecodeResults {
    pub magic: u32,
    pub mode: u32,
    pub tokens_done: u32,
    pub checksum_fails: u32,
    pub lost_frames: u32,
    pub deadlines_missed: u32,
    pub hist_base: u32,
    pub hist_shift: u32,
    pub min_latency: u32,
    pub max_latency: u32,
    pub last_latency: u32,
    /// `bin = (latency - hist_base) >> hist_shift`, clamped to the edges.
    pub bins: [u32; HIST_BINS],
    pub raw_next: u32,
    /// The last [`RAW_SAMPLES`] raw token latencies, as a ring.
    pub raw: [u32; RAW_SAMPLES],
}

impl DecodeResults {
    pub const fn new() -> Self {
        DecodeResults {
            magic: DECODE_RESULTS_MAGIC,
            mode: MODE_IDLE,
            tokens_done: 0,
            checksum_fails: 0,
            lost_frames: 0,
            deadlines_missed: 0,
            hist_base: 0,
            hist_shift: 0,
            min_latency: u32::MAX,
            max_latency: 0,
            last_latency: 0,
            bins: [0; HIST_BINS],
            raw_next: 0,
            raw: [0; RAW_SAMPLES],
        }
    }

    pub fn reset(&mut self, mode: u32, hist_base: u32, hist_shift: u32) {
        *self = DecodeResults::new();
        self.mode = mode;
        self.hist_base = hist_base;
        self.hist_shift = hist_shift;
    }

    pub fn record_latency(&mut self, latency: u32) {
        self.min_latency = self.min_latency.min(latency);
        self.max_latency = self.max_latency.max(latency);
        self.last_latency = latency;
        let bin =
            (latency.saturating_sub(self.hist_base) >> self.hist_shift).min((HIST_BINS - 1) as u32);
        self.bins[bin as usize] += 1;
        self.raw[(self.raw_next as usize) % RAW_SAMPLES] = latency;
        self.raw_next = self.raw_next.wrapping_add(1);
    }
}

impl Default for DecodeResults {
    fn default() -> Self {
        Self::new()
    }
}

/// The four ring-buffer endpoints an engine drives: forward traffic arrives
/// on the read link and leaves on the write link; acks travel the reverse
/// directions of the same links.
pub struct DecodeBuffers<'a, Rx, Tx> {
    /// Incoming forward frames (read link, receive side).
    pub up_rx: &'a Rx,
    /// Outgoing acks toward the upstream neighbor (read link, transmit side).
    pub up_ack_tx: &'a Tx,
    /// Outgoing forward frames (write link, transmit side).
    pub down_tx: &'a Tx,
    /// Incoming acks from the downstream neighbor (write link, receive side).
    pub down_ack_rx: &'a Rx,
}

/// Reads the raw cycle counter (all engines measure in this one cycle base).
#[inline]
pub fn now_cycles(timer: &Timer) -> u64 {
    timer.freeze();
    timer.scratchpad().into_inner()
}

#[inline]
fn region_base(seq: u32) -> usize {
    ((seq as usize) % REGION_COUNT) * REGION_STRIDE
}

#[inline]
fn word_to_u64(w: [u8; 8]) -> u64 {
    u64::from_le_bytes(w)
}

#[inline]
fn u64_to_word(v: u64) -> [u8; 8] {
    v.to_le_bytes()
}

fn checksum32(payload: &[[u8; 8]]) -> u32 {
    let mut sum: u64 = 0;
    for w in payload {
        sum = sum.wrapping_add(word_to_u64(*w));
    }
    sum as u32
}

fn checksum64(payload: &[[u8; 8]]) -> u64 {
    let mut sum: u64 = 0;
    for w in payload {
        sum = sum.wrapping_add(word_to_u64(*w));
    }
    sum
}

/// Writes payload then trailer (trailer last: the hardware sweeps slots in
/// cycle order, so a visible trailer implies a complete payload). The
/// trailer's upper half stores `seq + 1`, so an empty (all-zero) region can
/// never alias a valid frame.
fn send_frame<Tx: TransmitRingBufferInterface>(tx: &Tx, seq: u32, payload: &[[u8; 8]]) {
    let base = region_base(seq);
    tx.write_slice(payload, base);
    let trailer = (((seq + 1) as u64) << 32) | checksum32(payload) as u64;
    tx.write_slice(&[u64_to_word(trailer)], base + TRAILER_OFFSET);
}

fn write_ack<Tx: TransmitRingBufferInterface>(tx: &Tx, seq: u32) {
    let base = region_base(seq);
    let ack = ((ACK_MAGIC as u64) << 32) | seq as u64;
    tx.write_slice(&[u64_to_word(ack)], base + ACK_OFFSET);
}

fn read_ack<Rx: ReceiveRingBufferInterface>(rx: &Rx, seq: u32) -> bool {
    let base = region_base(seq);
    let mut buf = [[0u8; 8]; 1];
    rx.read_slice(&mut buf, base + ACK_OFFSET);
    word_to_u64(buf[0]) == ((ACK_MAGIC as u64) << 32) | seq as u64
}

/// Polls for the ack of `seq`. `true` when observed, `false` on timeout.
fn poll_ack<Rx: ReceiveRingBufferInterface>(
    rx: &Rx,
    timer: &Timer,
    seq: u32,
    timeout: u32,
) -> bool {
    let deadline = now_cycles(timer) + timeout as u64;
    loop {
        if read_ack(rx, seq) {
            return true;
        }
        if now_cycles(timer) > deadline {
            return false;
        }
    }
}

/// Region reuse gate: waits for the ack of whatever frame was last sent in
/// `seq`'s region (`true` when the region may be rewritten).
fn region_free<Rx: ReceiveRingBufferInterface>(
    rx: &Rx,
    timer: &Timer,
    sent_in_region: &[Option<u32>; REGION_COUNT],
    seq: u32,
    timeout: u32,
) -> bool {
    match sent_in_region[(seq as usize) % REGION_COUNT] {
        None => true,
        Some(prev) => poll_ack(rx, timer, prev, timeout),
    }
}

/// Reads the trailer slot of `seq`'s region once, returning
/// `(seq_plus_one, checksum)` of whatever occupies it (0 = empty).
fn peek_trailer<Rx: ReceiveRingBufferInterface>(rx: &Rx, seq: u32) -> (u32, u32) {
    let base = region_base(seq);
    let mut buf = [[0u8; 8]; 1];
    rx.read_slice(&mut buf, base + TRAILER_OFFSET);
    let w = word_to_u64(buf[0]);
    ((w >> 32) as u32, w as u32)
}

fn read_payload<Rx: ReceiveRingBufferInterface>(
    rx: &Rx,
    seq: u32,
    vector_words: usize,
    payload: &mut [[u8; 8]; MAX_VECTOR_WORDS],
) {
    rx.read_slice(&mut payload[..vector_words], region_base(seq));
}

/// Polls for frame `seq`. `true` and fills `payload` when received intact;
/// `false` when lost (overwritten by a later frame in the same region, poll
/// timeout, or trailer/payload checksum mismatch after a re-read).
/// On failure the raw trailer word occupying the region is left in
/// `LAST_SEEN_TRAILER` for diagnostics.
static mut LAST_SEEN_TRAILER: u64 = 0;

fn poll_frame<Rx: ReceiveRingBufferInterface>(
    rx: &Rx,
    timer: &Timer,
    seq: u32,
    vector_words: usize,
    timeout: u32,
    payload: &mut [[u8; 8]; MAX_VECTOR_WORDS],
) -> bool {
    let deadline = now_cycles(timer) + timeout as u64;
    let want = seq + 1;
    loop {
        let (got, got_checksum) = peek_trailer(rx, seq);
        unsafe {
            core::ptr::addr_of_mut!(LAST_SEEN_TRAILER)
                .write_volatile(((got as u64) << 32) | got_checksum as u64)
        };
        if got == want {
            read_payload(rx, seq, vector_words, payload);
            // The region may have been overwritten between trailer and
            // payload reads; the checksum in the trailer detects it.
            if checksum32(&payload[..vector_words]) == got_checksum {
                return true;
            }
        } else if got > want && ((got - want) as usize).is_multiple_of(REGION_COUNT) {
            // A later frame overwrote the region: `seq` is lost.
            return false;
        }
        if now_cycles(timer) > deadline {
            return false;
        }
    }
}

/// Prepares the transmit buffers: only the trailer and ack slots can alias a
/// later run's sequence numbers (payload is read positionally after a trailer
/// match and verified by checksum), so clearing those 16 words — instead of
/// both full buffers — keeps this far cheaper than one frame's service time.
fn prepare_buffers<Rx: ReceiveRingBufferInterface, Tx: TransmitRingBufferInterface>(
    bufs: &DecodeBuffers<Rx, Tx>,
) {
    let zero = [[0u8; 8]; 1];
    for region in 0..REGION_COUNT {
        let base = region * REGION_STRIDE;
        bufs.down_tx.write_slice(&zero, base + TRAILER_OFFSET);
        bufs.down_tx.write_slice(&zero, base + ACK_OFFSET);
        bufs.up_ack_tx.write_slice(&zero, base + TRAILER_OFFSET);
        bufs.up_ack_tx.write_slice(&zero, base + ACK_OFFSET);
    }
    bufs.down_tx.set_enable(true);
    bufs.up_ack_tx.set_enable(true);
    bufs.up_rx.set_enable(true);
    bufs.down_ack_rx.set_enable(true);
}

fn make_contribution(pattern: u64, vector_words: usize) -> [[u8; 8]; MAX_VECTOR_WORDS] {
    let mut payload = [[0u8; 8]; MAX_VECTOR_WORDS];
    for (i, w) in payload.iter_mut().enumerate().take(vector_words) {
        *w = u64_to_word(pattern.wrapping_add(i as u64));
    }
    payload
}

fn add_contribution(payload: &mut [[u8; 8]; MAX_VECTOR_WORDS], pattern: u64, vector_words: usize) {
    for (i, w) in payload.iter_mut().enumerate().take(vector_words) {
        *w = u64_to_word(word_to_u64(*w).wrapping_add(pattern.wrapping_add(i as u64)));
    }
}

/// Variant C: rendezvous by polling, acks on the reverse link direction.
pub fn run_variant_c<Rx: ReceiveRingBufferInterface, Tx: TransmitRingBufferInterface>(
    cfg: &DecodeConfig,
    timer: &Timer,
    uart: &mut Uart,
    bufs: &DecodeBuffers<Rx, Tx>,
    results: &mut DecodeResults,
) {
    results.reset(MODE_C, cfg.hist_base, cfg.hist_shift);
    prepare_buffers(bufs);
    let vw = (cfg.vector_words as usize).min(MAX_VECTOR_WORDS);
    let injector = cfg.is_injector != 0;
    let mut payload = [[0u8; 8]; MAX_VECTOR_WORDS];
    // A small budget of diagnostic prints, so the first few failures explain
    // themselves without flooding the UART.
    let mut diag_budget: u32 = 8;
    uwriteln!(
        uart,
        "C cfg inj={} up={} down={} vw={} n={} pat={:x} expA={:x} expB={:x}",
        cfg.is_injector,
        cfg.read_link,
        cfg.write_link,
        cfg.vector_words,
        cfg.token_count,
        cfg.local_pattern,
        cfg.expected_window_a,
        cfg.expected_window_b
    )
    .unwrap();
    // Region reuse is gated on the ack of the frame LAST SENT in that
    // region (not blindly seq - 4): after an abandoned token some sequence
    // numbers were never sent, and waiting for their acks would cascade the
    // loss into every following token.
    let mut sent_in_region: [Option<u32>; REGION_COUNT] = [None; REGION_COUNT];

    // Wait out one buffer wrap so cleared transmit buffers have propagated,
    // then gate on the common start cycle.
    let _ = timer.wait_until_stall_raw(now_cycles(timer) + 2 * Tx::DATA_LEN as u64);
    let _ = timer.wait_until_stall_raw(cfg.first_cycle);
    uwriteln!(uart, "C start").unwrap();

    'tokens: for token in 0..cfg.token_count {
        if token % 256 == 0 && token != 0 {
            uwriteln!(
                uart,
                "C progress: t={} lost={} fails={}",
                token,
                results.lost_frames,
                results.checksum_fails
            )
            .unwrap();
        }
        let token_start = now_cycles(timer);
        for layer in 0..cfg.layers_per_token {
            let seq1 = (token * cfg.layers_per_token + layer) * 2;
            let seq2 = seq1 + 1;

            if injector {
                // Send our contribution around the ring (lap 1). Region
                // reuse is guarded by the ack of the frame four sequence
                // numbers ago.
                if !region_free(
                    bufs.down_ack_rx,
                    timer,
                    &sent_in_region,
                    seq1,
                    cfg.poll_timeout,
                ) {
                    results.lost_frames += 1;
                    continue 'tokens;
                }
                let contribution = make_contribution(cfg.local_pattern, vw);
                send_frame(bufs.down_tx, seq1, &contribution[..vw]);
                sent_in_region[(seq1 as usize) % REGION_COUNT] = Some(seq1);

                // Turnaround: the accumulated lap-1 vector returns; verify
                // the grand total and forward it into lap 2.
                if !poll_frame(bufs.up_rx, timer, seq1, vw, cfg.poll_timeout, &mut payload) {
                    results.lost_frames += 1;
                    if diag_budget > 0 {
                        diag_budget -= 1;
                        let saw = unsafe { core::ptr::addr_of!(LAST_SEEN_TRAILER).read_volatile() };
                        uwriteln!(uart, "C lost turn tok={} seq={} saw={:x}", token, seq1, saw)
                            .unwrap();
                    }
                    continue 'tokens;
                }
                write_ack(bufs.up_ack_tx, seq1);
                if checksum64(&payload[..vw]) != cfg.expected_window_b {
                    results.checksum_fails += 1;
                    if diag_budget > 0 {
                        diag_budget -= 1;
                        uwriteln!(
                            uart,
                            "C sum turn tok={} got={:x} want={:x}",
                            token,
                            checksum64(&payload[..vw]),
                            cfg.expected_window_b
                        )
                        .unwrap();
                    }
                }
                if !region_free(
                    bufs.down_ack_rx,
                    timer,
                    &sent_in_region,
                    seq2,
                    cfg.poll_timeout,
                ) {
                    results.lost_frames += 1;
                    continue 'tokens;
                }
                send_frame(bufs.down_tx, seq2, &payload[..vw]);
                sent_in_region[(seq2 as usize) % REGION_COUNT] = Some(seq2);

                // Sink: the broadcast lap returns.
                if !poll_frame(bufs.up_rx, timer, seq2, vw, cfg.poll_timeout, &mut payload) {
                    results.lost_frames += 1;
                    continue 'tokens;
                }
                write_ack(bufs.up_ack_tx, seq2);
                if checksum64(&payload[..vw]) != cfg.expected_window_b {
                    results.checksum_fails += 1;
                }
            } else {
                // Relay: lap 1 — verify the prefix sum, add our
                // contribution, forward.
                if !poll_frame(bufs.up_rx, timer, seq1, vw, cfg.poll_timeout, &mut payload) {
                    results.lost_frames += 1;
                    if diag_budget > 0 {
                        diag_budget -= 1;
                        let saw = unsafe { core::ptr::addr_of!(LAST_SEEN_TRAILER).read_volatile() };
                        uwriteln!(uart, "C lost lap1 tok={} seq={} saw={:x}", token, seq1, saw)
                            .unwrap();
                    }
                    continue 'tokens;
                }
                write_ack(bufs.up_ack_tx, seq1);
                if checksum64(&payload[..vw]) != cfg.expected_window_a {
                    results.checksum_fails += 1;
                    if diag_budget > 0 {
                        diag_budget -= 1;
                        uwriteln!(
                            uart,
                            "C sum lap1 tok={} got={:x} want={:x}",
                            token,
                            checksum64(&payload[..vw]),
                            cfg.expected_window_a
                        )
                        .unwrap();
                    }
                }
                if !region_free(
                    bufs.down_ack_rx,
                    timer,
                    &sent_in_region,
                    seq1,
                    cfg.poll_timeout,
                ) {
                    results.lost_frames += 1;
                    continue 'tokens;
                }
                add_contribution(&mut payload, cfg.local_pattern, vw);
                send_frame(bufs.down_tx, seq1, &payload[..vw]);
                sent_in_region[(seq1 as usize) % REGION_COUNT] = Some(seq1);

                // Lap 2 — verify the grand total, forward unchanged.
                if !poll_frame(bufs.up_rx, timer, seq2, vw, cfg.poll_timeout, &mut payload) {
                    results.lost_frames += 1;
                    continue 'tokens;
                }
                write_ack(bufs.up_ack_tx, seq2);
                if checksum64(&payload[..vw]) != cfg.expected_window_b {
                    results.checksum_fails += 1;
                }
                if !region_free(
                    bufs.down_ack_rx,
                    timer,
                    &sent_in_region,
                    seq2,
                    cfg.poll_timeout,
                ) {
                    results.lost_frames += 1;
                    continue 'tokens;
                }
                send_frame(bufs.down_tx, seq2, &payload[..vw]);
                sent_in_region[(seq2 as usize) % REGION_COUNT] = Some(seq2);
            }

            if cfg.compute_cycles != 0 && layer + 1 < cfg.layers_per_token {
                timer.wait_stall(bittide_hal::manual_additions::timer::Duration::from_cycles(
                    cfg.compute_cycles,
                    timer.frequency().into_inner(),
                ));
            }
        }
        let latency = (now_cycles(timer) - token_start) as u32;
        results.record_latency(latency);
        results.tokens_done += 1;
    }
}

/// Variant A′: the same frame operations, fired by timer deadlines from the
/// UGN-derived schedule. No polls, no acks; a frame that is not present at
/// its deadline (or a deadline already passed) counts as missed and the
/// token is abandoned.
pub fn run_variant_a_prime<Rx: ReceiveRingBufferInterface, Tx: TransmitRingBufferInterface>(
    cfg: &DecodeConfig,
    timer: &Timer,
    uart: &mut Uart,
    bufs: &DecodeBuffers<Rx, Tx>,
    results: &mut DecodeResults,
) {
    results.reset(MODE_A_PRIME, cfg.hist_base, cfg.hist_shift);
    prepare_buffers(bufs);
    let vw = (cfg.vector_words as usize).min(MAX_VECTOR_WORDS);
    let injector = cfg.is_injector != 0;
    let mut payload = [[0u8; 8]; MAX_VECTOR_WORDS];

    let wait = |target: u64| -> bool {
        match timer.wait_until_stall_raw(target) {
            WaitResult::Success => true,
            WaitResult::AlreadyPassed => false,
        }
    };

    uwriteln!(uart, "A' start").unwrap();
    'tokens: for token in 0..cfg.token_count {
        if token % 64 == 0 && token != 0 {
            uwriteln!(
                uart,
                "A' progress: t={} missed={} fails={}",
                token,
                results.deadlines_missed,
                results.checksum_fails
            )
            .unwrap();
        }
        let token_base = cfg.first_cycle + (token as u64) * (cfg.token_period as u64);
        let token_start = now_cycles(timer);
        for layer in 0..cfg.layers_per_token {
            let seq1 = (token * cfg.layers_per_token + layer) * 2;
            let seq2 = seq1 + 1;
            let layer_base = token_base + (layer as u64) * (cfg.layer_period as u64);
            let lap = cfg.lap_offset as u64;

            // Window scheme mirrors the hardware calendar: the injector has
            // three windows per layer at offsets {0, lap, 2*lap}; relays have
            // two at {0, lap}.
            if injector {
                if !wait(layer_base) {
                    results.deadlines_missed += 1;
                    continue 'tokens;
                }
                let contribution = make_contribution(cfg.local_pattern, vw);
                send_frame(bufs.down_tx, seq1, &contribution[..vw]);

                if !wait(layer_base + lap) {
                    results.deadlines_missed += 1;
                    continue 'tokens;
                }
                let (got1, _) = peek_trailer(bufs.up_rx, seq1);
                if got1 != seq1 + 1 {
                    results.deadlines_missed += 1;
                    continue 'tokens;
                }
                read_payload(bufs.up_rx, seq1, vw, &mut payload);
                if checksum64(&payload[..vw]) != cfg.expected_window_b {
                    results.checksum_fails += 1;
                }
                send_frame(bufs.down_tx, seq2, &payload[..vw]);

                if !wait(layer_base + 2 * lap) {
                    results.deadlines_missed += 1;
                    continue 'tokens;
                }
                let (got2, _) = peek_trailer(bufs.up_rx, seq2);
                if got2 != seq2 + 1 {
                    results.deadlines_missed += 1;
                    continue 'tokens;
                }
                read_payload(bufs.up_rx, seq2, vw, &mut payload);
                if checksum64(&payload[..vw]) != cfg.expected_window_b {
                    results.checksum_fails += 1;
                }
            } else {
                if !wait(layer_base) {
                    results.deadlines_missed += 1;
                    continue 'tokens;
                }
                let (got1, _) = peek_trailer(bufs.up_rx, seq1);
                if got1 != seq1 + 1 {
                    results.deadlines_missed += 1;
                    continue 'tokens;
                }
                read_payload(bufs.up_rx, seq1, vw, &mut payload);
                if checksum64(&payload[..vw]) != cfg.expected_window_a {
                    results.checksum_fails += 1;
                }
                add_contribution(&mut payload, cfg.local_pattern, vw);
                send_frame(bufs.down_tx, seq1, &payload[..vw]);

                if !wait(layer_base + lap) {
                    results.deadlines_missed += 1;
                    continue 'tokens;
                }
                let (got2, _) = peek_trailer(bufs.up_rx, seq2);
                if got2 != seq2 + 1 {
                    results.deadlines_missed += 1;
                    continue 'tokens;
                }
                read_payload(bufs.up_rx, seq2, vw, &mut payload);
                if checksum64(&payload[..vw]) != cfg.expected_window_b {
                    results.checksum_fails += 1;
                }
                send_frame(bufs.down_tx, seq2, &payload[..vw]);
            }
        }
        let latency = (now_cycles(timer) - token_start) as u32;
        results.record_latency(latency);
        results.tokens_done += 1;
    }
}

/// Measures the cycles one frame's service takes on this CPU (contribution
/// build + payload/trailer write + payload read-back + checksum), feeding the
/// wrap-window margin check: the result must stay well under the ring-buffer
/// depth.
pub fn measure_frame_service_cycles<
    Rx: ReceiveRingBufferInterface,
    Tx: TransmitRingBufferInterface,
>(
    cfg: &DecodeConfig,
    timer: &Timer,
    bufs: &DecodeBuffers<Rx, Tx>,
) -> u32 {
    let vw = (cfg.vector_words as usize).min(MAX_VECTOR_WORDS);
    let mut payload = [[0u8; 8]; MAX_VECTOR_WORDS];
    let start = now_cycles(timer);
    let contribution = make_contribution(cfg.local_pattern, vw);
    send_frame(bufs.down_tx, 0, &contribution[..vw]);
    read_payload(bufs.up_rx, 0, vw, &mut payload);
    let _ = checksum64(&payload[..vw]);
    (now_cycles(timer) - start) as u32
}
