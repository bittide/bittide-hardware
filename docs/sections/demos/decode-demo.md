<!--
SPDX-FileCopyrightText: 2026 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Decode Demo

The decode demo measures what a workload gains when communication needs no
runtime coordination. It runs the same distributed computation — a serial loop
of "tokens", each token a sequence of "layers", each layer one small ring
all-reduce over all eight FPGAs — under several transport disciplines, on the
same bitstream, over the same links, and records a per-token latency histogram
for each. The workload shape mimics LLM decoding, the regime where collectives
are latency-bound and per-hop fixed costs (handshakes, credits, polling, acks)
dominate; the quantitative background is the model in the
`scaling-book` repository's `bittide-inference-model`, and the design
rationale is in `PLAN.md` at the repository root.

## What maps to what

| Rig variant | Data path | Real-world class |
|---|---|---|
| A — scheduled | hardware PE, fires at `localCounter == t` | bittide product |
| B — hardware async (cut-through) | same PE datapath behind a credit/valid-ack link layer | best-case hardware flow control (TPU-ICI / NVLink class) |
| B_sf — hardware async (store-and-forward) | as B, but relays buffer and validate each frame before forwarding | hardware flow control with per-hop validation |
| C — software async | management-unit rendezvous over ring buffers | NIC-doorbell / NCCL-proxy transport |
| A′ — software scheduled | same engine as C, fired by timer deadlines | isolates CPU-engine cost from protocol cost |

Scope honesty, which belongs in every use of these numbers: every variant runs
on bittide's synchronized clocks, over point-to-point links, with zero
contention. The rig demonstrates **coordination overhead**; it cannot
demonstrate congestion, multi-tenant arbitration, or OS-noise jitter, because
none exist here. Variant C's smear (soft-core instruction timing) is real but
is not a reproduction of any production fabric's measured per-hop cost.

## The computation

A systolic two-lap ring all-reduce
(`Bittide.DecodeProcessingElement.decodeReduceCore`): in lap 1 the injector
(node 0) streams `local_pattern + i` for `vector_words` 64-bit words and every
relay adds its own contribution flow-through (one register stage per hop); in
lap 2 the summed vector travels the ring again so every node observes the
grand total, and the injector sinks it, latching the token latency. Every
window is checksummed against host-computed expectations (contributions derive
from FPGA DNAs), so data integrity is verified end to end in every variant.

## Architecture

The demo instantiates the `GenericDemo` fabric (as the wire demo does) with a
three-bus user core:

- `DecodePeConfig` — the PE's workload/schedule registers, status counters and
  a 16-bin, 1-cycle-wide latency histogram, plus a write-only `arm` register
  that clears all status and (re)starts a run: variants A, B and B_sf run back
  to back without a reset.
- `CreditLinkConfig` — the credit link's knobs (`cut_through`,
  `credit_timeout`, `credit_pulse_len`) and its flow-control accounting
  (credits consumed/returned/granted, frames, header/credit errors, drops,
  timeouts, min/max credit round-trip time).
- `ProgrammableMux` — the wire demo's one-shot mux: the management unit owns
  the links until it is armed, then the PE does. Consequently the driver runs
  the management-unit variants (C, A′) strictly before the hardware variants.

In credit mode, frames are `HEADER(seq)` + payload on the ring's forward
direction; the receiver returns a `CREDIT` word on the reverse direction of
the same physical link. One credit is outstanding per link. Cut-through relays
regenerate the header one cycle after detecting it — the credit round trip
hides off the critical path whenever the ring's dependency chain is longer
than one hop, which is the pre-registered prediction (`PLAN.md` §2): B ≈ A +
framing, with the handshake visible in the measured per-hop credit RTT and,
on the critical path, in store-and-forward mode.

## Running

```
shake decodeDemoTest:test
```

Tests are configured to run the following binaries on the system's CPUs:

- Boot CPU: `decode-demo-boot` (`firmware-binaries/demos/decode-demo-boot`)
- Clock control CPU: `decode-demo-clock-control`
  (`firmware-binaries/demos/decode-demo-clock-control`)
- Management unit: `decode-demo-management-unit`
  (`firmware-binaries/demos/decode-demo-management-unit`)

The driver (`Bittide.Instances.Hitl.DecodeDemo.Driver`) reads the hardware
UGNs, computes the ring schedule with `Bittide.Calculator`, then runs
C → A′ → (mux arm) → A → B → B_sf, asserting per variant: zero checksum
failures, all tokens completed, variant A (and B) constant to within a few
cycles, and clean credit accounting. Latency dumps land in
`_build/hitl/Decode_Demo_DUT/`; post-processing
(`Bittide.Instances.Hitl.DecodeDemo.PostProc`) pools them into
`decode-demo-hist.csv` and `decode-demo-summary.txt`.

## Reading the result

The headline figure is the overlay of per-token latency histograms. The
pre-registered expectations: A is a spike, constant to the cycle; B matches A
to within per-hop framing, its handshake cost visible as the measured credit
RTT; B_sf shifts right by roughly `vector_words` + handshake slack per hop;
C is a smear with a displaced mean — the software-path penalty — and A′ sits
between, isolating the engine cost from the protocol cost. Deviations from
these predictions are findings, not failures.

## Measured results

From the rig (2026-08-20; 8 nodes, 16 layers/token, 64×64-bit words per
all-reduce, 125 MHz link clock; 10,000 tokens per hardware variant, 500 for
C, 200 for A′). "Latency" is the injector's per-token latency: the full
16-layer chain of complete all-reduces.

| Variant | Token latency (cycles) | Spread | vs. B |
|---|---|---|---|
| A — scheduled | 10,128 (81.0 µs) | 0 (min == max) | +2.3% |
| B — hardware async, cut-through | 9,903 (79.2 µs) | 0 (min == max) | 1× |
| B_sf — hardware async, store-and-forward | 25,743 (205.9 µs) | 0 (min == max) | 2.60× |
| A′ — software scheduled | 22,895,335 (183 ms) | 0 (one 512-cycle bin) | 2,312× |
| C — software async | 13.80M–13.86M (~110 ms) | ~68k cycles, multi-modal | ~1,394× |

Zero checksum failures, lost frames, or missed deadlines in any variant; the
credit link's accounting is exact (320,000 frames = credits consumed =
returned = granted, zero header/credit errors, drops or timeouts).

What the numbers say:

- B's 9,903 = 16 × (2·`lap_offset` + `vector_words` + 1) is the ring's
  dataflow optimum, and — as pre-registered after analysis — the credit
  handshake hides off the critical path in cut-through mode: its cost shows
  up only in the measured per-hop credit RTT (130–143 cycles cut-through,
  ~198–211 store-and-forward), not in token latency.
- A, fired purely by `localCounter == t` comparisons against a schedule
  computed once from the UGNs, lands within 16 cycles per layer of that
  optimum — the margin is a driver constant, and every one of the 80,000
  windows across A and its probe run hit exactly.
- B_sf puts the per-hop cost on the critical path: +990 cycles per layer ≈
  14 relay hops × (64-word frame buffering + ~7 cycles of handshake slack) —
  the "validate before forwarding" penalty the async-fabric comparison is
  about, still perfectly deterministic.
- C pays ~1,400× over the hardware floor for running the same rendezvous in
  management-unit firmware (~54k cycles per hop service), and it alone shows
  variance: a ~68k-cycle multi-modal smear from instruction-timing beats
  between the eight soft cores.
- A′ — the same software engine, but timer-fired on the shared clock — is
  slower still in the mean (its schedule must reserve worst-case per-hop
  service), yet its variance collapses to zero: scheduling, not hardware,
  is what removes the jitter.

Scope honesty applies unchanged: all variants share bittide's synchronized
clocks and contention-free links, so these numbers isolate coordination
overhead only.

## Contention

The quiet numbers above concede that async's parity is a property of an
unloaded fabric. The contention experiment (`PLAN2.md` at the repository
root) adds a second, verified traffic flow on the same ring links: a traffic
generator per node sends fixed-size 64-word bursts to its ring neighbor,
pattern-checked end to end, at 19/48/68% link duty. Under the calendar
(A_c) the generator's bursts are slots placed in the decode windows' idle
gaps, computed jointly by the driver; a hardware collision counter proves
disjointness. Under credit flow control (B_c) both flows run the credit
protocol and meet at a work-conserving round-robin frame arbiter with
opportunistic cut-through — deliberately fair to the async design.

Measured (rig, 2026-08-22; injector token latency over 10,000 tokens per
cell; quiet floors that boot: A 10,448, B 10,223):

| Duty | A_c decode | A_c generator | B_c decode | B_c generator |
|---|---|---|---|---|
| 19% | 10,448, spread 0 | 340,512/340,512, 0 errors, max queue 0 | 10,421..10,487 | 938,492/938,492, 0 errors, max queue 4 |
| 48% | 10,448, spread 0 | 851,280/851,280, 0 errors, max queue 0 | 10,421..10,487 | 2,346,230/2,346,230, 0 errors, max queue 54 |
| 68% | 10,448, spread 0 | 1,191,792/1,191,792, 0 errors, max queue 0 | 10,421..10,487 | 3,284,722/3,284,722, 0 errors, max queue 58 |

What the numbers say:

- **A_c is bit-identical to the quiet run at every duty.** The calendar
  placed up to 68% of competing load into the links' idle cycles with zero
  effect on decode — not a deterministic shift, but literally none, because
  the reserved slots fit in the gaps — and the competing flow itself never
  queued a single cycle. Both flows get exact admission contracts; that is
  the two-sided guarantee.
- **B_c shifts and smears, boundedly.** Decode pays +198..264 cycles over
  the quiet dataflow floor with a 66-cycle spread — almost exactly one
  burst length, the work-conserving arbiter's per-hop worst case — and the
  envelope is duty-independent while the probability mass and the competing
  flow's queueing (max 4 → 54 → 58 cycles) grow with load. The decode
  credit round-trip inflates from a constant 131 to 133..199. Reported
  with the same prominence as the headline: a well-designed single-hop
  arbiter *holds* under this offered load — lossless, bounded interference
  of well under 1% of token latency. The measured contrast is exactness
  and guarantees (zero versus small-and-stochastic), not catastrophe.

What the competing flow does and does not model: a dedicated
NVLink/ICI-class domain running one collective at a time has little internal
contention — that quiet case is the base demo, already measured at parity.
The generator models the shared-domain case serving stacks are moving into
(disaggregated KV-cache transfers riding decode's links, expert-parallel
all-to-alls overlapping tensor-parallel all-reduces). It does NOT model
scale-out spine congestion — an 8-node point-to-point ring has no spine.
The product framing of the result is economic: arbitration makes operators
choose between isolation (wasted capacity) and interference (jitter); a
calendar provides guaranteed latency for every admitted flow at high
utilization.

Two artifacts of the experiment worth naming: the generator's receiver
initially lost ~3 bursts per million to its own credit-grant train blocking
header reception (fixed: headers are accepted mid-train), and the shared
port needed an explicit one-cycle drain between owners because registered
transmit paths trail their state machines (without it, a frame's first word
could overwrite the previous owner's last). Both were found by the
hardware's own accounting counters — the "verified traffic everywhere"
discipline paying for itself.
