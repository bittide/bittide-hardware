<!--
SPDX-FileCopyrightText: 2026 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# PLAN: `decodeDemo` — a statically scheduled "LLM decode" all-reduce on the bittide rig

## 1. Goal

Produce the figure the AI-inference pitch needs: **the same distributed
computation, on the same 8 FPGAs, over the same links, under three transport
disciplines — statically scheduled against bittide's constant logical
latencies, hardware-asynchronous (credit/ack flow control, the ICI/NVLink
class), and software-asynchronous (rendezvous by CPU, the commodity NIC/NCCL
class) — as per-iteration latency histograms.**

Predicted shape, written down before anything is built (pre-registered, so the
result is evidence rather than curve-fitting):

- **A (scheduled)**: a spike, constant to the cycle.
- **B (hardware async)**: a spike shifted right by the per-phase handshake
  round trip — a *mean* penalty, near-zero variance on our quiet fabric. This
  is the fair comparison against the best async design possible on identical
  silicon, and the ICI-class delta.
- **C (software async)**: a smear with a displaced mean — the software-path
  penalty commodity deployments pay.

This matches the scaling-book model's three archetypes (bittide / async-hw /
async-sw). The quantitative background lives in
`~/code/scaling-book/bittide-inference-model/` (a Haskell max-plus model of
tensor-parallel decode: deterministic scheduling is worth 2.2× against an
idealized async hardware fabric and 11.4× against a software stack at 64
chips; `README.md` there documents assumptions, and the "Determinism Dividend"
report renders the argument). This demo is the physical-hardware counterpart
of that model.

The workload mimics LLM decoding, the regime where fabric determinism pays: a
serial loop of "tokens", each token a sequence of "layers", each layer = local
compute followed by a small ring all-reduce. Decode collectives are
latency-bound (tiny messages), so per-hop fixed costs — handshakes, credits,
polling, acks — dominate.

### Scope honesty (goes in the demo writeup verbatim)

- Every variant runs on bittide's synchronized clocks, over point-to-point
  links, with zero contention. The rig demonstrates **coordination overhead**;
  it cannot demonstrate congestion, multi-tenant arbitration, or OS-noise
  jitter, because none exist here. Real async fabrics smear for those reasons
  too — variant C's smear (soft-core instruction timing) is real but is not a
  reproduction of ICI's measured 2.7 µs/hop; that number remains a citation
  (`sharding.md:375` in the scaling book).
- With 8 nodes and 7 ring phases, the ln N straggler term is negligible. The
  demo demonstrates the per-hop fixed-cost argument; the scale argument stays
  with the Haskell model.

Deliverables:
1. `decodeDemoTest` HITL test running all variants back to back in CI.
2. Per-token latency histograms + met/missed-deadline counters per node,
   dumped to `_build/hitl/decodeDemoTest/`, post-processed to CSV + one PDF
   figure (A/B/C histogram overlay + tokens/s and mean/p99 table).
3. Data-integrity check: every all-reduce result verified against the expected
   sum (contributions derived from FPGA DNAs, wire-demo style).

## 2. Demo definition

**Base: the wire demo, not the soft-UGN demo.** Hardware-accelerated UGN
capture (`Bittide.CaptureUgn`, read by the driver through the memory map
exactly as `WireDemo/Driver.hs:readHardwareUgns` does), hardware PEs on the
raw link taps for the scheduled and hardware-async datapaths, and small ring
buffers — no soft-UGN discovery protocol, no depth-4000 buffers (the soft-UGN
demo's 14 × 32 KB ≈ 450 KB of BRAM per node buys nothing here).

- **Topology**: 8-node ring, chosen as a link subset of the complete K8 in
  `bittide-instances/src/Bittide/Instances/Hitl/Setup.hs` (`fpgaSetup` gives
  the physical link → neighbor table; one TX + one RX link per node for the
  demo; clock control keeps using all 7 links for maximal syntony stiffness).
- **Workload parameters** (host-writable config registers / GDB-written
  struct, so sweeps need no rebuild): `vectorWords` (default 64 → 512 B per
  phase; small on purpose: latency-bound), `layersPerToken` (default 16),
  `computeCycles` (emulated per-layer compute delay; sweep {0, 1k, 10k,
  100k}), `nTokens` (default 10_000).
- **All-reduce**: 7-phase ring reduce — each node adds its contribution to the
  passing vector and forwards; after 7 hops every node holds the total.
  Element type `u64`, wrapping add. Node contribution = a per-node pattern
  derived from `dna`, so the expected total is computable by the host and by
  every node. (Reduce-scatter + all-gather is a stretch goal.)
- **Token latency** = cycles between token start and the last phase's verified
  completion, measured against the shared free-running `localCounter` (the
  `Timer` device is bound to it, so all nodes measure in one cycle base).

### What maps to what

| Rig variant | Data path | Real-world class | Model archetype |
|---|---|---|---|
| A — scheduled | HW PE, fires at `localCounter == t` | bittide product | `bittide` |
| B — hardware async | same HW PE datapath + credit/valid-ack link layer | TPU ICI / NVLink-class hardware flow control | `async-hw` |
| C — software async | MU firmware rendezvous over ring buffers | NIC doorbell / NCCL-proxy transport | `async-sw` |
| A′ — software scheduled (optional control) | MU firmware, timer-fired | — (isolates CPU-engine cost from protocol cost) | — |

### Variant A — Scheduled: hardware PE on raw link taps

The wire demo pattern, extended from "XOR one word" to "stream-reduce a
vector": a `decodePe` sitting on `RXS2_RAW` (pre-handshake elastic-buffer tap)
and `GTH_TX` via a `programmableMux`, modeled directly on
`Bittide.WireDemoProcessingElement` + `Bittide.ProgrammableMux`. Config
registers (MU bus, host-written like `WireDemoPeConfig`):

- `read_link`, `write_link` — ring neighbor selection per node;
- `first_cycle : Unsigned 64` — absolute `localCounter` value of the first
  phase's first word (the wire demo's `first_b_cycle` arming pattern);
- `phase_period`, `phases_per_token`, `token_period`, `token_count`,
  `vector_words` — a periodic re-fire schedule, i.e. a deliberately minimal
  calendar;
- `local_pattern` — the node's contribution seed;
- status: `tokens_done`, `checksum_fail_count`, per-token completion-cycle
  histogram RAM (host-readable).

The PE consumes one 64-bit word per cycle at line rate: read incoming word,
add local contribution element, emit. Cost per phase = fabric latency +
`vectorWords` cycles + schedule margin. No CPU on the data path; no polling;
no flow control; nothing to jitter. The host computes `first_cycle` per node
from the hardware UGNs with `Bittide.Calculator`
(`toFpgaIndexed`/`toCounterMap`), exactly as the wire demo's
`generateSchedule` does, including an empirically determined
`internalDelay`-style constant.

### Variant B — Hardware async: credit/valid-ack link layer, same PE

The same streaming-reduce PE datapath, but transfers are initiated by flow
control instead of the calendar: the sender transmits a frame when it holds a
credit; the receiver's link layer detects the frame header, streams it into
the PE, and returns an ack/credit word on the reverse link; the sender blocks
until the credit returns. All in Clash — **no CPU in the loop**. This is the
best asynchronous design possible on this silicon, and the honest stand-in
for hardware flow-controlled fabrics (ICI, NVLink): schedule-free, but paying
a handshake round trip and header/framing per phase.

Expected overhead vs A — revised after working the pipeline analysis, before
anything was built (this supersedes an earlier +70–120 cycles/phase guess):
with cut-through relays and one credit outstanding, consecutive transfers on
the same hop are a full ring traversal apart, so the credit round trip is
*hidden* off the critical path. Cut-through B is therefore predicted at
A + ~2 cycles/hop of framing (~3% per token), with the handshake cost visible
as the measured per-hop credit RTT (~130–140 cycles, near-zero variance) in
the link layer's status registers. To also expose the handshake *on* the
critical path, B additionally has a config-selectable **store-and-forward
mode (B_sf)**: each relay buffers the full frame and forwards only after
complete reception + sequence check — the discipline of fabrics that validate
per hop — predicted at roughly +(vectorWords + credit slack) per hop, i.e.
within the band originally guessed for B. The figure gets both hardware-async
points, labeled. If either histogram is wider than predicted, that is a
finding about hardware handshake determinism; report it either way.

Design note: keep the link-layer state machine minimal (IDLE → SEND → AWAIT
CREDIT; receiver mirror image), one credit outstanding, header word carries
phase index for integrity checking. No retransmission — links are reliable
here; say so in the writeup (a real async fabric pays retransmission machinery
on top, which favors bittide further).

### Variant C — Software async: MU firmware + ring buffers

A software rendezvous protocol over the ring buffers, deliberately shaped like
a conventional NIC/driver path: sender writes frame + sequence-number/valid
marker, polls for the receiver's ack marker before reusing the slot window;
receiver polls for the marker, copies via `read_slice`/`write_slice`, runs the
reduce step on the MU, writes ack. No artificial delays — all smear must come
from real polling granularity, instruction timing, copy cost, and ack round
trips, or the figure is propaganda.

Label discipline: C is presented as the *software-transport* class, never as
"what ICI does". The VexRiscv is slow relative to the links (much slower than
a real NIC path relative to NVLink), so C's ratio to A overstates what a tuned
production stack loses; the writeup says this and points at B for the
hardware-class comparison.

### Variant A′ — optional control: software-scheduled

Same MU engine as C, but events fired by `timer.wait_until_stall_raw(cycle)`
against the UGN-derived schedule instead of polling/acks (the
deadline-accounting pattern from the soft-UGN firmware's `bittide_ugn.h`).
A′ vs C isolates protocol cost with the engine held constant; A′ vs A isolates
engine cost with the discipline held constant. Cheap once C exists; pre-empts
the "hardware vs software is unfair" objection.

### Expected magnitudes (recognize success/failure)

- Hop logical latency: ~34 cycles one-way (~270 ns); RTT 67–70 cycles.
- A: phase ≈ `vectorWords` (64) + hop (~34) + margin ≈ 100–150 cycles ≈ ~1 µs;
  all-reduce ≈ 7–10 µs, constant to the cycle. Token (16 layers) ≈ 120–160 µs
  + compute.
- B (cut-through): ≈ A + ~2 cycles/hop framing (~+32 cycles/layer, ~3% per
  token); per-hop credit RTT ~130–140 cycles reported from status registers,
  variance near zero.
- B_sf (store-and-forward): + ~(vectorWords + credit slack) ≈ +70–150 cycles
  per hop → all-reduce ~1.5–2× A's, variance ≪ mean shift.
- C: MU copy floor is a few hundred cycles per 64-word frame each direction,
  plus ack traversal and two poll quantizations per phase — several × A's
  mean with a visible tail.
- Deviations from these predictions are findings, not failures — report them.

## 3. Implementation approach

**Clash work is the critical path** (the `decodePe`, its config device, and
the variant-B link layer), but each piece is a modest extension of existing,
working components (`WireDemoProcessingElement` is a 3-state mealy machine;
`ProgrammableMux` is the arming pattern; the B link layer shares the PE's
stream interface). Everything else — `GenericDemo.{Core,BringUp,TopEntity,
MemoryMaps}`, the driver skeleton, OpenOCD/GDB plumbing, UGN reading, schedule
calculation — is reused from the wire demo unchanged.

**Ring buffers: small.** Variants A and B bypass them entirely (raw taps);
only variant C reads/writes them from the MU. Depth **512** (4 KB × 14 = 56 KB
BRAM per node): wrap window 512 cycles ≈ 4.1 µs must exceed the MU's per-frame
service time — tight-loop copy of 64 words is a few hundred cycles, ~4×
headroom (wire demo's 200 would not suffice; soft-UGN's 4000 is waste).
Requires one new `impl_ring_buffer_interfaces!` invocation in
`firmware-support/bittide-hal/src/manual_additions/ring_buffer.rs` (currently
only depths 16 and 4000 exist) — one macro line.

**Alignment**: skip the marker-based `align_ring_buffers` protocol. With
hardware UGNs in hand, the driver (or MU, from `capture_ugns`) sets each RX
buffer's `clear_at_count` so TX slot *i* lands in RX slot *i* — alignment
becomes arithmetic, not a protocol. Fall back to the marker protocol only if
the arithmetic route hits an unknown offset (then measure the constant and
retire it).

**Latency measurement**: A and B latch per-token completion cycles in the PE's
status/histogram RAM (host reads via memory map). C/A′ bin in MU data memory
(16 KB is ample: 256 log-spaced u32 bins per mode + last-512 raw samples +
counters ≈ 6 KB), dumped with `Gdb.dumpMemoryRegion` — the `dumpCcSamples`
pattern. UART carries one-line summaries only.

## 4. Work breakdown

### Phase 0 — Simulation prototype (de-risk before touching the rig)

- [ ] `bittide-instances/src/Bittide/Instances/Tests/DecodeLoop.hs`: 2-node
      Clash sim (pattern: `Tests/RingBuffer.hs` with its configurable-latency
      delay line): `decodePe` ↔ delay line ↔ `decodePe`, fixed fake UGN.
      Verify streaming reduce, periodic re-fire, checksum, latched latencies —
      in both scheduled (A) and credit/ack (B) modes.
- [ ] `firmware-binaries/sim-tests/decode_loop_test`: the MU async engine (C:
      marker/ack) and the A′ timed engine against the sim DUT's ring buffers;
      measure copy cost per frame (feeds depth/margin choices).
- Exit criteria: all four disciplines produce correct sums in sim; per-frame
  MU service time and B's per-phase handshake cost measured in sim.

### Phase 1 — Clash: PE + link layer + demo skeleton

- [ ] `bittide/src/Bittide/DecodeProcessingElement.hs`: streaming reduce PE +
      periodic arming (registers as in §2A), unit tests in `bittide/tests`.
      Crib the deleted calendar only if the periodic-register design gets
      awkward: `git show 'b42e5f73^:bittide/src/Bittide/Calendar.hs'`.
- [ ] `bittide/src/Bittide/CreditLink.hs`: the variant-B credit/valid-ack link
      layer (sender + receiver state machines, header framing, one credit
      outstanding), sharing the PE stream interface; unit tests including
      artificial ack delay.
- [ ] `bittide-instances/src/Bittide/Instances/Hitl/DecodeDemo/UserCore.hs`:
      wire-demo clone — `UserCoreBusses = 3` (`DecodePeConfig`,
      `CreditLinkConfig`, `ProgrammableMux`), `RingBufferDepth = 512`, PE +
      link layer on `RXS2_RAW`/`HANDSHAKE_OUT`/`GTH_TX` via the mux, a mode
      register selecting A/B datapath.
- [ ] `.../DecodeDemo/TopEntity.hs` (`decodeDemoTest = demoTest ...`,
      `tests = mkTests 'decodeDemoTest Driver.driver`) and
      `.../DecodeDemo/MemoryMaps.hs`.
- [ ] Registrations: `bittide-instances.cabal` modules;
      `("DecodeDemoBoot" | "DecodeDemoManagementUnit" | "DecodeDemoClockControl", …)`
      in `Instances/MemoryMaps.hs`; `DecodeDemo.tests` in `Hitl/Tests.hs`
      (shake target appears automatically).
- Exit criteria: `shake decodeDemoTest:pnr` passes; memory-map JSONs emitted;
  PE + CreditLink unit tests green.

### Phase 2 — Firmware

- [ ] `firmware-binaries/demos/decode-demo-{boot,clock-control}`: clones of
      the wire-demo crates (memmap name in `build.rs` changes); all three
      crates added to `firmware-binaries/Cargo.toml` members.
- [ ] `impl_ring_buffer_interfaces!` for depth 512.
- [ ] `firmware-binaries/demos/decode-demo-management-unit`:
  - link bring-up + `links_stable` + `set_auto_center_enable(false)` +
    `elastic_buffer_delta` into `capture_ugns` + hardware-UGN print — verbatim
    from the wire-demo MU;
  - `clear_at_count` alignment-by-arithmetic (§3);
  - the C and A′ engines from Phase 0, factored into
    `firmware-support/bittide-sys/src/decode_demo.rs` so sim and rig share
    code; histogram binning; `computeCycles` emulation via
    `timer_wait_until_cycles`; DNA-pattern checksum verification.
- Exit criteria: firmware builds both profiles; Phase-0 sim tests still green
  against the shared engine code.

### Phase 3 — Host driver

`.../DecodeDemo/Driver.hs`, cloned from `WireDemo/Driver.hs` (probe start, USB
reset, serial, OpenOCD boot tap, load boot → wait, all taps, load CC+MU → wait
for `[MU] Printed all hardware UGNs`, interrupt, `readHardwareUgns`), then:

- [ ] ring schedule: `Bittide.Calculator.toCounterMap`/`toFpgaIndexed` to
      translate one global `first_cycle` into each node's counter base;
      empirical `internalDelay` constant for our datapath (see Risks);
- [ ] run A (write `DecodePeConfig` + mux arming on all 8 nodes at
      `currentTime + PeriodToCycles GthTx (Seconds 10)`, wire-demo `Timer`
      capture pattern); read PE histograms;
- [ ] run B (mode register → credit path, arm, run); read PE histograms;
- [ ] run C (write MU config, run); dump MU histograms via
      `Gdb.dumpMemoryRegion`; optionally run A′; dump;
- [ ] assert: zero checksum failures in all variants; A's histogram width ≤ a
      few cycles; B's width ≤ a small bound and mean shift within the
      predicted band; C completed `nTokens` with missed-deadline rate below
      threshold.

### Phase 4 — Post-processing & the figure

- [ ] Post-processor (register as `mPostProc` or a `samples-to`-style
      sub-command): histogram dumps → CSV; render the A/B/C overlay (log-y) +
      summary table (mean / p50 / p99 / max, tokens/s) per node and pooled;
      annotate the pre-registered predictions from §2 next to the measured
      values.
- [ ] Doc page `docs/sections/demos/decode-demo.md` (+ `docs/SUMMARY.md`
      entry): what each variant maps to (the §2 table), the scope-honesty
      paragraph, how the result connects to the latency-bound-decode argument.

### Phase 5 — CI

- [ ] `.github/synthesis/all.json`:
      `{"top": "decodeDemoTest", "stage": "test", "cc_report": true}`
      (nightly first; promote to `main.json` once flake-free).
- [ ] Threshold assertions in the driver so CI is red/green without a human
      reading histograms.

### Phase 6 — Stretch

- Contention for variant B: run two rings sharing links (or add a second
  traffic generator) so credits contend — the first step toward showing
  *congestion* smear on hardware async, the failure mode the quiet-fabric demo
  cannot otherwise exhibit.
- Full smoltcp baseline over ring buffers
  (`docs/sections/asynchronous-communication.md` proposal; copy the shape of
  `bittide-sys/src/smoltcp/axi.rs`) — a fourth histogram for the "full network
  stack" class.
- Reduce-scatter + all-gather; multi-link ring (2+ links/node).
- Compute-crossover sweep plot (`computeCycles` axis) — the hardware analogue
  of the scaling-book model's efficiency-vs-scale curve.
- Real int MAC kernel behind a flag (credibility run: actual data, actual
  arithmetic, same schedule).

## 5. Risks & open questions

1. **`internalDelay` analogue.** The wire demo needed a hand-tuned `-4` cycle
   constant between PE tap and MU tap. Our PE sits on the same taps, so the
   constant should be nearby, but it must be re-derived empirically: first
   2-node run with generous `phase_period`, scope the checksum failures, then
   tighten. This is the classic time sink — each adjustment costs a
   program-and-run rig cycle.
2. **PE schedule vs elastic-buffer tap semantics.** `RXS2_RAW` is
   pre-handshake; confirm the tap carries stable words at the scheduled cycle
   and that back-to-back vector streaming is clean (comma/alignment words
   interleaved?). Phase 0 sim + a 2-node rig smoke test cover this. The
   variant-B receiver additionally needs reliable header detection on the same
   tap.
3. **Variant-B reverse channel.** Credits travel on the reverse direction of
   the (bidirectional) ring link — confirm the demo core exposes both
   directions to the user core cleanly; if not, credits ride the node's second
   demo link, which changes the RTT bookkeeping but not the structure.
4. **Async wrap window (C only).** Depth 512 = 4.1 µs before a slot is
   overwritten; the C engine must treat a late read as *lost*, not just late
   (sequence numbers make this detectable). If Phase-0 measured copy cost eats
   the headroom, bump depth once — a type-level constant + one macro line.
5. **Baseline fairness.** Reviewers will probe this hardest. B is the
   structural answer for the hardware class (no CPU, best-effort async on
   identical silicon, documented protocol); A′ splits C's gap into protocol
   vs engine cost. Never present C as ICI-class — the §2 mapping table goes in
   every writeup.
6. **UART is slow (≈90 KB/s shared by 3 CPUs)** — summaries only; all bulk
   data over GDB dumps / memory-map reads (known-good `dumpCcSamples`
   pattern).
7. **Rig time.** Bring-up (8 bitstreams, 3 GDB loads × 8) dominates wall clock
   (~minutes), same as existing demos; the runs themselves are seconds. No new
   rig constraints.
8. **Open:** checksum-failure policy mid-token (halt and latch vs count and
   continue)? Default: count and continue, latch first-failure cycle for
   debugging. Same policy in all variants so integrity accounting is
   comparable.

## 6. Milestones

Ordered by dependency; M0 and M1's Clash design proceed in parallel. The rate
limiters are not implementation effort but the physical loop: synthesis/PnR
turnaround per iteration, and rig access for M2–M5 (every `internalDelay`
tightening or tap-semantics surprise costs a program-and-run cycle).

| # | Milestone | Depends on | Gate |
|---|---|---|---|
| M0 | Sim prototype green | — | all four disciplines produce correct sums in sim; MU service time and B handshake cost measured |
| M1 | Skeleton synthesizes | M0 (Clash design) | `shake decodeDemoTest:pnr` passes; memory-map JSONs emitted |
| M2 | 2-node rig run | M0, M1 | variants A and B, zero checksum failures, `internalDelay` pinned |
| M3 | Full 8-ring | M2 | A + B + C (+ A′) complete `nTokens`, histograms dumped |
| M4 | The figure | M3 | CSV + PDF overlay render; measured values annotated against §2's pre-registered predictions |
| M5 | CI | M3, M4 | nightly `all.json` run green with threshold assertions |

The Clash PE and credit link layer are a modest cost over a software-only
route and buy the two headline results: the scheduled histogram's width is set
by the fabric, not by a CPU (constant to the cycle, which no software baseline
could claim), and the A-vs-B mean gap is a clean, strawman-free measurement of
what deleting the handshake is worth on identical silicon.

## 7. Why this demo, restated

Every published bittide artifact so far measures the *mechanism* (clock
convergence, constant logical latency). This is the first measurement of the
*consequence*: what a workload gains when communication needs no runtime
coordination — quantified against both the best possible hardware-async
discipline and the software-transport discipline, on the same silicon, with
predictions registered in advance. The workload shape (serial token loop,
latency-bound collectives) is chosen so the result transfers directly to the
LLM-decode argument in `~/code/scaling-book/bittide-inference-model/` — one
figure, physical hardware, no simulation disclaimer.
