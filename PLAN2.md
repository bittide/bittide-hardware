<!--
SPDX-FileCopyrightText: 2026 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# PLAN2: `decodeDemo` under contention — does the calendar hold where credits queue?

## 0. Context for the agent picking this up

Read first, in this order:

1. `PLAN.md` (repo root) — design rationale and results of the base decode
   demo, including the pre-registered predictions and what they taught us.
2. `docs/sections/demos/decode-demo.md` — the implemented demo, its
   architecture, and the measured results (2026-08-20).
3. `~/code/scaling-book/bittide-inference-model/REPORT.md` — "The Determinism
   Dividend", the full argument this experiment serves. §3 has the measured
   results; §4's sensitivity line and §5's "frontier claim" are what this
   experiment is designed to substantiate or kill.

State of play, in three sentences: the base demo measured that on a **quiet**
fabric, a cut-through credit link (variant B) matches the statically scheduled
calendar (variant A) — both constant to the cycle — while store-and-forward
(B_sf) costs 2.60× and CPU-driven coordination ~10³. The concession this
forced into the pitch: async's parity and zero variance are properties of the
*quiet* rig. The entire remaining performance claim ("sustained at load")
rests on one unmeasured prediction: **under contention, credit-based flow
control queues and smears, while a calendar interleaves competing flows with
zero interference.** This experiment measures exactly that.

Existing code this builds on (all in this repo, all working in nightly CI as
`decodeDemoTest`):

- `bittide/src/Bittide/DecodeProcessingElement.hs` — `decodeReduceCore`, the
  streaming two-lap ring all-reduce PE with calendar arming
  (`first_cycle`/`phase_period`/`token_period`/…) and a 16-bin cycle-wide
  latency histogram.
- `bittide/src/Bittide/CreditLink.hs` — the credit/valid-ack link layer with a
  `cut_through` register (True: cut-through relays; False: store-and-forward)
  and full flow-control accounting (credits consumed/returned/granted, frames,
  errors, drops, timeouts, min/max credit RTT).
- `bittide/src/Bittide/ProgrammableMux.hs` — the one-shot mux giving the PE
  the links after MU-driven bring-up.
- `bittide-instances/src/Bittide/Instances/Hitl/DecodeDemo/{UserCore,
  TopEntity, MemoryMaps, Driver, PostProc}.hs` — the demo instance, driver
  (UGN read → `Bittide.Calculator` schedule → run C → A′ → A → B → B_sf →
  dump), and CSV/summary post-processing.
- `firmware-binaries/demos/decode-demo-{boot,management-unit,clock-control}`.

## 1. The claim under test (pre-registered)

On the same bitstream, add a second, competing traffic flow on the same
physical ring links, and run the decode loop against it in both disciplines:

- **A_c (scheduled + contention)**: the calendar time-multiplexes decode and
  the competing flow into disjoint slots. Prediction: decode token latency
  **shifts by a deterministic, precomputable amount** (the reserved slots) and
  its spread **stays exactly 0**. The competing flow's throughput is also
  exact — the calendar is an admission contract for both flows.
- **B_c (credit cut-through + contention)**: decode and the competing flow
  contend for TX ports through an arbiter. Prediction: decode latency
  **shifts AND smears** — when a decode frame arrives at a busy port, the
  relay must buffer it (opportunistic cut-through degrades toward
  store-and-forward), and credit RTTs inflate and vary with the competing
  flow's phase. Spread should grow with offered load; expect multi-modality
  at duty cycles where burst alignment beats against the token period.
- Sweep offered load (duty cycle of the competing flow): {0, 25, 50, 75}%.
  At 0% both must reproduce the base demo's numbers (regression gate).
- Also record the **competing flow's** achieved throughput and latency in
  both disciplines: the calendar guarantees it exactly; the arbiter does not.
  (The two-sided guarantee is the product story: not "decode wins", but
  "both flows get contracts".)

Deviations from these predictions are findings, not failures — report them.
In particular: if B_c's smear is negligible even at 75% duty, that is a major
(negative) result for the pitch and must be reported with the same prominence.
It would mean well-designed hardware arbitration also holds under load, and
the performance thesis narrows further to buffering + software-path +
operations. Do not soften it.

## 2. Design

### The competing flow: a traffic-generator PE

Add a small traffic generator (TG) per node, sharing the decode ring's links:

- TG emits fixed-size bursts (`tg_burst_words`, default 64 — same shape as a
  decode frame) at a programmable duty cycle (`tg_duty_percent`), addressed to
  the ring neighbor (one hop; it exists to occupy links, not to compute).
  Payload: a counter pattern; the receiving TG checks it and counts errors —
  the competing flow is verified too, so "TG got starved/corrupted" is
  observable, not silent.
- **Scheduled mode**: TG bursts occupy calendar slots disjoint from decode
  slots. The driver computes both schedules together from the UGNs — decode's
  `phase_period` stretches by exactly the reserved TG slots. The predicted A_c
  latency is therefore *computable in the driver before the run*; assert
  measured == predicted (this is the "admission contract" demonstrated
  literally).
- **Credit mode**: TG and decode frames meet at a per-link TX arbiter
  (round-robin between the two flows; work-conserving; cut-through preserved
  when the port is idle — be *fair* to async, the result is worthless
  otherwise). Both flows run the existing `CreditLink` protocol with separate
  credit accounting.

### What to reuse vs. what is new

- New Clash: the TG core (a stripped `decodeReduceCore` — emit/check pattern,
  no reduce, own histogram) and the two-flow TX arbiter in front of
  `CreditLink`'s sender path. Calendar-slot interleave for scheduled mode is
  config, not new hardware: `decodeReduceCore`'s `phase_period` already
  expresses it if the driver spaces the phases; TG gets its own
  `first_cycle`/period registers.
- New config device: `TrafficGenConfig` (duty, burst words, mode,
  status/error counters, achieved-throughput counter, latency histogram).
  One more user bus in `UserCore.hs` (`UserCoreBusses` +1); register it in
  `MemoryMaps.hs` as for the existing devices.
- Driver: extend `DecodeDemo/Driver.hs` with the contention phases after the
  existing runs (keep the base runs untouched — they are the 0% regression
  gate and CI history). Sweep duty ∈ {25, 50, 75} × {A_c, B_c}. Keep
  `nTokens` = 10,000 for hardware variants.
- PostProc: extend to emit per-duty histograms for both flows; the headline
  figure is decode-latency histograms A_c vs B_c across duty cycles (spike
  that shifts predictably vs. distribution that shifts and spreads — if the
  prediction holds).
- Firmware: unchanged except bring-up of the new device (clear/arm registers).
- Keep everything in the existing top entity and HITL test (`decodeDemoTest`)
  so CI stays one bitstream; the driver just runs more phases. If PnR timing
  or utilization pushes back, split into `decodeContentionTest` with its own
  `.github/synthesis/all.json` entry — but try single-bitstream first.

### Measurement plan

Per (discipline, duty) cell, per node: decode token-latency histogram +
min/max; TG achieved throughput, latency histogram, error count; CreditLink
accounting (RTT min/max is the smoking gun for B_c — expect the max to grow
with duty while A_c's stays flat); zero checksum failures in both flows or
the cell is invalid. Dump via the existing `.data` mechanism
(NOT `.bin` — see the note in `Driver.hs`/git history: CI's cc-report step
globs `.bin`).

## 3. Work breakdown

- [x] **Phase 0 — sim.** Extend the existing decode-demo sim test
      (`bittide-instances` Tests + `firmware-binaries/sim-tests`) with the TG
      + arbiter; verify: 0% duty reproduces base numbers; scheduled interleave
      is collision-free by construction (assert no two flows own a port in
      the same cycle in sim); credit mode arbitration is fair and lossless.
      Measure the arbiter's idle-path cost — it must be ~0 when TG is off, or
      the 0% regression gate will catch it.
- [x] **Phase 1 — Clash.** TG core + arbiter + `TrafficGenConfig`; unit
      tests; `UserCore.hs`/`MemoryMaps.hs`/cabal registrations;
      `shake decodeDemoTest:pnr` green.
- [x] **Phase 2 — driver + firmware.** Joint schedule computation (decode +
      TG slots) with the measured-vs-predicted A_c assertion; duty sweep;
      firmware device bring-up; dumps.
- [x] **Phase 3 — rig + report.** Full run; PostProc figures; update
      `docs/sections/demos/decode-demo.md` (new "Contention" section with the
      same scope-honesty discipline), `PLAN.md` §measured, REPORT.md §3/§5 and
      the artifact (Martijn has the link) — including, prominently, whichever
      way the result went.
- [x] **Phase 4 — CI.** (the nightly decodeDemoTest driver runs the sweep) Contention phases in the nightly run with threshold
      assertions (A_c: spread == 0 and measured == predicted; B_c: thresholds
      set from the measured run, not invented).

Gates, not effort estimates (the rate limiter is PnR turnaround and rig
access): sim green with 0%-regression → PnR green → 2-node rig smoke at 50%
duty → full sweep → docs/report/CI.

### Measured (rig, 2026-08-22; injector token latency over 10,000 tokens/cell)

Quiet floors that boot: A 10,448; B 10,223 cycles. Generator: 64-word
verified bursts to the ring neighbor at 19/48/68% link duty.

- **A_c: 10,448, spread 0, at every duty — bit-identical to quiet A.** The
  driver placed the generator's slots in the decode windows' per-link idle
  gaps, so the precomputable shift is zero; the hardware collision counter
  stayed 0 and the generator itself ran 340k/851k/1.19M bursts with zero
  errors and zero queueing. Both flows got exact contracts.
- **B_c: 10,421..10,487 at every duty** — a shift of +198..264 over the
  quiet floor and a 66-cycle spread (one burst: the work-conserving
  round-robin arbiter's per-hop worst case). Lossless at all duties; the
  generator's queueing grows with duty (max 4 → 54 → 58 cycles) and the
  decode credit RTT inflates 131 → up to 199.
- Honest prominence for the async side: a fair single-hop arbiter HOLDS at
  ~87% total link load — bounded, lossless, sub-1% interference. The
  measured contrast is exactness and two-sided guarantees, not collapse.
  The per-hop wait bound scales with burst length, so the gap grows with
  transfer size and hop count; on this rig it is small.
- Instructive failures along the way, all caught by the verified-traffic
  counters: a driver call halted the clock-control CPUs mid-test (elastic
  buffers slip words once oscillators free-run — worth knowing in itself);
  the registered transmit path needed an explicit one-cycle port drain
  between owners; the generator's credit-grant train blocked header
  reception (~3 lost bursts per million, fixed).

## 4. Risks & honesty

1. **Arbiter fairness is the credibility of the whole result.** A strawman
   arbiter (decode-priority or non-work-conserving) makes B_c look bad and
   the measurement worthless. Round-robin, work-conserving, cut-through when
   idle, documented in the demo docs. If anything, err toward favoring the
   async design.
2. **The TG must be verified traffic.** Unverified filler invites "your
   contention was fake" — pattern-checked, error-counted, throughput-counted.
3. **One-hop TG traffic is mild contention.** It contends for links but not
   for multi-hop paths. If B_c barely smears, escalate honestly: multi-hop TG
   routes (crossing flows) before concluding the negative result — then
   conclude it.
4. **Single-credit CreditLink may serialize B_c artificially.** The base link
   runs one credit outstanding; under contention that may understate async
   (real fabrics run many credits). If the smear looks like credit
   starvation rather than port contention, add a credits knob and report both.
   This is the async-fairness analogue of risk 1.
5. **Scope honesty carries over verbatim** from the base demo: point-to-point
   synchronized links, one workload pair, no multi-tenant OS noise. This
   measures *arbitration under offered load*, the first rung of "sustained",
   not a production datacenter.
6. **What the TG flow does and does not model — write this into the demo
   docs.** A dedicated NVLink/ICI domain running one collective at a time has
   little internal contention (collective algorithms are link-disjoint); that
   quiet case is the base demo, already measured at parity. The TG flow
   models the *shared-domain* case modern serving stacks are moving into:
   prefill/decode-disaggregated KV-cache transfers riding the same links as
   decode collectives, EP all-to-alls overlapping TP all-reduces, DP gradient
   traffic overlapping TP in training. It does NOT model scale-out spine
   congestion (ECMP collisions, incast) — an 8-node point-to-point ring has
   no spine; never claim otherwise. Note also that the scaling book is
   silent on fabric contention entirely (its only interference mention is
   prefill-on-decode compute jitter, `inference.md:442`), so this experiment
   cannot cite the book for motivation — cite the serving-stack trend
   (disaggregation, comm-overlap engineering) instead. The realistic product
   framing of the result is economic: arbitration forces operators to choose
   isolation (wasted capacity) or interference (jitter); a calendar provides
   guaranteed latency at high utilization. Measure and report it as that
   choice, not as "async fabrics are congested".

## 5. What this decides

The sensitivity line in REPORT.md §4 has two measured anchors (0 ns quiet;
~570 ns store-and-forward) and one external band (book-measured ICI). This
experiment adds the missing datum: **where a loaded fabric sits on that line
on our own silicon** — and whether the calendar's spread stays at zero while
it happens. If A_c holds (spread 0, measured == predicted) and B_c shifts and
smears with duty, the "sustained at load" claim in the frontier argument
(REPORT.md §5 / artifact §5) gets its first measured leg. If B_c holds too,
the performance pitch narrows to buffering + software-path + operations, and
the report must say so in the same breath — that outcome is cheaper to learn
here than in a partner meeting.
