<!--
SPDX-FileCopyrightText: 2026 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Restoring a known state (UGN grooming)

A bittide application that wants a *fixed* schedule — the same per-link timing on
every run — needs the network to look the same on every boot. It does not. This
chapter explains how we restore a freshly-booted system to a stored, known-good
state so that a single precomputed schedule keeps working. We call this procedure
**UGN grooming**.

## The problem

Every link carries a [UGN](architecture.md): a signed clock/frame offset
`λ(i→j) = localCounter − remoteCounter`, measured between an ordered pair of nodes
`i → j`. The UGNs describe the network's latency profile, and the application's
schedule is derived from them.

Two facts make a naive "measure once, schedule forever" approach fail:

1. **Boot-time offsets.** Each node's local counter starts from an arbitrary value
   that depends on when its reset was released. Those offsets leak into the
   measured UGNs, so the raw UGNs differ — sometimes by hundreds of thousands of
   cycles — from one boot to the next, even though the physical network is
   unchanged.
2. **The round-trip constraint.** UGNs cannot be chosen independently. The sum of
   the UGNs around any directed cycle is the round-trip latency of that cycle and
   must be non-negative (you cannot receive a frame before it was sent). A set of
   UGNs is *physically realisable* exactly when its graph has **no negative
   cycle**.

So we cannot just overwrite the new UGNs with the old ones; we must transform this
boot's measurements back onto the stored reference *while respecting the round-trip
constraints*.

## UGNs as a weighted graph

Model the network as a directed graph: nodes are FPGAs (keyed by their DNA-derived
id) and every link `i → j` is an edge weighted by its UGN `λ(i→j)`. This is the
`Bittide.Graph.Weighted` library (`bittide/src/Bittide/Graph/Weighted.hs`),
which provides Bellman-Ford shortest paths, node potentials, and negative-cycle
detection. The grooming math is then a small layer on top of it, in
`Bittide.ClockControl.Ugn.Grooming` (`bittide/src/Bittide/ClockControl/Ugn/Grooming.hs`).

We keep one stored reference profile, `λsafe`. It is a previously-captured boot's
UGNs plus a small safety margin `ε`:

```
λsafe(i→j) = λobs(i→j) + ε
```

The margin is the headroom the elastic buffers are allowed to insert on top of
matching the reference latency; it absorbs the small boot-to-boot physical drift
while staying well within the buffer's safe occupancy range.

## The grooming algorithm

Given this boot's measured UGNs `λ` and the stored `λsafe`, we want two things:

* a **relabeling** `q` — one integer per node — that shifts each node's time base,
  and
* a small number of **frames to insert** per link,

such that after applying both, every link's UGN equals `λsafe(i→j)` again. A
relabeling by `q` changes a link's UGN to `λ(i→j) + q(j) − q(i)`: it is a pure
gauge change (it cancels around every cycle), which is exactly why it can undo
the per-node boot offsets without touching the physical round-trip latencies.

We want the relabeled UGN to fit *under* the target, leaving a non-negative
remainder to pad with inserted frames:

```
λ(i→j) + q(j) − q(i)  ≤  λsafe(i→j)        for every link i → j
```

Rearranging gives a **system of difference constraints** on the unknown potentials
`q`, with the per-link *slack* as the bound:

```
slack:       c(i→j) = λsafe(i→j) − λ(i→j)
constraint:  q(j) − q(i) ≤ c(i→j)
```

This is a classic shortest-path problem. Build the **slack graph** (the same edges,
reweighted by `c`), add a virtual super-source with a zero-weight edge to every
node, and run **Bellman-Ford**. Two outcomes:

* **Negative cycle in the slack graph → infeasible.** The UGNs changed too much to
  be restored to `λsafe`; grooming reports the witnessing cycle for diagnostics and
  gives up.
* **Otherwise, the shortest-path distances are the potentials `q`.** By the
  shortest-path relaxation invariant they satisfy every constraint, so the leftover
  padding is guaranteed non-negative:

```
frames(i→j) = λsafe(i→j) − (λ(i→j) + q(j) − q(i))   ≥ 0
```

`q` and `frames` together are the restore plan: relabel by `q`, then insert
`frames(i→j)` frames on each receiving link, and every UGN is back at `λsafe`.

## Applying the plan to hardware

The host computes the plan and pushes it to each node over GDB. The plan has two
halves, applied through two dedicated Wishbone peripherals.

### Relabeling — `TimedReset`

A relabeling `q` is realised by *when* each node's application leaves reset. The
`TimedReset` (`bittide/src/Bittide/TimedReset.hs`) peripheral holds the
application reset asserted until the node's local counter passes a software-chosen
`release_cycle`, then releases it (and, because the counter is monotonic, keeps it
released). The application counter is therefore `0` at that release point, which
relabels the node's time base. The host writes, gauged so node `0` is the origin:

```
release_cycle(i) = base − (q(i) − q(0))
```

### Frame corrections — `UgnCorrections` + MU firmware

The per-link `frames` are written to the `UgnCorrections` peripheral
(`bittide/src/Bittide/ClockControl/Ugn/Corrections.hs`) — a corrections vector plus
a `valid` flag. The management-unit firmware `wire-demo-management-unit`
(`firmware-binaries/demos/wire-demo-management-unit/src/main.rs`)
polls `valid`, then applies each link's correction to its elastic buffer as a
**single atomic write**. A frame-at-a-time loop is vulnerable to concurrent clock-control
(Callisto) adjustments between iterations, which shrink the net change; one atomic
write bounds that race to at most one frame.

The host orchestrates the ordering: corrections are applied while the application
is still held in reset, then the relabel-release cycles are written, so the
application starts in the fully-restored state.

## Storing `λsafe` in a canonical gauge

`λsafe` is stored in a **minimal non-negative gauge** (`canonicalizeUgn` in
`Bittide.Instances.Hitl.Utils.UgnGrooming`,
`bittide-instances/src/Bittide/Instances/Hitl/Utils/UgnGrooming.hs`).
Using the graph's potentials, every UGN is rewritten to its Bellman-Ford *reduced
cost* `λ(i→j) + q(i) − q(j)`, which is non-negative and leaves every cycle sum
unchanged. This is again a pure gauge change — the *same* physical system — but in
a gauge where the per-link UGNs are small and non-negative.

Why bother: the application's fixed schedule is derived from these UGNs, and the
programmable mux / elastic buffers only work over a bounded range. Grooming each
boot onto a canonical `λsafe` means the relabel absorbs that boot's large counter
offsets and the groomed app-frame UGNs land back on the same small, non-negative
values every run — which is precisely what makes one precomputed schedule reusable.
Note that canonicalization honours the true *asymmetric* round-trip constraints; a
naive symmetric midpoint is only a valid relabeling on an acyclic graph.

## Verifying it landed

To confirm on hardware that the relabel took effect, the wire-demo user core
exposes its application counter through an `app_counter` readback register. With
the application running, the host checks per node that

```
localCounter − app_counter == release_cycle
```

A divergence pinpoints a node whose reset did not release at the intended
(relabeled) cycle.

## Components involved

| Component | Location | Role |
|-----------|----------|------|
| Weighted graph library | `Bittide.Graph.Weighted` (`bittide/src/Bittide/Graph/Weighted.hs`) | Bellman-Ford, potentials, negative-cycle detection |
| Grooming algorithm | `Bittide.ClockControl.Ugn.Grooming` (`bittide/src/Bittide/ClockControl/Ugn/Grooming.hs`) | `λ` + `λsafe` → relabel `q` + frames, or infeasible |
| HITL grooming layer | `Utils.UgnGrooming` (`bittide-instances/src/Bittide/Instances/Hitl/Utils/UgnGrooming.hs`) | adapts the algorithm to `UgnEdge`s, canonical gauge, reset offsets |
| `TimedReset` peripheral | `Bittide.TimedReset` (`bittide/src/Bittide/TimedReset.hs`) | releases the app reset at the chosen cycle (the relabel) |
| `UgnCorrections` peripheral | `Bittide.ClockControl.Ugn.Corrections` (`bittide/src/Bittide/ClockControl/Ugn/Corrections.hs`) | host → MU channel for the per-link frame corrections |
| MU firmware | `wire-demo-management-unit` (`firmware-binaries/demos/wire-demo-management-unit/src/main.rs`) | applies the corrections to the elastic buffers |

This procedure is exercised end-to-end by the [Wire Demo](demos/wire-demo.md).
