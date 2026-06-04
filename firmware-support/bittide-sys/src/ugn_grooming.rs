// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! In-band UGN grooming: compute a per-node relabel and per-link frame corrections that
//! restore a freshly-booted bittide system onto a target `λ^safe`.
//!
//! This is a `no_std`, fixed-capacity port of the host-side Haskell building blocks
//! `Bittide.Instances.Hitl.Utils.Relabel.computeRelabel` (the timing relabel) and
//! `Bittide.ClockControl.Ugn.Grooming` / `Bittide.Graph.Weighted` (the Bellman-Ford
//! feasibility gate). Keeping it faithful to that reference lets the host re-run the
//! Haskell version as an independent oracle.
//!
//! UGNs (`λ_{i→j}`) are signed per-link clock/frame offsets — edge weights on a directed
//! graph of nodes. Given the measured `λ` and a target `λ^safe`, grooming yields:
//!
//!   * a per-node relabel `φ` (the round-trip split, see [`compute_relabel`]) realised as
//!     each node's reset-release cycle — it removes the large, boot-time counter offsets;
//!   * per-link frame corrections (small) the receiving node inserts/removes in its elastic
//!     buffer to reach `λ^safe`.
//!
//! The relabel is the *round-trip split*, **not** the Bellman-Ford potential: for a hop
//! `i ↔ j`, the boot offset is `O_{i→j} = (λ_{i→j} − λ_{j→i}) / 2` and the one-way latency
//! is its exact complement `λ_{i→j} − O_{i→j}`. Bellman-Ford is used only to check that the
//! groomed system is physically allowed (no negative slack cycle). Using the shortest-path
//! potential for the timing would absorb link-latency asymmetry into the relabel, which
//! accumulates and breaks a directed transport chain (hence each node would otherwise drift).

use crate::net_state::UgnEdge;
use heapless::Vec;

/// Node identifier: an FPGA DNA, matching [`crate::net_state::UgnEdge`].
pub type Node = [u8; 12];

/// Maximum number of distinct nodes the solver supports.
pub const MAX_NODES: usize = 8;

/// Maximum number of edges the solver supports (matches [`crate::net_state::MAX_UGN_EDGES`]).
pub const MAX_EDGES: usize = crate::net_state::MAX_UGN_EDGES;

/// A feasible relabel: per-node reset offsets `φ` and per-receiving-link frame corrections.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RelabelPlan {
    /// Per-node offset `φ` to add to the shared base to get its reset-release cycle. Gauged
    /// so the root node's offset is `0`. The reset release removes each node's boot offset.
    pub reset_offsets: Vec<(Node, i64), MAX_NODES>,
    /// Per-receiving-link frame corrections `(dst_node, dst_port, frames)`: frames to insert
    /// (`> 0`) or remove (`< 0`) on the receiving link to reach `λ^safe` after relabeling.
    pub frames: Vec<(Node, u32, i64), MAX_EDGES>,
}

impl RelabelPlan {
    /// This node's reset offset `φ` (`0` if absent, e.g. an isolated node).
    pub fn reset_offset(&self, node: &Node) -> i64 {
        self.reset_offsets
            .iter()
            .find(|(n, _)| n == node)
            .map(|(_, o)| *o)
            .unwrap_or(0)
    }

    /// The frame correction for the link received on `node`'s local `port` (`0` if none).
    pub fn frame(&self, node: &Node, port: u32) -> i64 {
        self.frames
            .iter()
            .find(|(n, p, _)| n == node && *p == port)
            .map(|(_, _, f)| *f)
            .unwrap_or(0)
    }
}

/// Outcome of [`compute_relabel`].
// The `Feasible` variant holds fixed-capacity vectors (no alloc on this target, so boxing
// the large fields is not an option); the size difference is inherent and fine.
#[allow(clippy::large_enum_variant)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RelabelResult {
    /// The groomed UGNs changed too much (the slack graph has a negative cycle); the
    /// witnessed cycle nodes (forward edge order) are included for diagnostics.
    Infeasible(Vec<Node, MAX_NODES>),
    /// A feasible restore.
    Feasible(RelabelPlan),
}

/// Build a symmetric target `λ^safe` from this boot's measurement: for each hop the target
/// one-way latency on *both* directions is the measured average plus `margin`. The relabel
/// then carries the (asymmetric) boot offset and the frame corrections are the uniform
/// `margin`. A placeholder for a stored prior-boot reference; `margin >= 0` keeps the
/// corrections small frame insertions.
pub fn symmetric_target(measured: &[UgnEdge], margin: i64) -> Vec<UgnEdge, MAX_EDGES> {
    let mut out: Vec<UgnEdge, MAX_EDGES> = Vec::new();
    for e in measured {
        // One-way latency = round-trip / 2 = λ_{i→j} − O_{i→j}, symmetric across the hop.
        let lat = match offset_of(measured, &e.src_node, &e.dst_node) {
            Some(o) => e.ugn - o,
            None => e.ugn,
        };
        let _ = out.push(UgnEdge {
            ugn: lat + margin,
            ..*e
        });
    }
    out
}

/// Are these UGNs physically allowed? Equivalently: does the graph with `weights = λ` have
/// no negative cycle (the round-trip constraint)? Mirrors
/// `Bittide.ClockControl.Ugn.Grooming.isAllowed`.
pub fn is_allowed(edges: &[UgnEdge]) -> bool {
    negative_cycle_in(edges).is_none()
}

/// Groom the measured UGNs `λ` onto the target `λ^safe`, rooted at `root` (whose reset offset
/// is gauged to `0`). Edges are matched by `(src_node, dst_node)`; only hops present in both
/// snapshots are groomed.
///
/// The round-trip split always yields realizable corrections (for a symmetric target the
/// per-link frames are the uniform, non-negative margin), so feasibility here is simply
/// whether the *groomed* graph `λ^safe` is physically allowed — no negative cycle in its own
/// weights. (This is the right gate for the round-trip-split relabel; the Bellman-Ford
/// *slack* potential of `Grooming.groomCorrection` is a stricter, different condition that
/// would reject valid grooming on a densely-connected graph.) Returns
/// [`RelabelResult::Infeasible`] with the witnessing cycle when the target is not allowed.
pub fn compute_relabel(measured: &[UgnEdge], target: &[UgnEdge], root: Node) -> RelabelResult {
    if let Some(cycle) = negative_cycle_in(target) {
        return RelabelResult::Infeasible(cycle);
    }

    // Offset potential φ: BFS spanning tree over the measured graph rooted at `root`,
    // φ[root] = 0, φ[j] = φ[i] + O_{i→j} along tree edges.
    let mut reset_offsets: Vec<(Node, i64), MAX_NODES> = Vec::new();
    let _ = reset_offsets.push((root, 0));
    let mut queue: Vec<Node, MAX_NODES> = Vec::new();
    let _ = queue.push(root);
    let mut head = 0;
    while head < queue.len() {
        let u = queue[head];
        head += 1;
        let phi_u = lookup(&reset_offsets, &u).unwrap_or(0);
        for e in measured {
            if e.src_node != u {
                continue;
            }
            let j = e.dst_node;
            if lookup(&reset_offsets, &j).is_some() {
                continue;
            }
            if let Some(o) = offset_of(measured, &u, &j) {
                let _ = reset_offsets.push((j, phi_u + o));
                let _ = queue.push(j);
            }
        }
    }

    // Per-receiving-link frame correction: λ^safe_{i→j} − (λ_{i→j} − O_{i→j}).
    let mut frames: Vec<(Node, u32, i64), MAX_EDGES> = Vec::new();
    for e in measured {
        let Some(o) = offset_of(measured, &e.src_node, &e.dst_node) else {
            continue;
        };
        let Some(tgt) = ugn_of(target, &e.src_node, &e.dst_node) else {
            continue;
        };
        let _ = frames.push((e.dst_node, e.dst_port, tgt - (e.ugn - o)));
    }

    RelabelResult::Feasible(RelabelPlan {
        reset_offsets,
        frames,
    })
}

// ---------------------------------------------------------------------------
// Internals
// ---------------------------------------------------------------------------

fn lookup(map: &[(Node, i64)], key: &Node) -> Option<i64> {
    map.iter().find(|(n, _)| n == key).map(|(_, v)| *v)
}

/// The measured/target UGN of the directed edge `src → dst`, if present.
fn ugn_of(edges: &[UgnEdge], src: &Node, dst: &Node) -> Option<i64> {
    edges
        .iter()
        .find(|e| &e.src_node == src && &e.dst_node == dst)
        .map(|e| e.ugn)
}

/// Boot-offset half of a hop's round trip: `O_{i→j} = (λ_{i→j} − λ_{j→i}) / 2` (floor
/// division, matching Haskell `div`), or `None` if either direction is missing.
fn offset_of(measured: &[UgnEdge], src: &Node, dst: &Node) -> Option<i64> {
    let fwd = ugn_of(measured, src, dst)?;
    let rev = ugn_of(measured, dst, src)?;
    Some(div_floor(fwd - rev, 2))
}

/// Floor division (rounds toward negative infinity), matching Haskell's `div`.
fn div_floor(a: i64, b: i64) -> i64 {
    let q = a / b;
    let r = a % b;
    if r != 0 && (r < 0) != (b < 0) {
        q - 1
    } else {
        q
    }
}

/// The distinct nodes appearing as a source or destination of any edge, first-seen order.
fn node_set(edges: &[UgnEdge]) -> Vec<Node, MAX_NODES> {
    let mut nodes: Vec<Node, MAX_NODES> = Vec::new();
    for e in edges {
        if !nodes.contains(&e.src_node) {
            let _ = nodes.push(e.src_node);
        }
        if !nodes.contains(&e.dst_node) {
            let _ = nodes.push(e.dst_node);
        }
    }
    nodes
}

/// Bellman-Ford negative-cycle detection on a graph whose edge weights are the edges' own
/// UGNs, from a virtual super-source (all distances start at `0`). Returns the nodes of a
/// negative cycle if one exists, otherwise `None`.
fn negative_cycle_in(graph: &[UgnEdge]) -> Option<Vec<Node, MAX_NODES>> {
    let nodes = node_set(graph);
    let n = nodes.len();
    let index = |node: &Node| nodes.iter().position(|x| x == node);

    let mut edges: Vec<(usize, usize, i64), MAX_EDGES> = Vec::new();
    for e in graph {
        if let (Some(i), Some(j)) = (index(&e.src_node), index(&e.dst_node)) {
            let _ = edges.push((i, j, e.ugn));
        }
    }

    let mut dist: Vec<i64, MAX_NODES> = Vec::new();
    let mut pred: Vec<Option<usize>, MAX_NODES> = Vec::new();
    for _ in 0..n {
        let _ = dist.push(0);
        let _ = pred.push(None);
    }

    for _ in 1..n {
        for &(i, j, w) in &edges {
            if dist[i] + w < dist[j] {
                dist[j] = dist[i] + w;
                pred[j] = Some(i);
            }
        }
    }
    for &(i, j, w) in &edges {
        if dist[i] + w < dist[j] {
            return Some(extract_cycle(&nodes, &pred, n, j));
        }
    }
    None
}

/// Walk the predecessor chain `n` steps onto a negative cycle, then follow it until it
/// repeats; returns the cycle nodes in forward order.
fn extract_cycle(
    nodes: &[Node],
    pred: &[Option<usize>],
    n: usize,
    j: usize,
) -> Vec<Node, MAX_NODES> {
    let step = |v: usize| pred[v].unwrap_or(v);
    let mut start = j;
    for _ in 0..n {
        start = step(start);
    }
    let mut idxs: Vec<usize, MAX_NODES> = Vec::new();
    let _ = idxs.push(start);
    let mut v = step(start);
    let mut guard = 0;
    while v != start && guard < n {
        let _ = idxs.push(v);
        v = step(v);
        guard += 1;
    }
    let mut out: Vec<Node, MAX_NODES> = Vec::new();
    for &idx in idxs.iter().rev() {
        let _ = out.push(nodes[idx]);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn node(id: u8) -> Node {
        let mut n = [0u8; 12];
        n[0] = id;
        n
    }

    fn edge(s: u8, sp: u32, d: u8, dp: u32, ugn: i64) -> UgnEdge {
        UgnEdge {
            src_node: node(s),
            src_port: sp,
            dst_node: node(d),
            dst_port: dp,
            ugn,
        }
    }

    /// A 3-node complete graph with asymmetric (boot-offset-bearing) UGNs. Each ordered pair
    /// has a link; `dst_port` is the receiver's local port index. The UGNs are physically
    /// consistent: with boot offsets δ = [0, 10, 25] and symmetric latencies L01=40, L02=70,
    /// L12=42, `λ_{i→j} = L_{ij} + (δ_j − δ_i)`, so the round-trip offsets form a valid
    /// potential (`O_{0→1}=10, O_{0→2}=25, O_{1→2}=15`).
    fn measured_3() -> [UgnEdge; 6] {
        [
            edge(0, 0, 1, 0, 50), // 0->1  (O_{0->1} = (50-30)/2 = 10)
            edge(1, 1, 0, 0, 30), // 1->0
            edge(0, 1, 2, 0, 95), // 0->2  (O_{0->2} = (95-45)/2 = 25)
            edge(2, 1, 0, 1, 45), // 2->0
            edge(1, 2, 2, 1, 57), // 1->2  (O_{1->2} = (57-27)/2 = 15)
            edge(2, 2, 1, 2, 27), // 2->1
        ]
    }

    #[test]
    fn relabel_offsets_are_distinct_and_chained() {
        let measured = measured_3();
        let target = symmetric_target(&measured, 100);
        let RelabelResult::Feasible(plan) = compute_relabel(&measured, &target, node(0)) else {
            panic!("expected feasible");
        };
        // Rooted at node 0: φ[0]=0, φ[1]=O_{0->1}=10, φ[2]=O_{0->2}=25.
        assert_eq!(plan.reset_offset(&node(0)), 0);
        assert_eq!(plan.reset_offset(&node(1)), 10);
        assert_eq!(plan.reset_offset(&node(2)), 25);
        // The offsets are all distinct — the bug we are fixing (they used to collapse to 0).
        let offs = [
            plan.reset_offset(&node(0)),
            plan.reset_offset(&node(1)),
            plan.reset_offset(&node(2)),
        ];
        assert!(offs[0] != offs[1] && offs[1] != offs[2] && offs[0] != offs[2]);
    }

    #[test]
    fn symmetric_target_gives_uniform_margin_frames() {
        let measured = measured_3();
        let margin = 100;
        let target = symmetric_target(&measured, margin);
        let RelabelResult::Feasible(plan) = compute_relabel(&measured, &target, node(0)) else {
            panic!("expected feasible");
        };
        // frame = λ^safe − (λ − O) = (lat + margin) − lat = margin, on every receiving link.
        for e in measured.iter() {
            assert_eq!(plan.frame(&e.dst_node, e.dst_port), margin);
        }
    }

    #[test]
    fn reconstructs_target() {
        // In the relabeled (application) domain the reset removes the offset, so
        // `λ_{i→j} − (φ_j − φ_i) + frames_{i→j} == λ^safe_{i→j}` for every groomed hop.
        let measured = measured_3();
        let target = symmetric_target(&measured, 100);
        let RelabelResult::Feasible(plan) = compute_relabel(&measured, &target, node(0)) else {
            panic!("expected feasible");
        };
        for e in measured.iter() {
            let phi_i = plan.reset_offset(&e.src_node);
            let phi_j = plan.reset_offset(&e.dst_node);
            let f = plan.frame(&e.dst_node, e.dst_port);
            let tgt = ugn_of(&target, &e.src_node, &e.dst_node).unwrap();
            assert_eq!(e.ugn - (phi_j - phi_i) + f, tgt, "reconstruction failed");
        }
    }

    #[test]
    fn infeasible_negative_cycle() {
        // target far below measured on a 2-cycle => negative slack cycle.
        let measured = [edge(0, 0, 1, 0, 0), edge(1, 0, 0, 0, 0)];
        let target = [edge(0, 0, 1, 0, -10), edge(1, 0, 0, 0, 0)];
        assert!(matches!(
            compute_relabel(&measured, &target, node(0)),
            RelabelResult::Infeasible(_)
        ));
    }
}
