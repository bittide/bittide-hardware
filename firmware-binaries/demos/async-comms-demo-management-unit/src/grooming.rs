// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! In-band UGN-grooming orchestration for the management unit.
//!
//! The pure algorithm lives in [`bittide_sys::ugn_grooming`]; this module wires it to the
//! demo: it grooms the collected [`UgnReport`], turns the resulting [`RelabelPlan`] into the
//! per-node [`NodeCorrectionWire`] the manager distributes over TCP, and applies a correction
//! to the local hardware (elastic buffers + the relabel reset).

use bittide_hal::shared_devices::{ElasticBuffer, TimedReset};
use bittide_macros::{signed, unsigned};
use bittide_sys::net_state::{UgnEdge, UgnReport};
use bittide_sys::smoltcp::link_protocol::{NodeCorrectionWire, MAX_LINKS};
use bittide_sys::ugn_grooming::{
    compute_relabel, symmetric_target, Node, RelabelPlan, RelabelResult, MAX_EDGES,
};
use heapless::Vec;

/// Safety margin (epsilon) added to each symmetric one-way latency to form the target
/// `λ^safe`. A placeholder for a stored prior-boot reference; `>= 0` keeps the per-link
/// frame corrections small insertions.
pub const GROOMING_MARGIN: i64 = 100;

/// Groom the collected report onto a symmetric target, rooted at `root` (whose reset offset
/// is gauged to `0` — pass the manager's own DNA so its release lands on the shared base).
pub fn groom_report(report: &UgnReport, root: Node) -> RelabelResult {
    let mut measured: Vec<UgnEdge, MAX_EDGES> = Vec::new();
    for edge in report.edges.iter().flatten() {
        let _ = measured.push(*edge);
    }
    let target = symmetric_target(&measured, GROOMING_MARGIN);
    compute_relabel(&measured, &target, root)
}

/// Build the wire correction for `node`: its relabel reset-release cycle
/// (`shared_base + φ_node`) and the per-local-port frame corrections.
pub fn node_correction(plan: &RelabelPlan, node: &Node, shared_base: u64) -> NodeCorrectionWire {
    let release_cycle = (shared_base as i64 + plan.reset_offset(node)) as u64;
    let mut frames = [0i32; MAX_LINKS];
    for (port, frame) in frames.iter_mut().enumerate() {
        *frame = plan.frame(node, port as u32) as i32;
    }
    NodeCorrectionWire::new(release_cycle, frames)
}

/// Apply a node's correction to local hardware: insert the per-port elastic-buffer frame
/// adjustments, then set the relabel reset-release cycle last (so the node is fully
/// configured before its application reset can release).
pub fn apply(
    elastic_buffers: &[&ElasticBuffer; MAX_LINKS],
    timed_reset: &TimedReset,
    correction: &NodeCorrectionWire,
) {
    for (port, eb) in elastic_buffers.iter().enumerate() {
        let frames = correction.frame(port);
        if frames != 0 {
            eb.set_adjustment(signed!(frames, n = 32));
        }
    }
    timed_reset.set_release_cycle(unsigned!(correction.release_cycle(), n = 64));
}
