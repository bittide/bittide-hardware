-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- |
Reusable building blocks for grooming a freshly-booted bittide system onto a stored
reference and applying the result to a demo. A demo wires these together (see
"Bittide.Instances.Hitl.WireDemo.Driver" for a worked example):

  1. Read the per-node hardware UGN counter captures.
  2. 'hardwareUgnEdges' turns them into 'UgnEdge's keyed by DNA node id.
  3. 'computeRelabel' grooms the measured UGNs onto a target @λ^safe@, yielding a
     per-node relabel and per-link frame corrections. It checks feasibility with
     Bellman-Ford ("Bittide.Instances.Hitl.Utils.UgnGrooming" / "Bittide.Graph.Weighted")
     but computes the timing relabel itself via the round-trip split (see there for why).
  4. The relabel becomes each node's reset-release cycle ('TimedReset') and the frame
     corrections are applied by the management unit (the @UgnCorrections@ peripheral),
     written over GDB with 'writeReleaseCycle' / 'writeCorrections'.

The relabel @q@ removes the (large, boot-time) counter offsets via the reset release;
the (small, frame-sized) per-link residual reaches @λ^safe@ via the elastic buffers.
@λ^safe@ should sit at or above the observed latency so the corrections are small
insertions, but small removals are fine too (within the elastic buffer's safe range).
-}
module Bittide.Instances.Hitl.Utils.Relabel (
  -- * Building UGN edges from hardware captures
  hardwareUgnEdges,

  -- * Computing a relabel
  RelabelPlan (..),
  computeRelabel,

  -- * Applying a relabel over GDB
  readCurrentTime,
  writeReleaseCycle,
  writeCorrections,
) where

import Clash.Prelude

import Bittide.Instances.Hitl.Setup (DeviceInfo (..), FpgaCount, LinkCount)
import Bittide.Instances.Hitl.Utils.MemoryMap (getPathAddress)
import Bittide.Instances.Hitl.Utils.Ugn (
  CounterCapture (..),
  UgnEdge (..),
  counterCaptureToTimingOracle,
  indexToNodeId,
  timingOracleToUgnEdge,
 )
import Bittide.Instances.Hitl.Utils.UgnGrooming (GroomUgnResult (..), groomToSafe)
import Bittide.Wishbone (TimeCmd (Capture))

import Data.Vector.Internal.Check (HasCallStack)
import Gdb (Gdb)
import Project.Handle (expectRight)
import Protocols.MemoryMap (MemoryMap)
import Vivado.Tcl (HwTarget)

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Gdb

{- | Turn the per-node, per-link captured @(localCounter, remoteCounter)@ pairs into
'UgnEdge's keyed by DNA node id. The outer vector is FPGA-indexed (the capturing /
destination node); each inner vector is link-indexed.
-}
hardwareUgnEdges ::
  Vec FpgaCount (Vec LinkCount (Unsigned 64, Unsigned 64)) -> [UgnEdge]
hardwareUgnEdges captures =
  L.concat (toList (imap nodeEdges captures))
 where
  nodeEdges dstIndex links = toList (imap (edge dstIndex) links)
  edge dstIndex port (localCounter, remoteCounter) =
    timingOracleToUgnEdge
      (counterCaptureToTimingOracle dstIndex (CounterCapture port localCounter remoteCounter))

{- | The result of grooming a measured boot onto a target: per-node reset offsets and
per-node, per-link frame corrections, both FPGA-/link-indexed and ready to apply.
-}
data RelabelPlan = RelabelPlan
  { resetOffsets :: Vec FpgaCount (Signed 64)
  {- ^ Per-node offset to add to the shared base to get its reset-release cycle. This
  realises the relabel @q@: every node's application counter leaves reset in the
  relabeled domain. Gauged so node 0's offset is 0.
  -}
  , corrections :: Vec FpgaCount (Vec LinkCount (Signed 64))
  {- ^ Per-node, per-link frame corrections: the frames the management unit inserts
  (@> 0@) or removes (@< 0@) on each receiving link to reach @λ^safe@ after relabeling.
  -}
  }
  deriving (Show)

{- | Groom this boot's measured UGNs onto the target @λ^safe@ and produce an applyable
'RelabelPlan': per-node reset offsets (the relabel @q@) and per-link frame corrections.
Returns 'Left' (with the witnessing node ids) if the UGNs changed too much to fit under
@λ^safe@.

The relabel is the /round-trip split/, not the Bellman-Ford potential. Bellman-Ford
('groomToSafe') is used only as the feasibility check: it answers "are the groomed UGNs
allowed" and witnesses the offending negative cycle when not. Its shortest-path
potentials are deliberately /not/ used for the timing — they place each hop on a
forward- or reverse-slack boundary, so the relabel would absorb link-latency asymmetry,
not just the boot offset, and that error accumulates and breaks a directed transport
chain.

Instead, for each hop @i <-> j@ (both directions must be present) the round trip is
split into a boot offset and a one-way latency:

  * @O_{i->j} = (λ_{i->j} - λ_{j->i}) \`div\` 2@ — the boot offset.
  * @λ_{i->j} - O_{i->j}@ — the measured one-way latency (the exact complement of the
    offset, so the two reconstruct @λ_{i->j}@ with no rounding drift).

The per-node reset offset is the offset potential @φ@ (@φ[node 0] = 0@,
@φ[j] = φ[i] + O_{i->j}@ along a spanning tree of the measured graph); the reset release
removes it. The per-link correction @λ^safe_{i->j} - (λ_{i->j} - O_{i->j})@ is the
frames the receiving node inserts (or removes) to bring the link's latency to the
target. Edges are matched by @(srcNode, dstNode)@; only hops present in both the
measured and target sets are groomed.
-}
computeRelabel ::
  -- | Measured UGNs @λ@
  [UgnEdge] ->
  -- | Target UGNs @λ^safe@
  [UgnEdge] ->
  Either [BitVector 32] RelabelPlan
computeRelabel measured target =
  case groomToSafe measured target of
    UgnsChangedTooMuch ns -> Left ns
    Groomed{} ->
      Right
        RelabelPlan
          { resetOffsets = imap (\idx _ -> fromInteger (phiOf (indexToNodeId idx))) indexUnit
          , corrections = imap (\idx _ -> imap (\port _ -> frameOf idx port) linkUnit) indexUnit
          }
 where
  indexUnit = repeat () :: Vec FpgaCount ()
  linkUnit = repeat () :: Vec LinkCount ()
  root = indexToNodeId (0 :: Index FpgaCount)

  measuredOf = Map.fromList [((e.srcNode, e.dstNode), toInteger e.ugn) | e <- measured]
  targetOf = Map.fromList [((e.srcNode, e.dstNode), toInteger e.ugn) | e <- target]

  -- Boot-offset half of a hop's round trip (the one-way latency cancels).
  offsetOf src dst = do
    fwd <- Map.lookup (src, dst) measuredOf
    rev <- Map.lookup (dst, src) measuredOf
    pure ((fwd - rev) `div` 2)

  -- Frames to bring each receiving link's latency to the target, keyed by the receiving
  -- (node, port): λ^safe_{i->j} - (λ_{i->j} - O_{i->j}).
  frameByDst =
    Map.fromList
      [ ((e.dstNode, e.dstPort), tgt - (toInteger e.ugn - o))
      | e <- measured
      , Just o <- [offsetOf e.srcNode e.dstNode]
      , Just tgt <- [Map.lookup (e.srcNode, e.dstNode) targetOf]
      ]
  frameOf idx port = fromInteger (Map.findWithDefault 0 (indexToNodeId idx, port) frameByDst)

  -- Offset potential φ over a spanning tree (BFS) of the measured graph, rooted at
  -- node 0: the per-node boot offset removed by the reset release.
  adjacency =
    Map.fromListWith
      (<>)
      [(e.srcNode, [(e.dstNode, o)]) | e <- measured, Just o <- [offsetOf e.srcNode e.dstNode]]
  phi = bfs (Map.singleton root 0) [root]
   where
    bfs acc [] = acc
    bfs acc (u : queue) = bfs acc' (queue <> new)
     where
      (acc', new) =
        L.foldl'
          ( \(a, ns) (n, o) ->
              if Map.member n a then (a, ns) else (Map.insert n (a Map.! u + o) a, ns <> [n])
          )
          (acc, [])
          (Map.findWithDefault [] u adjacency)
  phiOf node = Map.findWithDefault 0 node phi

{- | Read the current local-counter value from a device's @Timer@ peripheral. Requires
the CPU to be halted.
-}
readCurrentTime :: (HasCallStack) => MemoryMap -> (HwTarget, DeviceInfo) -> Gdb -> IO (Unsigned 64)
readCurrentTime mm (_, d) gdb = do
  putStrLn $ "Getting current time from device " <> d.deviceId
  commandAddress <- expectRight $ getPathAddress mm ["0", "Timer", "command"]
  scratchAddress <- expectRight $ getPathAddress mm ["0", "Timer", "scratchpad"]
  Gdb.writeLe gdb commandAddress Capture
  Gdb.readLe gdb scratchAddress

{- | Write a node's reset-release cycle to its @TimedReset@ peripheral. Once the local
counter exceeds this value the node's application (and its application counter) leave
reset. Requires the CPU to be halted.
-}
writeReleaseCycle :: (HasCallStack) => MemoryMap -> Gdb -> Unsigned 64 -> IO ()
writeReleaseCycle mm gdb releaseCycle = do
  addr <- expectRight $ getPathAddress mm ["0", "TimedReset", "release_cycle"]
  Gdb.writeLe gdb addr releaseCycle

{- | Write the per-link frame corrections and set the @valid@ flag on the
@UgnCorrections@ peripheral. Requires the CPU to be halted; resuming it makes the
management unit apply the corrections to its elastic buffers.
-}
writeCorrections :: (HasCallStack) => MemoryMap -> Gdb -> Vec LinkCount (Signed 64) -> IO ()
writeCorrections mm gdb corrections = do
  correctionsAddr <- expectRight $ getPathAddress mm ["0", "UgnCorrections", "corrections"]
  validAddr <- expectRight $ getPathAddress mm ["0", "UgnCorrections", "valid"]
  Gdb.writeLe gdb correctionsAddr corrections
  Gdb.writeLe gdb validAddr True
