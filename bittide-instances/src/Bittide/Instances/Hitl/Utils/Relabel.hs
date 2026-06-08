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
  -- * Building UGN edges
  ugnEdges,
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

import Bittide.Instances.Hitl.Setup (DeviceInfo (..), FpgaCount, LinkCount, knownLinkConfigs)
import Bittide.Instances.Hitl.Utils.MemoryMap (getPathAddress)
import Bittide.Instances.Hitl.Utils.Ugn (UgnEdge (..), indexToNodeId)
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

{- | Turn a per-node, per-link table of UGN values (@λ@) into 'UgnEdge's keyed by DNA
node id. The outer vector is FPGA-indexed (the capturing / destination node); each
inner vector is link-indexed (matching 'knownLinkConfigs').
-}
ugnEdges :: Vec FpgaCount (Vec LinkCount (Signed 64)) -> [UgnEdge]
ugnEdges lams =
  L.concat (toList (imap nodeEdges lams))
 where
  nodeEdges dstIndex links = toList (imap (edge dstIndex) links)
  edge dstIndex port lam =
    UgnEdge
      { srcNode = indexToNodeId (knownLinkConfigs !! dstIndex !! port)
      , srcPort = port
      , dstNode = indexToNodeId dstIndex
      , dstPort = port
      , ugn = lam
      }

{- | As 'ugnEdges', but from raw @(localCounter, remoteCounter)@ captures: the UGN is
their signed difference @local - remote@.
-}
hardwareUgnEdges ::
  Vec FpgaCount (Vec LinkCount (Unsigned 64, Unsigned 64)) -> [UgnEdge]
hardwareUgnEdges = ugnEdges . map (map (\(l, r) -> bitCoerce l - bitCoerce r))

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

This is a thin wrapper over 'groomToSafe' (Bellman-Ford, per the UGN grooming slides).
The relabel @q@ is the per-node potential that fits this boot's @λ@ under the stored
@λ^safe@ (@λ + B^T q <= λ^safe@); the residual @frames = λ^safe - (λ + B^T q)@ are the
small, non-negative per-link corrections the elastic buffers insert. So:

  * @resetOffsets_i = -(q_i - q_0)@ — the reset release that applies the relabel
    (gauged so node 0 is 0); and
  * @corrections@ — the per-receiving-link frames, keyed by @(dstNode, dstPort)@.

When @λ^safe@ is stored in a small non-negative gauge (see 'lambdaSafe'), the relabel
absorbs this boot's counter offsets so the groomed app-frame UGNs land back on @λ^safe@
— small, non-negative, and identical every run, which is what makes a single fixed
schedule reusable.

Edges are matched by @(srcNode, dstNode)@; pass the full measured + target graphs to
groom every link. Returns 'Left' (with the witnessing node ids) if @λ^safe@ is
unreachable (a negative cycle in the slack graph).
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
    Groomed{correction = q, frames} ->
      Right
        RelabelPlan
          { resetOffsets = (\idx -> negate (qOf idx - qOf 0)) <$> indicesI
          , corrections = (\idx -> (\port -> frameOf idx port) <$> indicesI) <$> indicesI
          }
     where
      qOf idx = Map.findWithDefault 0 (indexToNodeId idx) q
      frameByDst = Map.fromList [((e.dstNode, e.dstPort), f) | (e, f) <- frames]
      frameOf idx port = Map.findWithDefault 0 (indexToNodeId idx, port) frameByDst

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
