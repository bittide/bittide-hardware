-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- |
The application-facing UGN grooming layer for HITL: it adapts the generic grooming
algorithm ("Bittide.ClockControl.Ugn.Grooming", on "Bittide.Graph.Weighted") to the HITL
'UgnEdge' type and turns its result into a directly-applyable 'RelabelPlan'.

Nodes are identified by their DNA-derived 'BitVector' 32 id (the @srcNode@ / @dstNode@
fields of a 'UgnEdge'), so the graph is keyed directly on those ids. Edges are matched
between the measured @λ@ and stored @λ^safe@ snapshots by their @(srcNode, dstNode)@ pair
— unique because the demo topology is a complete graph (one link per ordered node pair).

@Signed 64@ UGN values are widened to 'Integer' for the (overflow-prone) relaxation, and
the resulting reset offsets / frame counts are narrowed back with a bounds check.
-}
module Bittide.Instances.Hitl.Utils.UgnGrooming (
  ugnGraph,
  safeMargin,
  canonicalizeUgn,
  RelabelPlan (..),
  computeRelabel,
) where

import Prelude

import Clash.Prelude (BitVector, Signed, Vec, checkedFromIntegral)

import Bittide.ClockControl.Ugn.Grooming (GroomResult (..), groomCorrection)
import Bittide.Graph.Weighted (Graph)
import Bittide.Instances.Hitl.Setup (FpgaCount, LinkCount, knownLinkConfigs)
import Bittide.Instances.Hitl.Utils.Ugn (UgnEdge (..), indexToNodeId)

import qualified Bittide.Graph.Weighted as G
import qualified Clash.Prelude as CP
import qualified Data.Map.Strict as Map

{- | Build a weighted graph from UGN edges, keyed on the DNA-derived node ids with
the (signed) UGN as the 'Integer' edge weight.
-}
ugnGraph :: [UgnEdge] -> Graph (BitVector 32) Integer
ugnGraph es = G.fromEdges [(e.srcNode, e.dstNode, toInteger e.ugn) | e <- es]

{- | Add a safety margin @ε@ to every edge: @λ^safe = λ + ε@. Mirrors the
record-update style of 'Bittide.Instances.Hitl.Utils.Ugn.addLatencyEdge'.
-}
safeMargin :: Signed 64 -> [UgnEdge] -> [UgnEdge]
safeMargin eps = map (\e -> e{ugn = e.ugn + eps})

{- | Relabel UGN edges to their minimal non-negative form: the Bellman-Ford reduced
costs @λ_{i->j} + q_i - q_j@ (potentials @q@ from the super-source), which are all
@>= 0@ and leave every round-trip / cycle sum unchanged.

This is a pure gauge change (a relabeling), so it represents the /same/ physical
system, but in a gauge where the UGNs are small and non-negative. Storing
@λ^safe@ in this gauge keeps the application's per-link UGNs (and hence its fixed
schedule) inside the working range a depth-bounded mux/elastic-buffer needs, while
still honouring the true /asymmetric/ cycle constraints (unlike a symmetric
midpoint, which is only a valid relabeling on an acyclic graph).

If the edges are not physically allowed (a negative cycle), they are returned
unchanged.
-}
canonicalizeUgn :: [UgnEdge] -> [UgnEdge]
canonicalizeUgn es =
  case G.potentials (ugnGraph es) of
    Nothing -> es
    Just q ->
      [ e{ugn = checkedFromIntegral (toInteger e.ugn + qOf e.srcNode - qOf e.dstNode)}
      | e <- es
      ]
     where
      qOf n = Map.findWithDefault 0 n q

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
@λ^safe@ (a negative cycle in the slack graph).

Calls 'groomCorrection' (Bellman-Ford, per the UGN grooming slides) directly on the
measured / target 'ugnGraph's. The relabel @q@ is the per-node potential that fits this
boot's @λ@ under the stored @λ^safe@ (@λ + B^T q <= λ^safe@); the residual
@frames = λ^safe - (λ + B^T q)@ are the small, non-negative per-link corrections the
elastic buffers insert. So:

  * @resetOffsets_i = -(q_i - q_0)@ — the reset release that applies the relabel
    (gauged so node 0 is 0); and
  * @corrections@ — the per-receiving-link frames. Since 'groomCorrection' returns frames
    keyed by @(srcNode, dstNode)@, the receiving @(fpga, port)@ is reconstructed via
    'knownLinkConfigs' (the same link map 'Bittide.Instances.Hitl.Utils.Ugn.ugnEdges'
    uses).

When @λ^safe@ is stored in a small non-negative gauge (see 'canonicalizeUgn'), the relabel
absorbs this boot's counter offsets so the groomed app-frame UGNs land back on @λ^safe@ —
small, non-negative, and identical every run, which is what makes a single fixed schedule
reusable.

Edges are matched by @(srcNode, dstNode)@; pass the full measured + target graphs to
groom every link.
-}
computeRelabel ::
  -- | Measured UGNs @λ@
  [UgnEdge] ->
  -- | Target UGNs @λ^safe@
  [UgnEdge] ->
  Either [BitVector 32] RelabelPlan
computeRelabel measured target =
  case groomCorrection (ugnGraph measured) (ugnGraph target) of
    Infeasible ns -> Left ns
    Feasible{correction = q, framesToInsert} ->
      Right
        RelabelPlan
          { resetOffsets = (\idx -> negate (qOf idx - qOf 0)) <$> CP.indicesI
          , corrections = (\idx -> (\port -> frameOf idx port) <$> CP.indicesI) <$> CP.indicesI
          }
     where
      qOf idx = checkedFromIntegral (Map.findWithDefault 0 (indexToNodeId idx) q)
      frameByEdge = Map.fromList [((i, j), f) | (i, j, f) <- framesToInsert]
      frameOf dstIdx port =
        checkedFromIntegral (Map.findWithDefault 0 (srcNode, dstNode) frameByEdge)
       where
        srcNode = indexToNodeId ((knownLinkConfigs CP.!! dstIdx) CP.!! port)
        dstNode = indexToNodeId dstIdx
