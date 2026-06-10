-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- |
A thin adapter exposing the pure UGN grooming algorithm
("Bittide.ClockControl.Ugn.Grooming") in terms of the HITL 'UgnEdge' type.

Nodes are identified by their DNA-derived 'BitVector' 32 id (the @srcNode@ /
@dstNode@ fields of a 'UgnEdge'), so the graph is keyed directly on those ids.
Edges are matched between the measured @λ@ and stored @λ^safe@ snapshots by their
@(srcNode, dstNode)@ pair — matching 'compareUgnEdges' / 'findMismatchedUgnEdges'
and unique because the demo topology is a complete graph (one link per ordered
node pair).

@Signed 64@ UGN values are widened to 'Integer' for the (overflow-prone)
relaxation, and the resulting frame counts are narrowed back with a bounds check.
-}
module Bittide.Instances.Hitl.Utils.UgnGrooming (
  ugnGraph,
  safeMargin,
  canonicalizeUgn,
  symmetrizeUgn,
  isAllowedUgn,
  GroomUgnResult (..),
  groomToSafe,
) where

import Prelude

import Clash.Prelude (BitVector, Signed, checkedFromIntegral)

import Bittide.ClockControl.Ugn.Grooming (GroomResult (..), groomCorrection, isAllowed)
import Bittide.Graph.Weighted (Graph)
import Bittide.Instances.Hitl.Utils.Ugn (UgnEdge (..))

import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)

import qualified Bittide.Graph.Weighted as G
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

{- | Symmetrize UGN edges so the two directions of every (bidirectional) link carry as
nearly equal a UGN as possible: each direction is set to half the link's round-trip,
@λ'_{i->j} = ⌊rt/2⌋@ and @λ'_{j->i} = ⌈rt/2⌉@ where @rt = λ_{i->j} + λ_{j->i}@ (the
lower-id direction takes the floor). This guarantees:

  * the two directions differ by at most one (exactly @rt \`mod\` 2@), and
  * non-negative UGNs, since a physically allowed round-trip is @>= 0@.

It preserves each link's round-trip exactly but, unlike 'canonicalizeUgn', is /not/ a
gauge relabeling — splitting every link at its midpoint cannot be realized by a single
per-node potential when the physical asymmetry has nonzero curl (the "symmetric midpoint
is only a valid relabeling on an acyclic graph" caveat). It is instead a canonical,
boot-offset-free symmetric form: the large per-node counter offsets cancel in the
round-trip, so the result is small and balanced.

Edges without a reverse-direction counterpart are left unchanged.
-}
symmetrizeUgn :: [UgnEdge] -> [UgnEdge]
symmetrizeUgn es = map symmetrize es
 where
  ugnOf = Map.fromList [((e.srcNode, e.dstNode), toInteger e.ugn) | e <- es]
  symmetrize e =
    case Map.lookup (e.dstNode, e.srcNode) ugnOf of
      Nothing -> e
      Just rev ->
        let
          roundTrip = toInteger e.ugn + rev
          half = roundTrip `div` 2 -- floor, since a physical round-trip is >= 0
          balanced = if e.srcNode < e.dstNode then half else roundTrip - half
         in
          e{ugn = checkedFromIntegral balanced}

-- | Are these measured UGNs physically allowed (no negative cycle)?
isAllowedUgn :: [UgnEdge] -> Bool
isAllowedUgn = isAllowed . ugnGraph

-- | Outcome of 'groomToSafe'.
data GroomUgnResult
  = {- | UGNs changed too much: the slack graph has a negative cycle, witnessed by
    these node ids.
    -}
    UgnsChangedTooMuch [BitVector 32]
  | {- | A feasible restore. 'correction' is the per-node relabeling @q@ to apply;
    'frames' is the number of frames to insert per edge after relabeling (each
    @>= 0@); 'groomedEdges' are the resulting edges at @λ^safe@. Applying the
    correction reconstructs @λ^safe@: for every measured edge @i -> j@,
    @λ_{i->j} + q_j - q_i + frames_{i->j} == λ^safe_{i->j}@.
    -}
    Groomed
      { correction :: Map (BitVector 32) (Signed 64)
      , groomedEdges :: [UgnEdge]
      , frames :: [(UgnEdge, Signed 64)]
      }
  deriving (Show, Eq)

{- | Groom a freshly measured set of UGNs @λ@ back to a previously stored
@λ^safe@. Returns 'UgnsChangedTooMuch' if no relabeling can fit @λ@ under
@λ^safe@, otherwise the relabeled-and-padded edges plus the per-edge frame counts.

Only edges present in /both/ snapshots (matched by @(srcNode, dstNode)@) are
groomed; edges unique to one snapshot are ignored.
-}
groomToSafe ::
  -- | Measured @λ@
  [UgnEdge] ->
  -- | Stored @λ^safe@
  [UgnEdge] ->
  GroomUgnResult
groomToSafe measured safe =
  case groomCorrection (ugnGraph measured) (ugnGraph safe) of
    Infeasible cycleNodes -> UgnsChangedTooMuch cycleNodes
    Feasible{correction = q, framesToInsert} ->
      Groomed
        { correction = Map.map checkedFromIntegral q
        , groomedEdges = mapMaybe (\(i, j, _) -> Map.lookup (i, j) safeByKey) framesToInsert
        , frames =
            [ (e, checkedFromIntegral f)
            | (i, j, f) <- framesToInsert
            , Just e <- [Map.lookup (i, j) measuredByKey]
            ]
        }
 where
  measuredByKey = byKey measured
  safeByKey = byKey safe
  byKey es = Map.fromList [((e.srcNode, e.dstNode), e) | e <- es]
