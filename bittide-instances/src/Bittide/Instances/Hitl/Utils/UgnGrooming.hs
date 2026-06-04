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
