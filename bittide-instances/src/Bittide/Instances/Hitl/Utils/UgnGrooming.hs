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
  symmetrizingPotentials,
  isAllowedUgn,
  GroomUgnResult (..),
  groomToSafe,
) where

import Prelude

import Clash.Prelude (BitVector, Signed, checkedFromIntegral)

import Bittide.ClockControl.Ugn.Grooming (GroomResult (..), groomCorrection, isAllowed)
import Bittide.Graph.Weighted (Graph)
import Bittide.Instances.Hitl.Utils.Ugn (UgnEdge (..))

import Data.List (sortOn, tails)
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

{- | Best-effort /relabeling/ that makes the two directions of every link carry as nearly
equal a UGN as possible.

This is a relabel (@λ'_{i->j} = λ_{i->j} + q_i - q_j@), so — unlike a per-link midpoint —
it leaves every round-trip and every cycle sum unchanged: the physical latency
characteristics are preserved exactly. A relabel only redistributes each link's fixed
round-trip between its two directions.

Perfect symmetry (both directions equal on every link) is reachable by a relabel only
when the per-link imbalance is curl-free (the "symmetric midpoint is a valid relabeling
only on an acyclic graph" caveat). In general it is not, so we minimise the cost

  @cost(q) = Σ_{neighbours i,j} (λ'_{i->j} - λ'_{j->i})^2@

— the squared difference between the two directions of every link, summed over all
neighbouring node pairs. That is a graph-Laplacian least-squares problem (see
'symmetrizingPotentials'): the per-node clock/boot-offset (gradient) part of the imbalance
is cancelled completely, and only the irreducible /physical/ asymmetry (forward path ≠
reverse path, the curl) remains, spread across the links it touches.

Potentials are rounded to integers (the relabel is realised as integer cycle offsets).
-}
symmetrizeUgn :: [UgnEdge] -> [UgnEdge]
symmetrizeUgn es =
  [e{ugn = e.ugn + checkedFromIntegral (qOf e.srcNode - qOf e.dstNode)} | e <- es]
 where
  q = symmetrizingPotentials es
  qOf n = Map.findWithDefault 0 n q

{- | Integer per-node potentials @q@ that least-squares minimise the total squared
directional imbalance @Σ (λ_{i->j} + q_i - q_j - (λ_{j->i} + q_j - q_i))^2@ over all
neighbouring node pairs (see 'symmetrizeUgn'). Equivalently, with the antisymmetric
half-difference @A_{i->j} = (λ_{i->j} - λ_{j->i}) / 2@, it minimises @Σ (A_{i->j} + q_i -
q_j)^2@, whose stationarity condition is the graph-Laplacian normal equations @L q = b@
with @b_i = -Σ_j A_{i->j}@. The relabel is gauge-free (a constant shift of all @q@ leaves
every UGN unchanged), so node 0 is pinned to 0. Potentials are rounded to integers.
-}
symmetrizingPotentials :: [UgnEdge] -> Map (BitVector 32) Integer
symmetrizingPotentials es
  | n == 0 || null links = Map.fromList [(node, 0) | node <- nodes]
  | otherwise = Map.fromList (zip nodes (map round qReal))
 where
  ugnOf = Map.fromList [((e.srcNode, e.dstNode), toInteger e.ugn) | e <- es]
  -- All node ids, deduplicated and in a stable order.
  nodes = Map.keys (Map.fromList [(node, ()) | e <- es, node <- [e.srcNode, e.dstNode]])
  n = length nodes
  indexOf = (Map.fromList (zip nodes [0 ..]) Map.!)
  {- Antisymmetric half-difference of each bidirectional link, keyed by node /position/.
  @i < j@ (by position) so each undirected link appears once. -}
  links =
    [ (indexOf i, indexOf j, fromIntegral (uij - uji) / 2 :: Double)
    | (i : rest) <- tails nodes
    , j <- rest
    , Just uij <- [Map.lookup (i, j) ugnOf]
    , Just uji <- [Map.lookup (j, i) ugnOf]
    ]
  -- Graph-Laplacian normal equations L q = rhs for min Σ (A + q_i − q_j)².
  lap =
    [ [if a == b then fromIntegral (degree a) else if linked a b then -1 else 0 | b <- [0 .. n - 1]]
    | a <- [0 .. n - 1]
    ]
  rhs = [sum [contrib a lk | lk <- links] | a <- [0 .. n - 1]]
  degree a = length [() | (i, j, _) <- links, i == a || j == a]
  linked a b = any (\(i, j, _) -> (i, j) == (a, b) || (i, j) == (b, a)) links
  contrib a (i, j, w)
    | a == i = -w
    | a == j = w
    | otherwise = 0
  -- L has the all-ones vector in its kernel; pin node 0 to 0 and solve the rest.
  qReal
    | n == 1 = [0]
    | otherwise = 0 : solveLinear sub subRhs
   where
    sub = [[lap !! a !! b | b <- [1 .. n - 1]] | a <- [1 .. n - 1]]
    subRhs = [rhs !! a | a <- [1 .. n - 1]]

{- | Solve a small dense linear system @A x = b@ by Gaussian elimination with partial
pivoting. Intended for the tiny (node-count sized) systems in 'symmetrizingPotentials'.
-}
solveLinear :: [[Double]] -> [Double] -> [Double]
solveLinear a b = solve (zipWith (,) a b)
 where
  -- Each equation is (coefficients, rhs); eliminate the leading variable each level.
  solve eqs =
    case sortOn (\(cs, _) -> negate (abs (firstCoeff cs))) eqs of
      [] -> []
      (pivotCoeffs, pivotRhs) : rest ->
        case pivotCoeffs of
          [] -> []
          lead : leadTail ->
            let
              norm = map (/ lead) leadTail -- normalised coeffs for the remaining vars
              normRhs = pivotRhs / lead
              reduce (cs, r) = case cs of
                c0 : csTail -> (zipWith (\cv nv -> cv - c0 * nv) csTail norm, r - c0 * normRhs)
                [] -> (cs, r)
              xsRest = solve (map reduce rest)
              x0 = normRhs - sum (zipWith (*) norm xsRest)
             in
              x0 : xsRest
  firstCoeff cs = case cs of
    x : _ -> x
    [] -> 0

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
