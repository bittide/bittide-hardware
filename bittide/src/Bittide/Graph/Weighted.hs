-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- |
A directed, weighted graph backed by a 'Data.Map' adjacency structure, together
with the shortest-path / negative-cycle machinery needed for UGN grooming.

The node label @n@ is generic (only 'Ord' is required), so callers can use a
domain type directly (e.g. a DNA-derived @BitVector 32@) without an intermediate
index-mapping layer. The weight @w@ is likewise generic; grooming uses 'Integer'
to avoid overflow during relaxation.

Note: at most one edge is stored per ordered pair @(i, j)@ ('addEdge' overwrites
a previous weight). This matches the complete-graph bittide demo topology, which
has exactly one directed link per ordered node pair. A topology with parallel
links between the same node pair would need a multigraph representation here.
-}
module Bittide.Graph.Weighted (
  -- * Type
  Graph,

  -- * Construction & query
  empty,
  fromEdges,
  addEdge,
  nodes,
  edges,
  nodeCount,
  edgeCount,
  hasEdge,
  weight,
  outgoing,
  incoming,

  -- * Transforms
  mapWeights,
  transpose,
  relabel,
  zipEdgesWith,

  -- * Paths & cycles
  ShortestPaths (..),
  bellmanFord,
  potentials,
  negativeCycle,
  cycleSum,
) where

import Prelude

import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)

import qualified Data.List as L
import qualified Data.Map.Strict as Map

{- | A directed, weighted graph. Invariant: every node that appears as the source
or destination of an edge is a key of the outer map (with an empty inner map if
it has no outgoing edges), so 'nodes' reports isolated and sink nodes too.
-}
newtype Graph n w = Graph (Map n (Map n w))
  deriving (Show, Eq)

-- | The empty graph (no nodes, no edges).
empty :: Graph n w
empty = Graph Map.empty

{- | Build a graph from an edge list @(src, dst, weight)@. On a duplicate ordered
pair the later entry in the list wins.
-}
fromEdges :: (Ord n) => [(n, n, w)] -> Graph n w
fromEdges = L.foldl' (\g (i, j, w) -> addEdge i j w g) empty

{- | Insert (or overwrite) the edge @i -> j@ with the given weight, registering
both endpoints as nodes.
-}
addEdge :: (Ord n) => n -> n -> w -> Graph n w -> Graph n w
addEdge i j w (Graph m) = Graph m2
 where
  -- Ensure 'j' exists as a node without clobbering its outgoing edges.
  m1 = Map.insertWith (\_new old -> old) j Map.empty m
  m2 = Map.alter (Just . Map.insert j w . fromMaybe Map.empty) i m1

-- | All nodes, in ascending order.
nodes :: Graph n w -> [n]
nodes (Graph m) = Map.keys m

-- | All edges as @(src, dst, weight)@.
edges :: Graph n w -> [(n, n, w)]
edges (Graph m) =
  [(i, j, w) | (i, dsts) <- Map.toList m, (j, w) <- Map.toList dsts]

-- | Number of nodes.
nodeCount :: Graph n w -> Int
nodeCount (Graph m) = Map.size m

-- | Number of edges.
edgeCount :: Graph n w -> Int
edgeCount (Graph m) = sum (Map.size <$> Map.elems m)

-- | Is there an edge @i -> j@?
hasEdge :: (Ord n) => n -> n -> Graph n w -> Bool
hasEdge i j (Graph m) = maybe False (Map.member j) (Map.lookup i m)

-- | Weight of edge @i -> j@, if present.
weight :: (Ord n) => n -> n -> Graph n w -> Maybe w
weight i j (Graph m) = Map.lookup i m >>= Map.lookup j

-- | Outgoing edges of @i@ as @(dst, weight)@.
outgoing :: (Ord n) => n -> Graph n w -> [(n, w)]
outgoing i (Graph m) = maybe [] Map.toList (Map.lookup i m)

-- | Incoming edges of @j@ as @(src, weight)@.
incoming :: (Ord n) => n -> Graph n w -> [(n, w)]
incoming j (Graph m) =
  [(i, w) | (i, dsts) <- Map.toList m, Just w <- [Map.lookup j dsts]]

-- | Apply a function to every edge weight.
mapWeights :: (w -> w') -> Graph n w -> Graph n w'
mapWeights f (Graph m) = Graph (Map.map (Map.map f) m)

-- | Reverse every edge, preserving the node set.
transpose :: (Ord n) => Graph n w -> Graph n w
transpose g@(Graph m) =
  L.foldl' (\acc (i, j, w) -> addEdge j i w acc) base (edges g)
 where
  -- Keep all nodes (so isolated/sink nodes survive), drop all edges.
  base = Graph (Map.map (const Map.empty) m)

{- | Relabel by a potential @q@: @w_{i->j} <- w_{i->j} + q_j - q_i@. Nodes absent
from @q@ are treated as @0@. This leaves the sum of weights around every cycle
unchanged (the @q@ terms telescope).
-}
relabel :: (Ord n, Num w) => Map n w -> Graph n w -> Graph n w
relabel q (Graph m) =
  Graph (Map.mapWithKey (\i dsts -> Map.mapWithKey (adjust i) dsts) m)
 where
  adjust i j w = w + qOf j - qOf i
  qOf n = Map.findWithDefault 0 n q

{- | Combine the weights of edges present in /both/ graphs with the given
function. The result's edge set is the intersection of the two edge sets; its
node set is the union of both node sets (so isolated nodes are preserved).
-}
zipEdgesWith ::
  (Ord n) => (w -> w -> w') -> Graph n w -> Graph n w -> Graph n w'
zipEdgesWith f g1 g2 = L.foldl' step base (edges g1)
 where
  base = Graph (Map.fromList [(n, Map.empty) | n <- L.union (nodes g1) (nodes g2)])
  step acc (i, j, w1) = case weight i j g2 of
    Just w2 -> addEdge i j (f w1 w2) acc
    Nothing -> acc

{- | Sum of edge weights around the closed cycle @n_0 -> n_1 -> ... -> n_k -> n_0@.
'Nothing' if any required edge is missing. The empty list sums to @0@.
-}
cycleSum :: (Ord n, Num w) => Graph n w -> [n] -> Maybe w
cycleSum _ [] = Just 0
cycleSum g ns = sum <$> traverse (\(i, j) -> weight i j g) pairs
 where
  pairs = zip ns (drop 1 ns ++ take 1 ns)

-- | Result of 'bellmanFord': either a witnessed negative cycle, or potentials.
data ShortestPaths n w
  = NegativeCycle [n]
  | Distances (Map n w)
  deriving (Show, Eq)

{- | Bellman-Ford from a virtual super-source: every node starts at distance @0@
(this is /not/ single-source), so disconnected components are each handled
independently and every negative cycle is reachable.

Returns 'Distances' @q@ satisfying @q_j - q_i <= w_{i->j}@ for every edge when no
negative cycle exists, otherwise a 'NegativeCycle' witness (the nodes on a
negative-weight cycle).
-}
bellmanFord :: (Ord n, Ord w, Num w) => Graph n w -> ShortestPaths n w
bellmanFord g =
  case L.find relaxable es of
    Nothing -> Distances dist
    Just (_, j, _) -> NegativeCycle (extractCycle predecessors n j)
 where
  ns = nodes g
  n = length ns
  es = edges g
  dist0 = Map.fromList [(v, 0) | v <- ns]

  -- \|V| - 1 relaxation passes.
  (dist, predecessors) =
    L.foldl' (\acc _ -> relaxOnce es acc) (dist0, Map.empty) [1 .. n - 1]

  -- An edge that can still be relaxed after |V|-1 passes lies downstream of a
  -- negative cycle.
  relaxable (i, j, w) =
    case (Map.lookup i dist, Map.lookup j dist) of
      (Just di, Just dj) -> di + w < dj
      _ -> False

-- | One relaxation pass over all edges, threading distances and predecessors.
relaxOnce ::
  (Ord n, Ord w, Num w) =>
  [(n, n, w)] ->
  (Map n w, Map n n) ->
  (Map n w, Map n n)
relaxOnce es acc = L.foldl' step acc es
 where
  step (d, p) (i, j, w) =
    case (Map.lookup i d, Map.lookup j d) of
      (Just di, Just dj)
        | di + w < dj -> (Map.insert j (di + w) d, Map.insert j i p)
      _ -> (d, p)

{- | Walk the predecessor chain @n@ steps to land on a negative cycle, then follow
it until it repeats. Returns the cycle nodes in forward (edge) order.
-}
extractCycle :: (Ord n) => Map n n -> Int -> n -> [n]
extractCycle predecessors n j = L.reverse (start : rest)
 where
  step v = Map.findWithDefault v v predecessors
  start = applyN n step j
  rest = takeWhile (/= start) (drop 1 (iterate step start))
  applyN k f x = iterate f x !! max 0 k

-- | Potentials @q@ such that @q_j - q_i <= w_{i->j}@, or 'Nothing' on a negative cycle.
potentials :: (Ord n, Ord w, Num w) => Graph n w -> Maybe (Map n w)
potentials g = case bellmanFord g of
  Distances q -> Just q
  NegativeCycle _ -> Nothing

-- | The nodes of a negative-weight cycle, if one exists.
negativeCycle :: (Ord n, Ord w, Num w) => Graph n w -> Maybe [n]
negativeCycle g = case bellmanFord g of
  NegativeCycle c -> Just c
  Distances _ -> Nothing
