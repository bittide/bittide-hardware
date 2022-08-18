-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Some graphs from mathematics.
module Bittide.Topology.Graph ( complete, cyclic, diamond, star, tree ) where

import Prelude

import Data.Array qualified as A
import Data.Function (on)
import Data.Graph (Graph, graphFromEdges)
import Data.List (groupBy, sort)
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)

-- | Diamond graph
diamond :: Graph
diamond = A.listArray (0, 3) [[1,3], [0,2,3], [2,3], [0,1,2]]

-- | Tree of depth @depth@ with @c@ children
tree :: Int -> Int -> Graph
tree depth c = treeGraph
 where
  -- | At depth @d_i@, child node @i@ is connected to the @(i-1) `div` c + 1@st
  -- node at depth @d_i - 1@
  pairs = [ (d_i, i, (i-1) `div` c + 1) | d_i <- [0..depth], i <- [1..(c^d_i)] ]
  mkEdges (0, _, _)           = Nothing
  mkEdges (lvl, node, p_node) = Just ((lvl, node), (lvl-1, p_node))
  directedEdges = mapMaybe mkEdges pairs
  -- "Data.Graph" takes a directed graphs
  edges = directedEdges ++ fmap swap directedEdges
  adjList = g <$> groupBy ((==) `on` fst) (sort edges)
  g ps@((x,_):_) = (x, snd <$> ps)
  g [] = (error "Internal error: no edges.", [])
  (treeGraph, _, _) =
    graphFromEdges ((\(key, keys) -> ((), key, keys)) <$> adjList)

-- | [Star graph](https://mathworld.wolfram.com/StarGraph.html)
star :: Int -> Graph
star = tree 1

-- | [Cyclic graph](https://mathworld.wolfram.com/CycleGraph.html) with @n@
-- vertices.
cyclic :: Int -> Graph
cyclic n = A.array bounds (fmap (\i -> (i, neighbors i)) [0..(n-1)])
 where
  bounds = (0, n-1)
  neighbors i = [(i-1) `mod` n, (i+1) `mod` n]

-- | [Complete graph](https://mathworld.wolfram.com/CompleteGraph.html) with @n@
-- vertices.
complete :: Int -> Graph
complete n = A.array bounds (fmap (\i -> (i, others i)) [0..(n-1)])
 where
  bounds = (0, n-1)
  others i = [ j | j <- [0..(n-1)], j /= i ]
