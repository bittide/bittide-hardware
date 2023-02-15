-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Some graphs from mathematics.
module Bittide.Topology.Graph
  ( Graph
      ( unboundGraph
      , hasEdge
      )
  , boundGraph
  , cyclic
  , complete
  , diamond
  , grid
  , star
  , torus2d
  , torus3d
  , tree
  , line
  , hypercube
  )
where

import Prelude
import Clash.Prelude
  ( SNat(..)
  , Nat
  , Index
  , KnownNat
  , type (^)
  , type (*)
  , type (+)
  , type (-)
  , type Div
  , snatToNum
  )

import GHC.TypeNats (natVal)
import GHC.Num.Natural(Natural)
import Data.Bifunctor (bimap)
import Data.Proxy (Proxy(..))
import Data.Bits (Bits (..))
import Data.Containers.ListUtils (nubOrd)
import Data.Function (on)
import Data.Graph (graphFromEdges, edges)
import Data.Graph qualified as G (Graph)
import Data.List (groupBy, sort)
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)

import Data.Array qualified as A

import GHC.TypeLits.KnownNat

data Graph (n :: Nat) =
  BoundGraph
    { unboundGraph :: G.Graph
    , hasEdge :: Index n -> Index n -> Bool
    }

-- | Smart constructor of 'BoundGraph'.
boundGraph :: KnownNat n => G.Graph -> Graph n
boundGraph graph = BoundGraph graph $ curry $ (A.!)
  $ A.accumArray (const id) False bounds
  $ zip (filter (uncurry (/=)) edgeIndices) [True, True ..]
 where
  bounds = ((minBound, minBound), (maxBound, maxBound))
  edgeIndices = map (bimap fromIntegral fromIntegral) $ edges graph

-- | @n@ nodes in a line, connected to their neighbors.
--
-- (differs from the
-- [mathematical terminology](https://mathworld.wolfram.com/LineGraph.html) but
-- conforms to callisto)
line :: KnownNat n => SNat n -> Graph n
line (snatToNum -> n :: Int) = boundGraph $ fromEdgeList es
 where
  es = [ (i, i-1) | i <- [2..n] ]

-- | @n@-dimensional hypercube
hypercube :: KnownNat n => SNat n -> Graph (2 ^ n)
hypercube (snatToNum -> n :: Int) = boundGraph $ fromEdgeList es
 where
  k = (2::Int)^n
  es =
    -- see Callisto code (julia):
    -- https://github.com/bittide/Callisto.jl/blob/73d908c6cb02b9b953cc104e5b42d432efc42598/src/topology.jl#L224
    [ let j = i .|. (1 `shiftL` b) in (i+1, j+1)
    | i <- [0..(k-1)], b <- [0..(n-1)], i .&. (1 `shiftL` b) == 0
    ]

-- | Diamond graph
diamond :: Graph 4
diamond = boundGraph $ A.listArray (0, 3)
  [[1,3], [0,2,3], [1,3], [0,1,2]]

-- | Given a list of edges, make a directed graph
fromEdgeList :: forall a. (Ord a) => [(a, a)] -> G.Graph
fromEdgeList es = dirGraph
 where
  -- "Data.Graph" deals with directed graphs
  allEdges :: [(a, a)]
  allEdges = es ++ fmap swap es
  adjList :: [(a, [a])]
  adjList = g <$> groupBy ((==) `on` fst) (nubOrd $ sort allEdges)
  -- now that we have a sorted/grouped list of edges, reformat by attaching
  -- a list of all connected nodes to each node.
  g :: [(a, b)] -> (a, [b])
  g ps@((x,_):_) = (x, snd <$> ps)
  g [] = error "No edges."
  (dirGraph, _, _) =
    graphFromEdges ((\(key, keys) -> ((), key, keys)) <$> adjList)

torus3d ::
  (KnownNat a, KnownNat b, KnownNat c) =>
  SNat a ->
  SNat b ->
  SNat c ->
  Graph (a * b * c)
torus3d (snatToNum -> a :: Int) (snatToNum -> b :: Int) (snatToNum -> c :: Int) =
  boundGraph $ fromEdgeList dirEdges
 where
  pairs = [ (l, m, n) | l <- [0..(a-1)], m <- [0..(b-1)], n <- [0..(c-1)] ]
  neighborsOf (l, m, n) =
    [ ((l-1) `mod` a, m, n)
    , ((l+1) `mod` a, m, n)
    , (l, (m-1) `mod` b, n)
    , (l, (m+1) `mod` b, n)
    , (l, m, (n-1) `mod` c)
    , (l, m, (n+1) `mod` c)
    ]
  dirEdges = concatMap (\p -> fmap (p,) (neighborsOf p)) pairs

-- | See [this figure](https://www.researchgate.net/figure/The-two-dimensional-torus-4x4_fig1_221134153)
torus2d ::
  (KnownNat rows, KnownNat cols) =>
  SNat rows ->
  SNat cols ->
  Graph (rows * cols)
torus2d (snatToNum -> rows :: Int) (snatToNum -> cols :: Int) =
  boundGraph $ fromEdgeList dirEdges
 where
  pairs = [ (m, n) | m <- [0..(rows-1)], n <- [0..(cols-1)] ]
  neighborsOf (m, n) =
    [ ((m-1) `mod` rows, n)
    , ((m+1) `mod` rows, n)
    , (m, (n-1) `mod` cols)
    , (m, (n+1) `mod` cols)
    ]
  dirEdges = concatMap (\p -> fmap (p,) (neighborsOf p)) pairs

-- | [Grid graph](https://mathworld.wolfram.com/GridGraph.html)
grid :: (KnownNat rows, KnownNat cols) =>
  SNat rows ->
  SNat cols ->
  Graph (rows * cols)
grid (snatToNum -> rows :: Int) (snatToNum -> cols :: Int) =
  boundGraph $ fromEdgeList dirEdges
 where
  pairs = [ (m, n) | m <- [1..rows], n <- [1..cols] ]
  mkEdges (m, n) =
    [ (a, b) | a <- [(m-1)..(m+1)], b <- [(n-1)..(n+1)], a /= m || b /= n, a == m || b == n, a > 0, b > 0, a <= rows, b <= cols ]
  dirEdges = concatMap (\p -> fmap (p,) (mkEdges p)) pairs

type family TreeSize (depth :: Nat) (children :: Nat) where
  TreeSize depth 0        = 1
  TreeSize depth 1        = depth + 1
  TreeSize depth children =
    Div ((children ^ (depth + 1)) - 1) (children - 1)

instance (KnownNat n, KnownNat m) => KnownNat2 $(nameToSymbol ''TreeSize) n m where
  natSing2 =
    let
      x = natVal (Proxy @n)
      y = natVal (Proxy @m)
      z = treeSize x y
    in
      SNatKn z
   where
     treeSize :: Natural -> Natural -> Natural
     treeSize d = \case
       0 -> 1
       1 -> d + 1
       c -> ((c ^ (d + 1)) - 1) `div` (c - 1)
  {-# INLINE natSing2 #-}

-- | Tree of depth @depth@ with @childs@ children
tree ::
  (KnownNat depth, KnownNat childs) =>
  SNat depth ->
  SNat childs ->
  Graph (TreeSize depth childs)
tree (snatToNum -> depth :: Int) (snatToNum -> c :: Int) = boundGraph treeGraph
 where
  -- | At depth @d_i@, child node @i@ is connected to the @(i-1) `div` c + 1@st
  -- node at depth @d_i - 1@
  pairs = [ (d_i, i, (i-1) `div` c + 1) | d_i <- [0..depth], i <- [1..(c^d_i)] ]
  mkEdges (0, _, _)           = Nothing
  mkEdges (lvl, node, p_node) = Just ((lvl, node), (lvl-1, p_node))
  directedEdges = mapMaybe mkEdges pairs
  treeGraph = fromEdgeList directedEdges

-- | [Star graph](https://mathworld.wolfram.com/StarGraph.html)
star :: (KnownNat childs) => SNat childs -> Graph (TreeSize 1 childs)
star = tree (SNat :: SNat 1)

-- | [Cyclic graph](https://mathworld.wolfram.com/CycleGraph.html) with @n@
-- vertices.
cyclic :: (KnownNat n) => SNat n -> Graph n
cyclic (snatToNum -> n) =
  boundGraph $ A.array bounds (fmap (\i -> (i, neighbors i)) [0..(n-1)])
 where
  bounds = (0, n-1)
  neighbors i = [(i-1) `mod` n, (i+1) `mod` n]

-- | [Complete graph](https://mathworld.wolfram.com/CompleteGraph.html) with @n@
-- vertices.
complete :: (KnownNat n) => SNat n -> Graph n
complete (snatToNum -> n) =
  boundGraph $ A.array bounds (fmap (\i -> (i, others i)) [0..(n-1)])
 where
  bounds = (0, n-1)
  others i = [ j | j <- [0..(n-1)], j /= i ]
