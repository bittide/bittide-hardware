-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE QuasiQuotes #-}

module Bittide.ClockControl.Topology (
  -- * Data Types
  TopologyType (..),
  TopologyName,
  Topology (..),

  -- * Special Topologies
  cyclic,
  complete,
  diamond,
  grid,
  star,
  torus2d,
  torus3d,
  tree,
  pendulum,
  line,
  hypercube,
  dumbbell,
  hourglass,
  beads,

  -- * Utilities
  hasEdge,
  size,
  toDot,
  pairwise,
) where

import Prelude

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Bifunctor (bimap)
import Data.Bits (Bits (..))
import Data.Containers.ListUtils (nubOrd)
import Data.Graph (Graph)
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import GHC.Generics (Generic)

import qualified Clash.Util.Interpolate as I
import qualified Data.Array as A (array, listArray)
import qualified Data.Array as Array
import qualified Data.Graph as Graph
import qualified Data.List as L
import qualified Data.Map.Strict as M (fromList, (!))

-- | Special topologies may have names given as a string.
type TopologyName = String

{- | A topology is a graph that is used to build different \"interesting\"
network shapes for clock control to act on.
-}
data Topology = Topology
  { name :: TopologyName
  , graph :: Graph
  , type_ :: TopologyType
  }
  deriving (Show, Eq, Ord, Generic)

size :: Topology -> Int
size Topology{graph} = snd (Array.bounds graph) + 1

{- | Checks whether a directed edge exists between two nodes in a topology. Its
computational complexity is @O(n)@, where @n@ is the number of neighbors of
the source node.
-}
hasEdge :: Topology -> Int -> Int -> Bool
hasEdge topology i j
  | i > snd (Array.bounds topology.graph) = False
  | otherwise = j `elem` (topology.graph Array.! i)

instance ToJSON Topology where
  toJSON Topology{name, graph, type_} =
    toJSON (name, Array.elems graph, type_)

instance FromJSON Topology where
  parseJSON v = do
    (name, edges, type_) <- parseJSON v
    let graph = A.listArray (0, L.length edges - 1) edges
    return Topology{name, graph, type_}

-- | Disambiguates between a selection of known topologies
data TopologyType
  = Diamond
  | Pendulum {length :: !Int, weight :: !Int}
  | Line {nodes :: !Int}
  | HyperCube {dimensions :: !Int}
  | Grid {rows :: !Int, cols :: !Int}
  | Torus2D {rows :: !Int, cols :: !Int}
  | Torus3D {rows :: !Int, cols :: !Int, planes :: !Int}
  | Tree {depth :: !Int, children :: !Int}
  | Star {nodes :: !Int}
  | Cycle {nodes :: !Int}
  | Complete {nodes :: !Int}
  | Dumbbell {width :: !Int, left :: !Int, right :: !Int}
  | Hourglass {nodes :: !Int}
  | Beads {count :: !Int, distance :: !Int, weight :: !Int}
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

fromEdgeList :: forall a. (Ord a) => [(a, a)] -> Graph
fromEdgeList edges = Graph.buildG (0, length vertices - 1) (nubOrd allEdges1)
 where
  allEdges0 = edges ++ fmap swap edges -- links are bidirectional in our setup
  vertices = nubOrd $ uncurry (<>) $ unzip edges
  vertexMap = M.fromList $ zip vertices [0 ..]
  allEdges1 = bimap (vertexMap M.!) (vertexMap M.!) <$> allEdges0

-- | @n@ nodes in a line, with a fully connected blob of @m@ nodes at one end.
pendulum :: Int -> Int -> Topology
pendulum length_ weight =
  (dumbbell length_ 0 weight)
    { name = "pendulum"
    , type_ = Pendulum length_ weight
    }

{- | @n@ nodes in a line, connected to their neighbors.

(differs from the
[mathematical terminology](https://mathworld.wolfram.com/LineGraph.html) but
conforms to callisto)
-}
line :: Int -> Topology
line nNodes = (dumbbell nNodes 0 0){name = "line", type_ = Line nNodes}

-- | @n@-dimensional hypercube
hypercube :: Int -> Topology
hypercube nDimensions =
  Topology
    { name = "hypercube"
    , graph = fromEdgeList es
    , type_ = HyperCube nDimensions
    }
 where
  k = (2 :: Int) ^ nDimensions
  es =
    -- see Callisto code (julia):
    -- https://github.com/bittide/Callisto.jl/blob/73d908c6cb02b9b953cc104e5b42d432efc42598/src/topology.jl#L224
    [ let j = i .|. (1 `shiftL` b) in (i + 1, j + 1)
    | i <- [0 .. (k - 1)]
    , b <- [0 .. (nDimensions - 1)]
    , i .&. (1 `shiftL` b) == 0
    ]

-- | Diamond graph
diamond :: Topology
diamond =
  Topology
    { name = "diamond"
    , type_ = Diamond
    , graph = A.listArray (0, 3) [[1, 3], [0, 2, 3], [1, 3], [0, 1, 2]]
    }

-- | Three dimensional torus.
torus3d :: Int -> Int -> Int -> Topology
torus3d nRows nCols nPlanes =
  Topology
    { name = "torus3d"
    , graph = fromEdgeList dirEdges
    , type_ = Torus3D nRows nCols nPlanes
    }
 where
  pairs =
    [ (l, m, n)
    | l <- [0 .. (nRows - 1)]
    , m <- [0 .. (nCols - 1)]
    , n <- [0 .. (nPlanes - 1)]
    ]
  neighborsOf (l, m, n) =
    [ ((l - 1) `mod` nRows, m, n)
    , ((l + 1) `mod` nRows, m, n)
    , (l, (m - 1) `mod` nCols, n)
    , (l, (m + 1) `mod` nCols, n)
    , (l, m, (n - 1) `mod` nPlanes)
    , (l, m, (n + 1) `mod` nPlanes)
    ]
  dirEdges = concatMap (\p -> fmap (p,) (neighborsOf p)) pairs

-- | See [this figure](https://www.researchgate.net/figure/The-two-dimensional-torus-4x4_fig1_221134153)
torus2d :: Int -> Int -> Topology
torus2d nRows nCols =
  Topology
    { name = "torus2d"
    , graph = fromEdgeList dirEdges
    , type_ = Torus2D nRows nCols
    }
 where
  pairs = [(m, n) | m <- [0 .. (nRows - 1)], n <- [0 .. (nCols - 1)]]
  neighborsOf (m, n) =
    [ ((m - 1) `mod` nRows, n)
    , ((m + 1) `mod` nRows, n)
    , (m, (n - 1) `mod` nCols)
    , (m, (n + 1) `mod` nCols)
    ]
  dirEdges = concatMap (\p -> fmap (p,) (neighborsOf p)) pairs

-- | [Grid graph](https://mathworld.wolfram.com/GridGraph.html)
grid :: Int -> Int -> Topology
grid nRows nCols =
  Topology
    { name = "grid"
    , graph = fromEdgeList dirEdges
    , type_ = Grid nRows nCols
    }
 where
  pairs = [(m, n) | m <- [1 .. nRows], n <- [1 .. nCols]]
  mkEdges (m, n) =
    [ (a, b)
    | a <- [(m - 1) .. (m + 1)]
    , b <- [(n - 1) .. (n + 1)]
    , a /= m || b /= n
    , a == m || b == n
    , a > 0
    , b > 0
    , a <= nRows
    , b <= nCols
    ]
  dirEdges = concatMap (\p -> fmap (p,) (mkEdges p)) pairs

-- | Tree of depth @depth@ with @childs@ children
tree :: Int -> Int -> Topology
tree depth nChildren =
  Topology
    { name = "tree"
    , graph = treeGraph
    , type_ = Tree depth nChildren
    }
 where
  -- \| At depth @d_i@, child node @i@ is connected to the @(i-1) `div` c + 1@st
  -- node at depth @d_i - 1@
  pairs =
    [ (d_i, i, (i - 1) `div` nChildren + 1)
    | d_i <- [0 .. depth]
    , i <- [1 .. (nChildren ^ d_i)]
    ]
  mkEdges (0, _, _) = Nothing
  mkEdges (lvl, node, p_node) = Just ((lvl, node), (lvl - 1, p_node))
  directedEdges = mapMaybe mkEdges pairs
  treeGraph = fromEdgeList directedEdges

-- | [Star graph](https://mathworld.wolfram.com/StarGraph.html)
star :: Int -> Topology
star nNodes = (tree 1 nNodes){name = "star", type_ = Star nNodes}

{- | [Cyclic graph](https://mathworld.wolfram.com/CycleGraph.html) with @n@
vertices.
-}
cyclic :: Int -> Topology
cyclic nNodes = (beads nNodes 0 1){name = "cycle", type_ = Cycle nNodes}

{- | [Complete graph](https://mathworld.wolfram.com/CompleteGraph.html) with @n@
vertices.
-}
complete :: Int -> Topology
complete nNodes = (beads 1 0 nNodes){name = "complete", type_ = Complete nNodes}

{- | An dumbbell shaped graph consisting of two independent complete
sub-graphs of size @l@ and @r@, connected via a chain of @w@ nodes
in between two distinct nodes of each sub-graph.
-}
dumbbell :: Int -> Int -> Int -> Topology
dumbbell width nLeft nRight =
  Topology
    { name = "dumbbell"
    , graph = tGraph
    , type_ = Dumbbell width nLeft nRight
    }
 where
  m = width + nLeft + nRight - 1

  tGraph =
    if width + nLeft + nRight == 0
      then A.array (0, -1) []
      else A.array (0, m) $ fmap (\i -> (i, neighbors i)) [0 .. m]

  neighbors i
    | i < nLeft =
        (if i == nLeft - 1 && width + nRight > 0 then (nLeft :) else id)
          [j | j <- [0 .. nLeft - 1], j /= i]
    | i >= width + nLeft =
        (if i == width + nLeft && width + nLeft > 0 then ((width + nLeft - 1) :) else id)
          [j + width + nLeft | j <- [0 .. nRight - 1], j + width + nLeft /= i]
    | otherwise =
        (if nLeft > 0 || i > nLeft then ((i - 1) :) else id) $
          (if nRight > 0 || i < nLeft + width - 1 then ((i + 1) :) else id)
            []

{- | An hourglass shaped graph consisting of two independent complete
sub-graphs, only connected via a single edge between two distinct
nodes of each sub-graph.
-}
hourglass :: Int -> Topology
hourglass nNodes =
  (dumbbell 0 nNodes nNodes){name = "hourglass", type_ = Hourglass nNodes}

{- | A beads shaped graph consisting of two @count@ independent complete
subgraphs of size @weight@ (representing the beads), connected via a
closed circular chain of @distance@ nodes in between (representing the
thread).
-}
beads :: Int -> Int -> Int -> Topology
beads count distance weight =
  Topology
    { name = "beads"
    , graph = fromEdgeList es
    , type_ = Beads{count, distance, weight}
    }
 where
  s = count * (distance + weight)
  es =
    [ (x + i, x + j)
    | n <- [0 .. count - 1]
    , i <- [0 .. weight - 1]
    , j <- [0 .. weight - 1]
    , i /= j
    , let x = n * (distance + weight)
    ]
      <> [ (x', x)
         | weight > 0
         , n <- [0 .. count - 1]
         , let x' = n * (distance + weight)
               x = (x' - 1) `mod` s
         , x /= x'
         ]
      <> [ (x', x)
         | n <- [0 .. count - 1]
         , i <- [0 .. distance - 1]
         , let x' = n * (distance + weight) + weight + i
               x = (x' - 1) `mod` s
         , x /= x'
         ]

-- | Turns a 'Topology' into a graphviz DOT structure
toDot :: Topology -> String
toDot topology =
  [I.i|
    graph #{tName} {
      #{renderedEdges}
    }
  |]
 where
  tName = topology.name -- XXX: Doesn't work in the quasiquoter?
  renderEdge (i, j) = [I.i|#{i} -- #{j};|]
  renderedEdges = L.intercalate "\n" (renderEdge <$> uniEdges)
  uniEdges = [(i, j) | (i, j) <- Graph.edges topology.graph, i < j]

{- | Successive overlapping pairs.

>>> pairwise [1, 2, 3, 4]
[(1,2),(2,3),(3,4)]
>>> pairwise []
[]
-}
pairwise :: [a] -> [(a, a)]
pairwise as = zip as (drop 1 as)
