-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bittide.Topology (
  -- * Data Types
  TopologyType (..),
  TopologyName,
  Topology (..),
  STopology (..),
  TreeSize,

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
  fromGraph,
  hasEdge,
  toDot,
  toDotS,
  randomTopology,
  pairwise,
) where

import Prelude

import Clash.Prelude (
  Generic,
  Index,
  KnownNat,
  Nat,
  SNat (..),
  d0,
  d1,
  natToInteger,
  snatToNum,
  type Div,
  type (*),
  type (+),
  type (-),
  type (^),
 )

import Control.Monad (forM_, replicateM, replicateM_, when)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Array.IO (IOUArray)
import Data.Array.MArray (freeze, getElems, newListArray, readArray, writeArray)
import Data.Bifunctor (bimap)
import Data.Bits (Bits (..))
import Data.Containers.ListUtils (nubOrd)
import Data.Data ((:~:) (..))
import Data.Graph (Graph)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Tuple (swap)
import GHC.Num.Natural (Natural)
import GHC.TypeLits.KnownNat (KnownNat2 (..), SNatKn (..), nameToSymbol)
import GHC.TypeNats (natVal)
import System.Random (randomIO, randomRIO)

import qualified Clash.Util.Interpolate as I
import qualified Data.Array as A (array, listArray, (!))
import qualified Data.Array as Array
import qualified Data.Graph as Graph
import qualified Data.List as L
import qualified Data.Map.Strict as M (fromList, (!))
import qualified GHC.TypeNats as Nats

-- | Special topologies may have names given as a string.
type TopologyName = String

{- | A topology is just a simple graph extended with a type level size
bound, a name and a 'TopologyType'.
-}
data Topology (n :: Nat) = Topology
  { name :: TopologyName
  , graph :: Graph
  , type_ :: TopologyType
  }
  deriving (Show, Eq, Ord, Generic)

{- | Checks whether a directed edge exists between two nodes in a topology. Its
computational complexity is @O(n)@, where @n@ is the number of neighbors of
the source node.
-}
hasEdge :: (KnownNat n) => Topology n -> Index n -> Index n -> Bool
hasEdge topology i j = fromIntegral j `elem` (topology.graph Array.! fromIntegral i)

instance ToJSON (Topology n) where
  toJSON Topology{name, graph, type_} =
    toJSON (name, Array.elems graph, type_)

-- | Existentially quantified version hiding the type level bound.
data STopology where
  STopology :: (KnownNat n) => Topology n -> STopology

instance Show STopology where
  show (STopology (topology :: Topology n)) =
    "STopology @" ++ show (natToInteger @n) ++ " (" ++ show topology ++ ")"

instance Eq STopology where
  (STopology (t1 :: Topology n)) == (STopology (t2 :: Topology m)) =
    case Nats.sameNat (Proxy @n) (Proxy @m) of
      Nothing -> False
      Just Refl -> t1 == t2

instance Ord STopology where
  compare (STopology (t1 :: Topology n)) (STopology (t2 :: Topology m)) =
    case Nats.sameNat (Proxy @n) (Proxy @m) of
      Nothing -> compare (natToInteger @n) (natToInteger @m)
      Just Refl -> compare t1 t2

instance ToJSON STopology where
  toJSON (STopology topology) = toJSON topology

instance FromJSON STopology where
  parseJSON v = do
    (name, elems, type_) <- parseJSON v
    let n = length elems
    case Nats.someNatVal (fromIntegral n) of
      Nats.SomeNat (_ :: Proxy n) ->
        pure $ STopology @n $ fromGraph name type_ (Array.listArray (0, n - 1) elems)

-- | Smart constructor of 'Topology'.
fromGraph :: forall n. (KnownNat n) => TopologyName -> TopologyType -> Graph -> Topology n
fromGraph name type_ graph = Topology{name, graph, type_}

{- | Disambiguates between a selection of known topologies, topologies
that are loaded from DOT files, and random topologies.
-}
data TopologyType
  = Diamond
  | Pendulum {length :: Natural, weight :: Natural}
  | Line {nodes :: Natural}
  | HyperCube {dimensions :: Natural}
  | Grid {rows :: Natural, cols :: Natural}
  | Torus2D {rows :: Natural, cols :: Natural}
  | Torus3D {rows :: Natural, cols :: Natural, planes :: Natural}
  | Tree {depth :: Natural, children :: Natural}
  | Star {nodes :: Natural}
  | Cycle {nodes :: Natural}
  | Complete {nodes :: Natural}
  | Dumbbell {width :: Natural, left :: Natural, right :: Natural}
  | Hourglass {nodes :: Natural}
  | Beads {count :: Natural, distance :: Natural, weight :: Natural}
  | JsonFile {path :: FilePath}
  | Random {nodes :: Natural}
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

fromEdgeList :: forall a. (Ord a) => [(a, a)] -> Graph
fromEdgeList edges = Graph.buildG (0, length vertices - 1) (nubOrd allEdges1)
 where
  allEdges0 = edges ++ fmap swap edges -- links are bidirectional in our setup
  vertices = nubOrd $ uncurry (<>) $ unzip edges
  vertexMap = M.fromList $ zip vertices [0 ..]
  allEdges1 = bimap (vertexMap M.!) (vertexMap M.!) <$> allEdges0

-- | @n@ nodes in a line, with a fully connected blob of @m@ nodes at one end.
pendulum :: (KnownNat l, KnownNat w) => SNat l -> SNat w -> Topology (l + w)
pendulum sl sw =
  let Topology{graph = g} = dumbbell sl d0 sw
   in fromGraph "pendulum" (Pendulum (snatToNum sl) (snatToNum sw)) g

{- | @n@ nodes in a line, connected to their neighbors.

(differs from the
[mathematical terminology](https://mathworld.wolfram.com/LineGraph.html) but
conforms to callisto)
-}
line :: (KnownNat n) => SNat n -> Topology n
line sn =
  let Topology{graph = g} = dumbbell sn d0 d0
   in fromGraph "line" (Line $ snatToNum sn) g

-- | @n@-dimensional hypercube
hypercube :: SNat n -> Topology (2 ^ n)
hypercube sn@SNat =
  fromGraph "hypercube" (HyperCube $ snatToNum sn) (fromEdgeList es)
 where
  n = snatToNum sn
  k = (2 :: Int) ^ n
  es =
    -- see Callisto code (julia):
    -- https://github.com/bittide/Callisto.jl/blob/73d908c6cb02b9b953cc104e5b42d432efc42598/src/topology.jl#L224
    [ let j = i .|. (1 `shiftL` b) in (i + 1, j + 1)
    | i <- [0 .. (k - 1)]
    , b <- [0 .. (n - 1)]
    , i .&. (1 `shiftL` b) == 0
    ]

-- | Diamond graph
diamond :: Topology 4
diamond =
  fromGraph "diamond" Diamond (A.listArray (0, 3) [[1, 3], [0, 2, 3], [1, 3], [0, 1, 2]])

-- | Three dimensional torus.
torus3d :: SNat a -> SNat b -> SNat c -> Topology (a * b * c)
torus3d sna@SNat snb@SNat snc@SNat =
  fromGraph "torus3d" (Torus3D a b c) (fromEdgeList dirEdges)
 where
  a = snatToNum sna
  b = snatToNum snb
  c = snatToNum snc
  pairs = [(l, m, n) | l <- [0 .. (a - 1)], m <- [0 .. (b - 1)], n <- [0 .. (c - 1)]]
  neighborsOf (l, m, n) =
    [ ((l - 1) `mod` a, m, n)
    , ((l + 1) `mod` a, m, n)
    , (l, (m - 1) `mod` b, n)
    , (l, (m + 1) `mod` b, n)
    , (l, m, (n - 1) `mod` c)
    , (l, m, (n + 1) `mod` c)
    ]
  dirEdges = concatMap (\p -> fmap (p,) (neighborsOf p)) pairs

-- | See [this figure](https://www.researchgate.net/figure/The-two-dimensional-torus-4x4_fig1_221134153)
torus2d :: SNat rows -> SNat cols -> Topology (rows * cols)
torus2d snRows@SNat snCols@SNat =
  fromGraph "torus2d" (Torus2D rows cols) (fromEdgeList dirEdges)
 where
  rows = snatToNum snRows
  cols = snatToNum snCols
  pairs = [(m, n) | m <- [0 .. (rows - 1)], n <- [0 .. (cols - 1)]]
  neighborsOf (m, n) =
    [ ((m - 1) `mod` rows, n)
    , ((m + 1) `mod` rows, n)
    , (m, (n - 1) `mod` cols)
    , (m, (n + 1) `mod` cols)
    ]
  dirEdges = concatMap (\p -> fmap (p,) (neighborsOf p)) pairs

-- | [Grid graph](https://mathworld.wolfram.com/GridGraph.html)
grid :: SNat rows -> SNat cols -> Topology (rows * cols)
grid snRows@SNat snCols@SNat =
  fromGraph "grid" (Grid rows cols) (fromEdgeList dirEdges)
 where
  rows = snatToNum snRows
  cols = snatToNum snCols
  pairs = [(m, n) | m <- [1 .. rows], n <- [1 .. cols]]
  mkEdges (m, n) =
    [ (a, b)
    | a <- [(m - 1) .. (m + 1)]
    , b <- [(n - 1) .. (n + 1)]
    , a /= m || b /= n
    , a == m || b == n
    , a > 0
    , b > 0
    , a <= rows
    , b <= cols
    ]
  dirEdges = concatMap (\p -> fmap (p,) (mkEdges p)) pairs

-- | Type family for calculating the size of a tree.
type family TreeSize (depth :: Nat) (children :: Nat) where
  TreeSize depth 0 = 1
  TreeSize depth 1 = depth + 1
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
tree :: SNat depth -> SNat childs -> Topology (TreeSize depth childs)
tree snDepth@SNat snChilds@SNat =
  fromGraph "tree" (Tree depth c) treeGraph
 where
  depth = snatToNum snDepth
  c = snatToNum snChilds
  -- \| At depth @d_i@, child node @i@ is connected to the @(i-1) `div` c + 1@st
  -- node at depth @d_i - 1@
  pairs = [(d_i, i, (i - 1) `div` c + 1) | d_i <- [0 .. depth], i <- [1 .. (c ^ d_i)]]
  mkEdges (0, _, _) = Nothing
  mkEdges (lvl, node, p_node) = Just ((lvl, node), (lvl - 1, p_node))
  directedEdges = mapMaybe mkEdges pairs
  treeGraph = fromEdgeList directedEdges

-- | [Star graph](https://mathworld.wolfram.com/StarGraph.html)
star :: (KnownNat childs) => SNat childs -> Topology (TreeSize 1 childs)
star sn =
  let Topology{graph = g} = tree (SNat @1) sn
   in fromGraph "star" (Star $ snatToNum sn) g

{- | [Cyclic graph](https://mathworld.wolfram.com/CycleGraph.html) with @n@
vertices.
-}
cyclic :: (KnownNat n) => SNat n -> Topology n
cyclic sn =
  let Topology{graph = g} = beads sn d0 d1
   in fromGraph "cycle" (Cycle $ snatToNum sn) g

{- | [Complete graph](https://mathworld.wolfram.com/CompleteGraph.html) with @n@
vertices.
-}
complete :: (KnownNat n) => SNat n -> Topology n
complete sn =
  let Topology{graph = g} = beads d1 d0 sn
   in fromGraph "complete" (Complete $ snatToNum sn) g

{- | An dumbbell shaped graph consisting of two independent complete
sub-graphs of size @l@ and @r@, connected via a chain of @w@ nodes
in between two distinct nodes of each sub-graph.
-}
dumbbell :: SNat w -> SNat l -> SNat r -> Topology (w + l + r)
dumbbell sw@SNat sl@SNat sr@SNat =
  fromGraph "dumbbell" (Dumbbell (sn2i sw) (sn2i sl) (sn2i sr)) tGraph
 where
  sn2i = snatToNum
  w = snatToNum sw
  l = snatToNum sl
  r = snatToNum sr
  m = w + l + r - 1

  tGraph =
    if w + l + r == 0
      then A.array (0, -1) []
      else A.array (0, m) $ fmap (\i -> (i, neighbors i)) [0 .. m]

  neighbors i
    | i < l =
        (if i == l - 1 && w + r > 0 then (l :) else id)
          [j | j <- [0 .. l - 1], j /= i]
    | i >= w + l =
        (if i == w + l && w + l > 0 then ((w + l - 1) :) else id)
          [j + w + l | j <- [0 .. r - 1], j + w + l /= i]
    | otherwise =
        (if l > 0 || i > l then ((i - 1) :) else id) $
          (if r > 0 || i < l + w - 1 then ((i + 1) :) else id)
            []

{- | An hourglass shaped graph consisting of two independent complete
sub-graphs, only connected via a single edge between two distinct
nodes of each sub-graph.
-}
hourglass :: (KnownNat n) => SNat n -> Topology (n + n)
hourglass sn =
  let Topology{graph = g} = dumbbell d0 sn sn
   in fromGraph "hourglass" (Hourglass $ snatToNum sn) g

{- | A beads shaped graph consisting of two @c@ independent complete
subgraphs of size @w@ (representing the beads), connected via a
closed circular chain of @d@ nodes in between (representing the
thread).
-}
beads :: SNat c -> SNat d -> SNat w -> Topology (c * (d + w))
beads sc@SNat sd@SNat sw@SNat =
  fromGraph "beads" (Beads (sn2i sc) (sn2i sd) (sn2i sw)) (fromEdgeList es)
 where
  sn2i = snatToNum
  c = snatToNum sc :: Int
  d = snatToNum sd :: Int
  w = snatToNum sw :: Int
  s = c * (d + w)
  es =
    [ (x + i, x + j)
    | n <- [0 .. c - 1]
    , i <- [0 .. w - 1]
    , j <- [0 .. w - 1]
    , i /= j
    , let x = n * (d + w)
    ]
      <> [ (x', x)
         | w > 0
         , n <- [0 .. c - 1]
         , let x' = n * (d + w)
               x = (x' - 1) `mod` s
         , x /= x'
         ]
      <> [ (x', x)
         | n <- [0 .. c - 1]
         , i <- [0 .. d - 1]
         , let x' = n * (d + w) + w + i
               x = (x' - 1) `mod` s
         , x /= x'
         ]

-- | Generates a random topology of the given size.
randomTopology :: SNat n -> IO (Topology n)
randomTopology sn@SNat = do
  let
    n = snatToNum sn
    is = [0, 1 .. n - 1]

  -- get some random vertex permuation for ensuring connectivity
  aP <- newListArray (0, n - 1) is
  replicateM_ (10 * n) $ do
    p1 <- randomRIO (0, n - 1)
    p2 <- randomRIO (0, n - 1)
    v1 <- readArray aP p1
    v2 <- readArray aP p2
    writeArray aP p1 v2
    writeArray aP p2 v1

  randomPermutation <- getElems (aP :: IOUArray Int Int)

  -- get some random edge availability matrix for self-loop-free,
  -- undirected graphs
  aE <- replicateM (n * n) randomIO >>= newListArray ((0, 0), (n - 1, n - 1))
  forM_ is $ \i ->
    writeArray aE (i, i) False
  forM_ [(i, j) | i <- is, j <- is, i /= j] $ \(i, j) -> do
    x1 <- readArray aE (i, j)
    x2 <- readArray aE (j, i)
    when (x1 /= x2) $ do
      writeArray aE (i, j) False
      writeArray aE (j, i) False

  -- ensure connectivity
  forM_ (pairwise randomPermutation) $ \(i, j) -> do
    writeArray aE (i, j) True
    writeArray aE (j, i) True

  available <- freeze (aE :: IOUArray (Int, Int) Bool)

  -- create the graph
  return $
    fromGraph "random" (Random (fromIntegral n)) $
      Graph.buildG
        (0, n - 1)
        [ (i, j)
        | i <- is
        , j <- is
        , available A.! (i, j)
        ]

-- | Turns a 'Topology' into a graphviz DOT structure
toDot :: (KnownNat n) => Topology n -> String
toDot topology =
  [I.i|
    graph #{tName} {
      #{renderedEdges}
    }
  |]
 where
  tName = topology.name -- XXX: Doesn't work in the quasiquoter?
  renderEdge (i, j) = [I.i|n#{i} -- n#{j};|]
  renderedEdges = L.intercalate "\n" (renderEdge <$> uniEdges)
  uniEdges = [(i, j) | (i, j) <- Graph.edges topology.graph, i < j]

-- | Turns an 'STopology' into a graphviz DOT structure
toDotS :: STopology -> String
toDotS (STopology topology) = toDot topology

{- | Successive overlapping pairs.

>>> pairwise [1, 2, 3, 4]
[(1,2),(2,3),(3,4)]
>>> pairwise []
[]
-}
pairwise :: [a] -> [(a, a)]
pairwise as = zip as (drop 1 as)
