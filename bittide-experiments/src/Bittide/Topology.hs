-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bittide.Topology (
  -- * Data Types
  TopologyType (..),
  TopologyName,
  Topology (
    topologyName,
    topologyGraph,
    topologyType,
    hasEdge
  ),
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
  froccTopologyType,
  fromDot,
  toDot,
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
  SomeNat (..),
  d0,
  d1,
  natToNum,
  snatProxy,
  snatToNum,
  someNatVal,
  type Div,
  type (*),
  type (+),
  type (-),
  type (^),
 )

import Control.Monad (forM, forM_, replicateM, replicateM_, unless, when)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Array.IO (IOUArray)
import Data.Array.MArray (freeze, getElems, newListArray, readArray, writeArray)
import Data.Bifunctor (bimap, first)
import Data.Bits (Bits (..))
import Data.Containers.ListUtils (nubOrd)
import Data.Function (on)
import Data.Graph (Graph, buildG, edges, graphFromEdges, scc)
import Data.List (groupBy, sort)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Tuple (swap)
import GHC.Num.Natural (Natural)
import GHC.TypeLits.KnownNat (KnownNat2 (..), SNatKn (..), nameToSymbol)
import GHC.TypeNats (natVal)
import Language.Dot.Graph (GraphType (..), Name (..), Statement (..), Subgraph (..))
import Language.Dot.Parser (parse)
import System.Random (randomIO, randomRIO)

import qualified Data.Array as A (accumArray, array, listArray, (!))
import qualified Data.Map.Strict as M (fromList, (!))
import qualified Data.Set as S (fromList, toList)
import qualified GHC.TypeNats as Nats

-- | Special topologies may have names given as a string.
type TopologyName = String

{- | A topology is just a simple graph extended with a type level size
bound, a name and a 'TopologyType'.
-}
data Topology (n :: Nat) = Topology
  { topologyName :: TopologyName
  , topologyGraph :: Graph
  , topologyType :: TopologyType
  , hasEdge :: Index n -> Index n -> Bool
  }

-- | Existentially quantified version hiding the type level bound.
data STopology = forall n. (KnownNat n) => STopology (Topology n)

-- | Smart constructor of 'Topology'.
fromGraph :: forall n. (KnownNat n) => TopologyName -> Graph -> Topology n
fromGraph name graph =
  Topology name graph (Random $ natToNum @n) $
    curry $
      (A.!) $
        A.accumArray (const id) False bounds $
          zip (filter (uncurry (/=)) edgeIndices) [True, True ..]
 where
  bounds = ((minBound, minBound), (maxBound, maxBound))
  edgeIndices = map (bimap fromIntegral fromIntegral) $ edges graph

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
  | DotFile {filepath :: FilePath}
  | Random {nodes :: Natural}
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data SomeSNat where
  SomeSNat :: (KnownNat n) => SNat n -> SomeSNat

-- | Generates some topology of the given topology type, if possible.
froccTopologyType ::
  TopologyType ->
  IO (Either String STopology)
froccTopologyType = \case
  Diamond -> ret diamond
  Pendulum l w ->
    case (naturalToSomeSNat l, naturalToSomeSNat w) of
      (SomeSNat sL, SomeSNat sW) ->
        ret (pendulum sL sW)
  Line n ->
    case naturalToSomeSNat n of
      SomeSNat sN -> ret (line sN)
  HyperCube n ->
    case naturalToSomeSNat n of
      SomeSNat sN -> ret (hypercube sN)
  Grid rows cols ->
    case (naturalToSomeSNat rows, naturalToSomeSNat cols) of
      (SomeSNat sRows, SomeSNat sCols) -> ret (grid sRows sCols)
  Torus2D rows cols ->
    case (naturalToSomeSNat rows, naturalToSomeSNat cols) of
      (SomeSNat sRows, SomeSNat sCols) -> ret (torus2d sRows sCols)
  Torus3D rows cols planes ->
    case (naturalToSomeSNat rows, naturalToSomeSNat cols, naturalToSomeSNat planes) of
      (SomeSNat sRows, SomeSNat sCols, SomeSNat sPlanes) ->
        ret (torus3d sRows sCols sPlanes)
  Tree depth childs ->
    case (naturalToSomeSNat depth, naturalToSomeSNat childs) of
      (SomeSNat sDepth, SomeSNat sChilds) -> ret (tree sDepth sChilds)
  Star n ->
    case naturalToSomeSNat n of
      SomeSNat sN -> ret (star sN)
  Cycle n ->
    case naturalToSomeSNat n of
      SomeSNat sN -> ret (cyclic sN)
  Complete n ->
    case naturalToSomeSNat n of
      SomeSNat sN -> ret (complete sN)
  Dumbbell w l r ->
    case (naturalToSomeSNat w, naturalToSomeSNat l, naturalToSomeSNat r) of
      (SomeSNat sW, SomeSNat sL, SomeSNat sR) -> ret (dumbbell sW sL sR)
  Hourglass n ->
    case naturalToSomeSNat n of
      SomeSNat sN -> ret (hourglass sN)
  Beads c d w ->
    case (naturalToSomeSNat c, naturalToSomeSNat d, naturalToSomeSNat w) of
      (SomeSNat sC, SomeSNat sD, SomeSNat sW) -> ret (beads sC sD sW)
  Random n ->
    case naturalToSomeSNat n of
      SomeSNat sN -> do
        topology <- randomTopology sN
        ret topology
  DotFile f -> do
    contents <- readFile f
    pure $ first (("Invalid DOT file - " <> f <> "\n") <>) $ fromDot $ contents
 where
  ret :: forall (n :: Nat). (KnownNat n) => Topology n -> IO (Either String STopology)
  ret = pure . pure . STopology

  naturalToSomeSNat :: Natural -> SomeSNat
  naturalToSomeSNat n =
    case Nats.someNatVal n of
      Nats.SomeNat sn -> SomeSNat (snatProxy sn)

-- | Given a list of edges, turn it into a directed graph.
fromEdgeList :: forall a. (Ord a) => [(a, a)] -> Graph
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
  g ps@((x, _) : _) = (x, snd <$> ps)
  g [] = error "No edges."
  (dirGraph, _, _) =
    graphFromEdges ((\(key, keys) -> ((), key, keys)) <$> adjList)

-- | @n@ nodes in a line, with a fully connected blob of @m@ nodes at one end.
pendulum :: SNat l -> SNat w -> Topology (l + w)
pendulum sl sw =
  (dumbbell sl d0 sw)
    { topologyName = "pendulum"
    , topologyType = Pendulum (snatToNum sl) (snatToNum sw)
    }

{- | @n@ nodes in a line, connected to their neighbors.

(differs from the
[mathematical terminology](https://mathworld.wolfram.com/LineGraph.html) but
conforms to callisto)
-}
line :: SNat n -> Topology n
line sn =
  (dumbbell sn d0 d0)
    { topologyName = "line"
    , topologyType = Line $ snatToNum sn
    }

-- | @n@-dimensional hypercube
hypercube :: SNat n -> Topology (2 ^ n)
hypercube sn@SNat =
  (fromGraph "hypercube" $ fromEdgeList es)
    { topologyType = HyperCube $ snatToNum sn
    }
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
  (fromGraph "diamond" $ A.listArray (0, 3) [[1, 3], [0, 2, 3], [1, 3], [0, 1, 2]])
    { topologyType = Diamond
    }

-- | Three dimensional torus.
torus3d :: SNat a -> SNat b -> SNat c -> Topology (a * b * c)
torus3d sna@SNat snb@SNat snc@SNat =
  (fromGraph "torus3d" $ fromEdgeList dirEdges)
    { topologyType = Torus3D a b c
    }
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
  (fromGraph "torus2d" $ fromEdgeList dirEdges)
    { topologyType = Torus2D rows cols
    }
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
  (fromGraph "grid" $ fromEdgeList dirEdges)
    { topologyType = Grid rows cols
    }
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
  (fromGraph "tree" treeGraph)
    { topologyType = Tree depth c
    }
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
star :: SNat childs -> Topology (TreeSize 1 childs)
star sn =
  (tree SNat sn)
    { topologyName = "star"
    , topologyType = Star $ snatToNum sn
    }

{- | [Cyclic graph](https://mathworld.wolfram.com/CycleGraph.html) with @n@
vertices.
-}
cyclic :: SNat n -> Topology n
cyclic sn =
  (beads sn d0 d1)
    { topologyName = "cycle"
    , topologyType = Cycle $ snatToNum sn
    }

{- | [Complete graph](https://mathworld.wolfram.com/CompleteGraph.html) with @n@
vertices.
-}
complete :: SNat n -> Topology n
complete sn =
  (beads d1 d0 sn)
    { topologyName = "complete"
    , topologyType = Complete $ snatToNum sn
    }

{- | An dumbbell shaped graph consisting of two independent complete
sub-graphs of size @l@ and @r@, connected via a chain of @w@ nodes
in between two distinct nodes of each sub-graph.
-}
dumbbell :: SNat w -> SNat l -> SNat r -> Topology (w + l + r)
dumbbell sw@SNat sl@SNat sr@SNat =
  t{topologyType = Dumbbell (sn2i sw) (sn2i sl) (sn2i sr)}
 where
  sn2i = snatToNum
  w = snatToNum sw
  l = snatToNum sl
  r = snatToNum sr
  m = w + l + r - 1

  t =
    fromGraph "dumbbell" $
      if w + l + r == 0
        then A.array (0, -1) []
        else A.array (0, m) $ fmap (\i -> (i, neighbours i)) [0 .. m]

  neighbours i
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
hourglass :: SNat n -> Topology (n + n)
hourglass sn =
  (dumbbell d0 sn sn)
    { topologyName = "hourglass"
    , topologyType = Hourglass $ snatToNum sn
    }

{- | A beads shaped graph consisting of two @c@ independend complete
subgraphs of size @w@ (representing the beads), connected via a
closed circular chain of @d@ nodes in between (representing the
thread).
-}
beads :: SNat c -> SNat d -> SNat w -> Topology (c * (d + w))
beads sc@SNat sd@SNat sw@SNat =
  (fromGraph "beads" $ fromEdgeList es)
    { topologyType = Beads (sn2i sc) (sn2i sd) (sn2i sw)
    }
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
    fromGraph "random" $
      buildG
        (0, n - 1)
        [ (i, j)
        | i <- is
        , j <- is
        , available A.! (i, j)
        ]

{- | Turns a topology into a graphviz DOT structure, as it is required by
the [happy-dot](https://hackage.haskell.org/package/happy-dot) library.
-}
toDot ::
  (KnownNat n) =>
  -- | topology to be turned into graphviz dot
  Topology n ->
  -- | the result, as it is needed by happy-dot
  (Bool, GraphType, Maybe Name, [Statement])
toDot t =
  ( True
  , Graph
  , Just $ StringID $ t.topologyName
  , map asEdgeStatement $ edges $ t.topologyGraph
  )
 where
  asEdgeStatement (x, y) =
    EdgeStatement
      (map (\i -> NodeRef (XMLID ('n' : show i)) Nothing) [x, y])
      []

{- | Reads a topology from a DOT file. Only the name and structure of the
graph is used, i.e., any additional graphviz dot specific
annotations are ignored.
-}
fromDot ::
  -- | the string holding the graphviz dot content
  String ->
  -- | either an error, if given an unusable input, or the extracted topology
  Either String STopology
fromDot cnt = do
  (strict, gType, maybe "" fromDotName -> name, statements) <- parse cnt

  unless strict $ Left "Graph must be strict"
  when (gType == Digraph) $ Left "Graph must be undirected"

  edgeChains <- catMaybes <$> mapM fromStatement statements

  let
    namedEdges = concatMap (pairwise . map asString) edgeChains
    edgeNames = dedupAndSort $ concatMap (\(x, y) -> [x, y]) namedEdges
    n = length edgeNames
    idx = (M.!) $ M.fromList $ zip edgeNames [0, 1 ..]
    graph =
      buildG (0, n - 1)
      -- Self loops are removed at this point as they are redundant in
      -- terms of the simulated topology. In terms of connectivity, a
      -- node can synchronize its clock with itself even without a
      -- elastic buffer in between. Therefore, we all self-loops in
      -- the input, but remove them here, since they don't have any
      -- effect on the topology that is created out of the graph.
      $
        filter (uncurry (/=))
        -- remove duplicate edges and sort for pretty printing
        $
          dedupAndSort $
            concatMap
              (\(x, y) -> [(idx x, idx y), (idx y, idx x)])
              namedEdges

  when (length (scc graph) > 1) $ Left "Graph must be strongly connected"

  case someNatVal $ toInteger n of
    Nothing -> Left "cannot construct SNat arguments"
    Just (SomeNat (_ :: Proxy n)) ->
      return $ STopology $ fromGraph @n name graph
 where
  fromStatement :: Statement -> Either String (Maybe [Name])
  fromStatement = \case
    EdgeStatement xs _ -> fmap Just $ forM xs $ \case
      NodeRef n _ -> return n
      Subgraph{} -> Left "Subgraphs are not supported"
    -- we are only interested in the edges, everthing else can be
    -- ignored
    _ -> pure Nothing

  dedupAndSort :: (Ord a) => [a] -> [a]
  dedupAndSort = S.toList . S.fromList

  asString = \case
    StringID x -> 's' : x
    XMLID x -> 'x' : x

  fromDotName = \case
    StringID x -> x
    XMLID x -> x

{- | Successive overlapping pairs.

>>> pairwise [1, 2, 3, 4]
[(1,2),(2,3),(3,4)]
>>> pairwise []
[]
-}
pairwise :: [a] -> [(a, a)]
pairwise as = zip as (drop 1 as)
