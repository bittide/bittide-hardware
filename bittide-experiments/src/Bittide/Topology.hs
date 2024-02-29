-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bittide.Topology
  ( -- * Data Types
    Topology(..)
  , Graph
      ( unboundGraph
      , hasEdge
      )
  , boundGraph
    -- * Special Topologies
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
  , hourglass
    -- * Utilities
  , topologyName
  , topologyParser
  , randomGraph
  , toDot
  , fromDot
  , pairwise
  ) where

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

import Control.Monad (forM, forM_, when, replicateM, replicateM_)

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.=), (.:), object)
import Data.Aeson.Types (typeMismatch)
import Data.Array.IO (IOUArray)
import Data.Array.MArray (newListArray, readArray, writeArray, getElems, freeze)
import Data.Bifunctor (bimap)
import Data.Bits (Bits (..))
import Data.Containers.ListUtils (nubOrd)
import Data.Function (on)
import Data.Graph (graphFromEdges, edges, buildG, scc)
import Data.List (groupBy, sort)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Proxy (Proxy(..))
import Data.Tuple (swap)

import Data.Array      qualified as A ((!), array, listArray, accumArray)
import Data.Graph      qualified as G (Graph)
import Data.Map.Strict qualified as M ((!), fromList)
import Data.Set        qualified as S (fromList, toList)

import GHC.TypeNats (natVal)
import GHC.Num.Natural(Natural)
import GHC.TypeLits.KnownNat (KnownNat2(..),SNatKn(..), nameToSymbol)

import Language.Dot.Graph (GraphType(..), Name(..), Statement(..), Subgraph(..))
import Language.Dot.Parser (parse)

import Options.Applicative
  ( Parser, action, auto, short, long, help, info, option, command
  , progDesc, strOption, metavar, hsubparser, commandGroup, footerDoc
  )
import Options.Applicative.Help.Pretty (pretty)

import System.Random (randomIO, randomRIO)

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

data Topology =
    Diamond
  | Line Int
  | HyperCube Int
  | Grid Int Int
  | Torus2D Int Int
  | Torus3D Int Int Int
  | Tree Int Int
  | Star Int
  | Cycle Int
  | Complete Int
  | Hourglass Int
  | DotFile FilePath
  | Random Int
  deriving (Show, Ord, Eq)

instance ToJSON Topology where
  toJSON t = object $ case t of
    Diamond       -> [ gt ]
    DotFile f     -> [ gt, "filepath"   .= f ]
    Line n        -> [ gt, "nodes"      .= n ]
    HyperCube n   -> [ gt, "dimensions" .= n ]
    Star n        -> [ gt, "nodes"      .= n ]
    Cycle n       -> [ gt, "nodes"      .= n ]
    Complete n    -> [ gt, "nodes"      .= n ]
    Hourglass n   -> [ gt, "nodes"      .= n ]
    Random n      -> [ gt, "nodes"      .= n ]
    Tree d c      -> [ gt, "depth"      .= d, "childs" .= c ]
    Grid r c      -> [ gt, "rows"       .= r, "cols"   .= c ]
    Torus2D r c   -> [ gt, "rows"       .= r, "cols"   .= c ]
    Torus3D r c p -> [ gt, "rows"       .= r, "cols"   .= c, "planes" .= p ]
   where
    gt = "graph" .= topologyName t

instance FromJSON Topology where
  parseJSON v = case v of
    Object o -> o .: "graph" >>= \(name :: String) -> case name of
      "diamond"   -> return Diamond
      "dotfile"   -> DotFile   <$> o .: "filepath"
      "line"      -> Line      <$> o .: "nodes"
      "hypercube" -> HyperCube <$> o .: "dimensions"
      "star"      -> Star      <$> o .: "nodes"
      "cycle"     -> Cycle     <$> o .: "nodes"
      "complete"  -> Complete  <$> o .: "nodes"
      "hourglass" -> Hourglass <$> o .: "nodes"
      "random"    -> Random    <$> o .: "nodes"
      "tree"      -> Tree      <$> o .: "depth" <*> o .: "childs"
      "grid"      -> Grid      <$> o .: "rows"  <*> o .: "cols"
      "torus2d"   -> Torus2D   <$> o .: "rows"  <*> o .: "cols"
      "torus3d"   ->
        Torus3D
          <$> o .: "rows"
          <*> o .: "cols"
          <*> o .: "planes"
      _ -> tmm
    _ -> tmm
   where
    tmm = typeMismatch "Topology" v

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

-- | A hourglass shaped graph consisting of two independent complete
-- sub-graphs, only connected via a single edge between two distinct
-- nodes of each sub-graph.
hourglass :: KnownNat n => SNat n -> Graph (2 * n)
hourglass (snatToNum -> n)
  | n == 0    = boundGraph $ A.array (0,-1) []
  | otherwise = boundGraph $ A.array bounds
                  $ fmap (\i -> (i, neighbours i)) [0..2*n-1]
 where
  bounds = (0, 2*n-1)
  neighbours i
    | even i    =
      (if i == 0 then (1 :) else id)
        [ 2*j   | j <- [0..n-1], 2*j   /= i ]
    | otherwise =
      (if i == 1 then (0 :) else id)
        [ 2*j+1 | j <- [0..n-1], 2*j+1 /= i ]

topologyName :: Topology -> String
topologyName = \case
  Diamond{}   -> "diamond"
  Line{}      -> "line"
  HyperCube{} -> "hypercube"
  Grid{}      -> "grid"
  Torus2D{}   -> "torus2d"
  Torus3D{}   -> "torus3d"
  Tree{}      -> "tree"
  Star{}      -> "star"
  Cycle{}     -> "cycle"
  Complete{}  -> "complete"
  Hourglass{} -> "hourglass"
  DotFile{}   -> "dotfile"
  Random{}    -> "random"

topologyParser :: Parser Topology
topologyParser = hsubparser
  (  commandGroup "Available topologies:"
  <> metavar "TOPOLOGY"
  <> command (topologyName Diamond)
       (  info
            ( pure Diamond
            )
            $ progDesc "diamond graph"
       <> footerDoc
            ( Just $ pretty $ unlines
                [ "looks like:"
                , ""
                , "      o"
                , "     / \\"
                , "    o---o"
                , "     \\ /"
                , "      o"
                ]
            )
       )
  <> command (topologyName $ Line undefined)
       (  info
            ( Line <$> option auto
                (  long "nodes"
                <> short 'n'
                <> metavar "NUM"
                <> help "number of nodes of the graph"
                )
            )
            $ progDesc "line graph"
       <> footerDoc
            ( Just $ pretty $ unlines
                [ "looks like: (for n = 3)"
                , ""
                , "  o---o---o"
                ]
            )
       )
  <> command (topologyName $ HyperCube undefined)
       (  info
            ( HyperCube <$> option auto
                (  long "dimensions"
                <> short 'n'
                <> metavar "NUM"
                <> help "number of dimensions"
                )
            )
            $ progDesc "hyper cube"
       <> footerDoc
            ( Just $ pretty $ unlines
                [ "looks like: (for n = 3)"
                , ""
                , "      o---o"
                , "     /|  /|"
                , "    o---o |"
                , "    | o-|-o"
                , "    |/  |/"
                , "    o---o"
                ]
            )
       )
  <> command (topologyName $ Grid undefined undefined)
       (  info
            ( Grid
                <$> option auto
                      (  long "rows"
                      <> short 'r'
                      <> metavar "NUM"
                      <> help "number of rows"
                      )
                <*> option auto
                      (  long "cols"
                      <> short 'c'
                      <> metavar "NUM"
                      <> help "number of columns"
                      )
            ) $ progDesc "2-dimensional grid"
       <> footerDoc
            ( Just $ pretty $ unlines
                [ "looks like: (for r = 3, c = 4)"
                , ""
                , "   o---o---o---o"
                , "   |   |   |   |"
                , "   o---o---o---o"
                , "   |   |   |   |"
                , "   o---o---o---o"
                ]
            )
       )
  <> command (topologyName $ Torus2D undefined undefined)
       (  info
            ( Torus2D
                <$> option auto
                      (  long "rows"
                      <> short 'r'
                      <> metavar "NUM"
                      <> help "number of rows"
                      )
                <*> option auto
                      (  long "cols"
                      <> short 'c'
                      <> metavar "NUM"
                      <> help "number of columns"
                      )
            )
            $ progDesc "2-dimensional torus"
       <> footerDoc
            ( Just $ pretty $ unlines
                [  "c.f. https://www.researchgate.net/figure/"
                <> "The-two-dimensional-torus-4x4_fig1_221134153"
                ]
            )
       )
  <> command (topologyName $ Torus3D undefined undefined undefined)
       (  info
            ( Torus3D
                <$> option auto
                      (  long "rows"
                      <> short 'r'
                      <> metavar "NUM"
                      <> help "number of rows"
                      )
                <*> option auto
                      (  long "cols"
                      <> short 'c'
                      <> metavar "NUM"
                      <> help "number of columns"
                      )
                <*> option auto
                      (  long "planes"
                      <> short 'k'
                      <> metavar "NUM"
                      <> help "number of planes"
                      )
            ) $ progDesc "3-dimensional torus"
       <> footerDoc
            ( Just $ pretty $ unlines
                [  "c.f. https://upload.wikimedia.org/wikipedia/"
                <> "commons/thumb/3/3f/2x2x2torus.svg/"
                <> "220px-2x2x2torus.svg.png"
                ]
            )
       )
  <> command (topologyName $ Tree undefined undefined)
       (  info
            ( Tree
                <$> option auto
                      (  long "depth"
                      <> short 'd'
                      <> metavar "NUM"
                      <> help "depth of the tree"
                      )
                <*> option auto
                      (  long "childs"
                      <> short 'c'
                      <> metavar "NUM"
                      <> help "number of children"
                      )
            ) $ progDesc "balanced tree"
       <> footerDoc
            ( Just $ pretty $ unlines
                [ "looks like: (for d = 2, c = 2)"
                , ""
                , "       o"
                , "      / \\"
                , "     o   o"
                , "    /|   |\\"
                , "   o o   o o"
                ]
            )
       )
  <> command (topologyName $ Star undefined)
       (  info
            ( Star <$> option auto
                (  long "nodes"
                <> short 'n'
                <> metavar "NUM"
                <> help "number of non-central nodes of the graph"
                )
            ) $ progDesc "star shaped graph"
       <> footerDoc
            ( Just $ pretty $ unlines
                [ "looks like: (for n = 8)"
                , ""
                , "    o o o"
                , "     \\|/"
                , "   o--o--o"
                , "     /|\\"
                , "    o o o"
                ]
            )
       )
  <> command (topologyName $ Cycle undefined)
       (  info
            ( Cycle <$> option auto
                (  long "nodes"
                <> short 'n'
                <> metavar "NUM"
                <> help "number of nodes of the graph"
                )
            ) $ progDesc "cycle shaped graph"
       <> footerDoc
            ( Just $ pretty $ unlines
                [ "looks like: (for NODES = 6)"
                , ""
                , "     o--o"
                , "    /    \\"
                , "   o      o"
                , "    \\    /"
                , "     o--o"
                ]
            )
       )
  <> command (topologyName $ Complete undefined)
       (  info
            ( Complete <$> option auto
                (  long "nodes"
                <> short 'n'
                <> metavar "NUM"
                <> help "number of nodes of the graph"
                )
            ) $ progDesc "fully connected graph"
       <> footerDoc
            ( Just $ pretty $ unlines
                [ "looks like: (for NODES = 4)"
                , ""
                , "      o"
                , "     /|\\"
                , "    o-+-o"
                , "     \\|/"
                , "      o"
                ]
            )
       )
  <> command (topologyName $ Hourglass undefined)
       (  info
            ( Hourglass <$> option auto
                (  long "nodes"
                <> short 'n'
                <> metavar "NUM"
                <> help "number of nodes in one half of the hourglass"
                )
            ) $ progDesc "hourglass shaped graph with fully connected 'halves'"
       <> footerDoc
            ( Just $ pretty $ unlines
                [ "looks like: (for NODES = 3)"
                , ""
                , "    o---o"
                , "     \\ /"
                , "      o"
                , "      |"
                , "      o"
                , "     / \\"
                , "    o---o"
                ]
            )
       )
  <> command (topologyName $ Random undefined)
       (  info
            ( Random <$> option auto
                (  long "nodes"
                <> short 'n'
                <> metavar "NUM"
                <> help "number of nodes of the graph"
                )
            ) $ progDesc "random connected graph"
       )
  <> command (topologyName $ DotFile undefined)
       (  info
            ( DotFile <$> strOption
                (  long "dot"
                <> short 'd'
                <> metavar "FILE"
                <> action "file"
                <> help "GraphViz DOT file"
                )
            ) $ progDesc "GraphViz DOT graph"
       )
  )

randomGraph :: Int -> IO G.Graph
randomGraph n = do
  let is = [0,1..n - 1]

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
    d1 <- readArray aE (i, j)
    d2 <- readArray aE (j, i)
    when (d1 /= d2) $ do
      writeArray aE (i, j) False
      writeArray aE (j, i) False

  -- ensure connectivity
  forM_ (pairwise randomPermutation) $ \(i, j) -> do
    writeArray aE (i, j) True
    writeArray aE (j, i) True

  available <- freeze (aE :: IOUArray (Int, Int) Bool)

  -- create the graph
  return $ buildG (0, n - 1)
    [ (i, j)
    | i <- is
    , j <- is
    , available A.! (i, j)
    ]

-- | Turns a graph into a graphviz dot structure, as it is required by
-- the [happy-dot](https://hackage.haskell.org/package/happy-dot) library.
toDot ::
  G.Graph ->
  -- ^ graph to be turned into graphviz dot
  String ->
  -- ^ some name for the graph to be used within gravhviz dot
  (Bool, GraphType, Maybe Name, [Statement])
  -- ^ the result, as it is needed by happy-dot
toDot g name = (True, Graph, Just $ StringID name, map asEdgeStatement $ edges g)
 where
  asEdgeStatement (x, y) =
    EdgeStatement (map (\i -> NodeRef (XMLID ('n' : show i)) Nothing) [x,y]) []

-- | Reads a graph from a dot file. Only the name and structure of the
-- graph is used, i.e., any additional graphviz dot specific
-- annotations are ignored.
fromDot ::
  String
  -- ^ the string holding the graphviz dot content
  -> Either String (G.Graph, String)
  -- ^ either an error, if given an unusable input, or the extracted
  -- graph and name
fromDot cnt = do
  (strict, gType, maybe "" fromDotName -> name, statements) <- parse cnt

  when (not strict) $ Left "Graph must be strict"
  when (gType == Digraph) $ Left "Graph must be undirected"

  edgeChains <- catMaybes <$> mapM fromStatement statements

  let
    namedEdges = concatMap (pairwise . map asString) edgeChains
    edgeNames = dedupAndSort $ concatMap (\(x, y) -> [x, y]) namedEdges
    n = length edgeNames - 1
    idx = (M.!) $ M.fromList $ zip edgeNames [0,1..]
    g = buildG (0,n)
      -- Self loops are removed at this point as they are redundant in
      -- terms of the simulated topology. In terms of connectivity, a
      -- node can synchronize its clock with itself even without a
      -- elastic buffer in between. Therefore, we all self-loops in
      -- the input, but remove them here, since they don't have any
      -- effect on the topology that is created out of the graph.
      $ filter (uncurry (/=))
      -- remove duplicate edges and sort for pretty printing
      $ dedupAndSort
      $ concatMap (\(x, y) -> [(idx x, idx y), (idx y, idx x)])
        namedEdges

  when (length (scc g) > 1) $ Left "Graph must be strongly connected"

  return (g, name)

 where
  fromStatement :: Statement -> Either String (Maybe [Name])
  fromStatement = \case
    EdgeStatement xs _ -> fmap Just $ forM xs $ \case
      NodeRef n _ -> return n
      Subgraph{}  -> Left "Subgraphs are not supported"
    -- we are only interested in the edges, everthing else can be
    -- ignored
    _ -> pure Nothing

  dedupAndSort :: Ord a => [a] -> [a]
  dedupAndSort = S.toList . S.fromList

  asString = \case
    StringID x -> 's' : x
    XMLID x    -> 'x' : x

  fromDotName = \case
    StringID x -> x
    XMLID x    -> x

-- | Successive overlapping pairs.
--
-- >>> pairwise [1, 2, 3, 4]
-- [(1,2),(2,3),(3,4)]
-- >>> pairwise []
-- []
pairwise :: [a] -> [(a,a)]
pairwise as = zip as (tail as)