-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Bittide.Topology
  ( -- * Data Types
    TopologyType(..)
  , TopologyName
  , Topology
      ( topologyName
      , topologyGraph
      , topologyType
      , hasEdge
      )
  , STop(..)
  , TreeSize
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
  , fromGraph
  , fromTopologyType
  , fromDot
  , toDot
  , randomTopology
  , topTypeCLIParser
  ) where

import Prelude

import Clash.Prelude
  ( SNat(..)
  , SomeNat(..)
  , Nat
  , Index
  , KnownNat
  , type (^)
  , type (*)
  , type (+)
  , type (-)
  , type Div
  , d0
  , natToInteger
  , snatProxy
  , snatToNum
  , snatToInteger
  , someNatVal
  )

import Control.Monad (forM, forM_, when, replicateM, replicateM_)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.=), (.:), object)
import Data.Aeson.Types (typeMismatch)
import Data.Array.IO (IOUArray)
import Data.Array.MArray (newListArray, readArray, writeArray, getElems, freeze)
import Data.Bifunctor (bimap, first)
import Data.Bits (Bits (..))
import Data.Containers.ListUtils (nubOrd)
import Data.Function (on)
import Data.Graph (Graph, graphFromEdges, edges, buildG, scc)
import Data.List (groupBy, sort)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Proxy (Proxy(..))
import Data.Tuple (swap)

import Data.Array      qualified as A ((!), array, listArray, accumArray)
import Data.Map.Strict qualified as M ((!), fromList)
import Data.Set        qualified as S (fromList, toList)

import GHC.Num.Natural (Natural)
import GHC.TypeNats (natVal)
import GHC.TypeLits.KnownNat (KnownNat2(..), SNatKn(..), nameToSymbol)

import Language.Dot.Graph (GraphType(..), Name(..), Statement(..), Subgraph(..))
import Language.Dot.Parser (parse)

import Options.Applicative
  ( Parser, action, auto, short, long, help, info, option, command
  , progDesc, strOption, metavar, hsubparser, commandGroup, footerDoc
  )
import Options.Applicative.Help.Pretty (pretty)

import System.Random (randomIO, randomRIO)

-- | Special topologies may have names given as a string.
type TopologyName = String

-- | A topology is just a simple graph extended with a type level size
-- bound, a name and a 'TopologyType'.
data Topology (n :: Nat) =
  Topology
    { topologyName :: TopologyName
    , topologyGraph :: Graph
    , topologyType  :: TopologyType IO Integer
    , hasEdge :: Index n -> Index n -> Bool
    }

-- | Existentially quantified version hiding the type level bound.
data STop = forall n. KnownNat n => STop (Topology n)

-- | Smart constructor of 'Topology'.
fromGraph :: forall n. KnownNat n => TopologyName -> Graph -> Topology n
fromGraph name graph =
  Topology name graph (Random $ natToInteger @n) $ curry $ (A.!)
    $ A.accumArray (const id) False bounds
    $ zip (filter (uncurry (/=)) edgeIndices) [True, True ..]
 where
  bounds = ((minBound, minBound), (maxBound, maxBound))
  edgeIndices = map (bimap fromIntegral fromIntegral) $ edges graph

-- | Disambiguates between a selection of known topologies, topologies
-- that are loaded from DOT files, and random topologies. The first
-- type parameter @m@ indicates a context, that may be required to
-- generate an instance of the given topology type. If no specific
-- context is required @m@ is left unspecified. The second type
-- parameter @n@ indicates an integral type, in which the topology may
-- be parameterized.
data TopologyType m n where
  Diamond :: TopologyType m n
  Line :: n -> TopologyType m n
  HyperCube :: n -> TopologyType m n
  Grid :: n -> n -> TopologyType m n
  Torus2D :: n -> n -> TopologyType m n
  Torus3D :: n -> n -> n -> TopologyType m n
  Tree :: n -> n -> TopologyType m n
  Star :: n -> TopologyType m n
  Cycle :: n -> TopologyType m n
  Complete :: n -> TopologyType m n
  Hourglass :: n -> TopologyType m n
  DotFile :: FilePath -> TopologyType IO n
  Random :: n -> TopologyType IO n

instance Show (TopologyType m a) where
  show = \case
    Diamond{}   -> topologyName   diamond
    Line{}      -> topologyName $ line d0
    HyperCube{} -> topologyName $ hypercube d0
    Grid{}      -> topologyName $ grid d0 d0
    Torus2D{}   -> topologyName $ torus2d d0 d0
    Torus3D{}   -> topologyName $ torus3d d0 d0 d0
    Tree{}      -> topologyName $ tree d0 d0
    Star{}      -> topologyName $ star d0
    Cycle{}     -> topologyName $ cyclic d0
    Complete{}  -> topologyName $ complete d0
    Hourglass{} -> topologyName $ hourglass d0
    DotFile{}   -> "dotfile"
    Random{}    -> "random"

-- Unfortunately, we cannot derive 'Eq' and 'Ord' for GADTs.
instance Eq a => Eq (TopologyType m a) where
  x == y = case (x, y) of
    (Diamond,       Diamond          ) -> True
    (Line n,        Line n'          ) -> n == n'
    (HyperCube n,   HyperCube n'     ) -> n == n'
    (Grid n m,      Grid n' m'       ) -> n == n' && m == m'
    (Torus2D n m,   Torus2D n' m'    ) -> n == n' && m == m'
    (Torus3D n m k, Torus3D n' m' k' ) -> n == n' && m == m' && k == k'
    (Tree n m,      Tree n' m'       ) -> n == n' && m == m'
    (Star n,        Star n'          ) -> n == n'
    (Cycle n,       Cycle n'         ) -> n == n'
    (Complete n,    Complete n'      ) -> n == n'
    (Hourglass n,   Hourglass n'     ) -> n == n'
    (DotFile p,     DotFile p'       ) -> p == p'
    _                                  -> False

instance Ord a => Ord (TopologyType m a) where
  compare x y = case (x, y) of
    (Diamond,       Diamond          ) -> EQ
    (Line n,        Line n'          ) -> compare n n'
    (HyperCube n,   HyperCube n'     ) -> compare n n'
    (Grid n m,      Grid n' m'       ) -> compare (n, m) (n', m')
    (Torus2D n m,   Torus2D n' m'    ) -> compare (n, m) (n', m')
    (Torus3D n m k, Torus3D n' m' k' ) -> compare (n, m, k) (n', m', k')
    (Tree n m,      Tree n' m'       ) -> compare (n, m) (n', m')
    (Star n,        Star n'          ) -> compare n n'
    (Cycle n,       Cycle n'         ) -> compare n n'
    (Complete n,    Complete n'      ) -> compare n n'
    (Hourglass n,   Hourglass n'     ) -> compare n n'
    (DotFile p,     DotFile p'       ) -> compare p p'
    (Random{},      Random{}         ) -> LT
    _ -> compare (ordId x) (ordId y)
   where
    ordId = \case
      Diamond{}   -> 0 :: Int
      Line{}      -> 1
      HyperCube{} -> 2
      Grid{}      -> 3
      Torus2D{}   -> 4
      Torus3D{}   -> 5
      Tree{}      -> 6
      Star{}      -> 7
      Cycle{}     -> 8
      Complete{}  -> 9
      Hourglass{} -> 10
      DotFile{}   -> 11
      Random{}    -> 12

instance ToJSON a => ToJSON (TopologyType m a) where
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
    gt = "graph" .= show t

instance FromJSON a => FromJSON (TopologyType IO a) where
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

newtype FUN a = FUN (forall n . SNat n -> a)

-- | Generates some topology of the given topology type, if possible.
fromTopologyType ::
  (Integral n, Applicative m) =>
  TopologyType m n -> m (Either String STop)
fromTopologyType tt = case tt of
  Diamond -> ret $ Just $ STop diamond
  Line n -> ret $ FUN (\sn@SNat -> STop $ line sn) <#> n
  HyperCube n -> ret $ FUN (\sn@SNat -> STop $ hypercube sn) <#> n
  Grid rows cols ->
    let grid# :: FUN (FUN STop)
        grid# = FUN (\n@SNat -> FUN (\m@SNat -> STop $ grid n m))
     in ret $ grid# <#> rows <!> cols
  Torus2D rows cols ->
    let torus2d# :: FUN (FUN STop)
        torus2d# = FUN (\r@SNat -> FUN (\c@SNat -> STop $ torus2d r c))
     in ret $ torus2d# <#> rows <!> cols
  Torus3D rows cols planes ->
    let torus3d# :: FUN (FUN (FUN STop))
        torus3d# = FUN (\r@SNat -> FUN (\c@SNat -> FUN (\p@SNat -> STop $ torus3d r c p)))
     in ret $ torus3d# <#> rows <!> cols <!> planes
  Tree depth childs ->
    let tree# :: FUN (FUN STop)
        tree# = FUN (\n@SNat -> FUN (\m@SNat -> STop $ tree n m))
     in ret $ tree# <#> depth <!> childs
  Star n -> ret $ FUN (\sn@SNat -> STop $ star sn) <#> n
  Cycle n -> ret $ FUN (\sn@SNat -> STop $ cyclic sn) <#> n
  Complete n -> ret $ FUN (\sn@SNat -> STop $ complete sn) <#> n
  Hourglass n -> ret $ FUN (\sn@SNat -> STop $ hourglass sn) <#> n
  Random n -> either (return . Left) (Right <$>) $
    maybeToEither $ FUN (\sn@SNat -> STop <$> randomTopology sn) <#> n
  DotFile f -> readFile f >>=
    return . first (("Invalid DOT file - " <> f <> "\n") <>) . fromDot
 where
  ret = pure . maybeToEither
  maybeToEither = maybe (Left "cannot construct SNat arguments") Right

  infixl 8 <!>
  (<!>) :: Integral i => Maybe (FUN a) -> i -> Maybe a
  msnf <!> n = do
    snf <- msnf
    SomeNat p <- someNatVal $ toInteger n
    case snf of
      FUN f -> pure (f (snatProxy p))

  infixl 8 <#>
  (<#>) :: Integral i => FUN a -> i -> Maybe a
  (<#>) f i = Just f <!> i

-- | Given a list of edges, turn it into a directed graph.
fromEdgeList :: forall a. Ord a => [(a, a)] -> Graph
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

-- | @n@ nodes in a line, connected to their neighbors.
--
-- (differs from the
-- [mathematical terminology](https://mathworld.wolfram.com/LineGraph.html) but
-- conforms to callisto)
line :: SNat n -> Topology n
line sn@SNat =
  ( fromGraph "line" $ fromEdgeList es )
  { topologyType = Line n }
 where
  n = snatToInteger sn
  es = [ (i, i-1) | i <- [2..n] ]

-- | @n@-dimensional hypercube
hypercube :: SNat n -> Topology (2 ^ n)
hypercube sn@SNat =
  ( fromGraph "hypercube" $ fromEdgeList es )
  { topologyType = HyperCube $ snatToInteger sn }
 where
  n = snatToNum sn
  k = (2 :: Int)^n
  es =
    -- see Callisto code (julia):
    -- https://github.com/bittide/Callisto.jl/blob/73d908c6cb02b9b953cc104e5b42d432efc42598/src/topology.jl#L224
    [ let j = i .|. (1 `shiftL` b) in (i+1, j+1)
    | i <- [0..(k-1)]
    , b <- [0..(n-1)]
    , i .&. (1 `shiftL` b) == 0
    ]

-- | Diamond graph
diamond :: Topology 4
diamond =
  ( fromGraph "diamond" $ A.listArray (0, 3) [[1,3], [0,2,3], [1,3], [0,1,2]] )
  { topologyType = Diamond }

-- | Three dimensional torus.
torus3d :: SNat a -> SNat b -> SNat c -> Topology (a * b * c)
torus3d sna@SNat snb@SNat snc@SNat =
  ( fromGraph "torus3d"  $ fromEdgeList dirEdges )
  { topologyType = Torus3D a b c }
 where
  a = snatToInteger sna
  b = snatToInteger snb
  c = snatToInteger snc
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
torus2d :: SNat rows -> SNat cols -> Topology (rows * cols)
torus2d snRows@SNat snCols@SNat =
  ( fromGraph "torus2d" $ fromEdgeList dirEdges )
  { topologyType = Torus2D rows cols }
 where
  rows = snatToInteger snRows
  cols = snatToInteger snCols
  pairs = [ (m, n) | m <- [0..(rows-1)], n <- [0..(cols-1)] ]
  neighborsOf (m, n) =
    [ ((m-1) `mod` rows, n)
    , ((m+1) `mod` rows, n)
    , (m, (n-1) `mod` cols)
    , (m, (n+1) `mod` cols)
    ]
  dirEdges = concatMap (\p -> fmap (p,) (neighborsOf p)) pairs

-- | [Grid graph](https://mathworld.wolfram.com/GridGraph.html)
grid :: SNat rows -> SNat cols -> Topology (rows * cols)
grid snRows@SNat snCols@SNat =
  ( fromGraph "grid" $ fromEdgeList dirEdges )
  { topologyType = Grid rows cols }
 where
  rows = snatToInteger snRows
  cols = snatToInteger snCols
  pairs = [ (m, n) | m <- [1..rows], n <- [1..cols] ]
  mkEdges (m, n) =
    [ (a, b)
    | a <- [(m-1)..(m+1)], b <- [(n-1)..(n+1)]
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
tree :: SNat depth -> SNat childs -> Topology (TreeSize depth childs)
tree snDepth@SNat snChilds@SNat =
  ( fromGraph "tree" treeGraph )
  { topologyType = Tree depth c }
 where
  depth = snatToInteger snDepth
  c = snatToInteger snChilds
  -- | At depth @d_i@, child node @i@ is connected to the @(i-1) `div` c + 1@st
  -- node at depth @d_i - 1@
  pairs = [ (d_i, i, (i-1) `div` c + 1) | d_i <- [0..depth], i <- [1..(c^d_i)] ]
  mkEdges (0, _, _)           = Nothing
  mkEdges (lvl, node, p_node) = Just ((lvl, node), (lvl-1, p_node))
  directedEdges = mapMaybe mkEdges pairs
  treeGraph = fromEdgeList directedEdges

-- | [Star graph](https://mathworld.wolfram.com/StarGraph.html)
star :: SNat childs -> Topology (TreeSize 1 childs)
star sn =
  (tree SNat sn)
  { topologyName = "star"
  , topologyType = Star $ snatToInteger sn
  }

-- | [Cyclic graph](https://mathworld.wolfram.com/CycleGraph.html) with @n@
-- vertices.
cyclic :: SNat n -> Topology n
cyclic sn@SNat =
  ( fromGraph "cycle" $ A.array bounds ((\i -> (i, neighbors i)) <$> [0..(n-1)]) )
  { topologyType = Cycle $ snatToInteger sn }
 where
  n = snatToNum sn
  bounds = (0, n-1)
  neighbors i = [(i-1) `mod` n, (i+1) `mod` n]

-- | [Complete graph](https://mathworld.wolfram.com/CompleteGraph.html) with @n@
-- vertices.
complete :: SNat n -> Topology n
complete sn@SNat =
  ( fromGraph "complete" $ A.array bounds ((\i -> (i, others i)) <$> [0..(n-1)]) )
  { topologyType = Complete $ snatToInteger sn }
 where
  n = snatToNum sn
  bounds = (0, n-1)
  others i = [ j | j <- [0..(n-1)], j /= i ]

-- | An hourglass shaped graph consisting of two independent complete
-- sub-graphs, only connected via a single edge between two distinct
-- nodes of each sub-graph.
hourglass :: SNat n -> Topology (2 * n)
hourglass sn@SNat = t { topologyType = Hourglass $ snatToInteger sn }
 where
  t = fromGraph "hourglass" $
        if n == 0
        then A.array (0,-1) []
        else A.array bounds $ fmap (\i -> (i, neighbours i)) [0..2*n-1]

  n = snatToNum sn
  bounds = (0, 2*n-1)
  neighbours i
    | even i    =
      (if i == 0 then (1 :) else id)
        [ 2*j   | j <- [0..n-1], 2*j   /= i ]
    | otherwise =
      (if i == 1 then (0 :) else id)
        [ 2*j+1 | j <- [0..n-1], 2*j+1 /= i ]

-- | Command line parser for the different topology types.
topTypeCLIParser :: Parser (TopologyType IO Integer)
topTypeCLIParser = hsubparser
  (  commandGroup "Available topologies:"
  <> metavar "TOPOLOGY"
  <> command (show Diamond)
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
  <> command (show $ Line ())
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
  <> command (show $ HyperCube ())
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
  <> command (show $ Grid () ())
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
  <> command (show $ Torus2D () ())
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
  <> command (show $ Torus3D () () ())
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
  <> command (show $ Tree () ())
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
  <> command (show $ Star ())
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
  <> command (show $ Cycle ())
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
  <> command (show $ Complete ())
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
  <> command (show $ Hourglass ())
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
  <> command (show $ Random ())
       (  info
            ( Random <$> option auto
                (  long "nodes"
                <> short 'n'
                <> metavar "NUM"
                <> help "number of nodes of the graph"
                )
            ) $ progDesc "random connected graph"
       )
  <> command (show $ DotFile "")
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

-- | Generates a random topology of the given size.
randomTopology :: SNat n -> IO (Topology n)
randomTopology sn@SNat = do
  let
    n = snatToNum sn
    is = [0,1..n - 1]

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
  return $ fromGraph "random" $ buildG (0, n - 1)
    [ (i, j)
    | i <- is
    , j <- is
    , available A.! (i, j)
    ]

-- | Turns a topology into a graphviz DOT structure, as it is required by
-- the [happy-dot](https://hackage.haskell.org/package/happy-dot) library.
toDot ::
  KnownNat n =>
  Topology n ->
  -- ^ topology to be turned into graphviz dot
  (Bool, GraphType, Maybe Name, [Statement])
  -- ^ the result, as it is needed by happy-dot
toDot t =
  ( True
  , Graph
  , Just $ StringID $ topologyName t
  , map asEdgeStatement $ edges $ topologyGraph t
  )
 where
  asEdgeStatement (x, y) = EdgeStatement
    (map (\i -> NodeRef (XMLID ('n' : show i)) Nothing) [x,y]) []

-- | Reads a topology from a DOT file. Only the name and structure of the
-- graph is used, i.e., any additional graphviz dot specific
-- annotations are ignored.
fromDot ::
  String
  -- ^ the string holding the graphviz dot content
  -> Either String STop
  -- ^ either an error, if given an unusable input, or the extracted topology
fromDot cnt = do
  (strict, gType, maybe "" fromDotName -> name, statements) <- parse cnt

  when (not strict) $ Left "Graph must be strict"
  when (gType == Digraph) $ Left "Graph must be undirected"

  edgeChains <- catMaybes <$> mapM fromStatement statements

  let
    namedEdges = concatMap (pairwise . map asString) edgeChains
    edgeNames = dedupAndSort $ concatMap (\(x, y) -> [x, y]) namedEdges
    n = length edgeNames
    idx = (M.!) $ M.fromList $ zip edgeNames [0,1..]
    graph = buildG (0,n-1)
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

  when (length (scc graph) > 1) $ Left "Graph must be strongly connected"

  case someNatVal $ toInteger n of
    Nothing -> Left "cannot construct SNat arguments"
    Just (SomeNat (_ :: Proxy n)) ->
      return $ STop $ fromGraph @n name graph
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
