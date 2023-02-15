-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
  ( ToJSON(..)
  , FromJSON(..)
  , Value(..)
  , (.=)
  , (.:)
  , encode
  , decode
  , object
  )
import Data.Aeson.Types (typeMismatch)
import Control.Monad (forM, forM_, when, replicateM, replicateM_)
import GHC.Generics (Generic)
import GHC.Int (Int64)
import System.Exit (exitSuccess, exitFailure, die)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import Data.ByteString.Lazy qualified as BS
import Data.Set qualified as S (fromList, toList)
import Data.Map.Strict qualified as M ((!), fromList)
import Data.Maybe (catMaybes)
import Data.Graph (Graph, buildG, edges, scc)
import Data.Array.MArray (newListArray, readArray, writeArray, getElems, freeze)
import Data.Array.IO (IOUArray)
import Data.Array qualified as A ((!))
import System.Random (randomIO, randomRIO)

import Language.Dot.Graph
import Language.Dot.Pretty (render)
import Language.Dot.Parser (parse)

import Options.Applicative
import Options.Applicative.Help.Pretty (text)

import Bittide.Plot

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
  | DotFile FilePath
  | Random Int
  deriving (Show, Ord, Eq)

ttype :: Topology -> String
ttype = \case
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
  DotFile{}   -> "dotfile"
  Random{}    -> "random"

instance ToJSON Topology where
  toJSON t = object $ case t of
    Diamond       -> [ gt ]
    DotFile f     -> [ gt, "filepath"   .= f ]
    Line n        -> [ gt, "nodes"      .= n ]
    HyperCube n   -> [ gt, "dimensions" .= n ]
    Star n        -> [ gt, "nodes"      .= n ]
    Cycle n       -> [ gt, "nodes"      .= n ]
    Complete n    -> [ gt, "nodes"      .= n ]
    Random n      -> [ gt, "nodes"      .= n ]
    Tree d c      -> [ gt, "depth"      .= d, "childs" .= c ]
    Grid r c      -> [ gt, "rows"       .= r, "cols"   .= c ]
    Torus2D r c   -> [ gt, "rows"       .= r, "cols"   .= c ]
    Torus3D r c p -> [ gt, "rows"       .= r, "cols"   .= c, "planes" .= p ]
   where
    gt = "graph" .= ttype t

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

topologyParser :: Parser Topology
topologyParser = hsubparser
  (  commandGroup "Available topologies:"
  <> metavar "TOPOLOGY"
  <> command (ttype Diamond)
       (  info
            ( pure Diamond
            )
            $ progDesc "diamond graph"
       <> footerDoc
            ( Just $ text $ unlines
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
  <> command (ttype $ Line undefined)
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
            ( Just $ text $ unlines
                [ "looks like: (for n = 3)"
                , ""
                , "  o---o---o"
                ]
            )
       )
  <> command (ttype $ HyperCube undefined)
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
            ( Just $ text $ unlines
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
  <> command (ttype $ Grid undefined undefined)
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
            ( Just $ text $ unlines
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
  <> command (ttype $ Torus2D undefined undefined)
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
            ( Just $ text $ unlines
                [  "c.f. https://www.researchgate.net/figure/"
                <> "The-two-dimensional-torus-4x4_fig1_221134153"
                ]
            )
       )
  <> command (ttype $ Torus3D undefined undefined undefined)
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
            ( Just $ text $ unlines
                [  "c.f. https://upload.wikimedia.org/wikipedia/"
                <> "commons/thumb/3/3f/2x2x2torus.svg/"
                <> "220px-2x2x2torus.svg.png"
                ]
            )
       )
  <> command (ttype $ Tree undefined undefined)
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
            ( Just $ text $ unlines
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
  <> command (ttype $ Star undefined)
       (  info
            ( Star <$> option auto
                (  long "nodes"
                <> short 'n'
                <> metavar "NUM"
                <> help "number of non-central nodes of the graph"
                )
            ) $ progDesc "star shaped graph"
       <> footerDoc
            ( Just $ text $ unlines
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
  <> command (ttype $ Cycle undefined)
       (  info
            ( Cycle <$> option auto
                (  long "nodes"
                <> short 'n'
                <> metavar "NUM"
                <> help "number of nodes of the graph"
                )
            ) $ progDesc "cycle shaped graph"
       <> footerDoc
            ( Just $ text $ unlines
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
  <> command (ttype $ Complete undefined)
       (  info
            ( Complete <$> option auto
                (  long "nodes"
                <> short 'n'
                <> metavar "NUM"
                <> help "number of nodes of the graph"
                )
            ) $ progDesc "fully connected graph"
       <> footerDoc
            ( Just $ text $ unlines
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
  <> command (ttype $ Random undefined)
       (  info
            ( Random <$> option auto
                (  long "nodes"
                <> short 'n'
                <> metavar "NUM"
                <> help "number of nodes of the graph"
                )
            ) $ progDesc "random connected graph"
       )
  <> command (ttype $ DotFile undefined)
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

data Options =
  Options
    { topology           :: Maybe Topology
    , outMode            :: OutputMode
    , simulationSteps    :: Int
    , simulationSamples  :: Int
    , stabilityMargin    :: Int
    , stabilityFrameSize :: Int
    , stopWhenStable     :: Bool
    , offsets            :: [Int64]
    , outDir             :: FilePath
    , jsonArgs           :: Maybe FilePath
    , stable             :: Bool
    }
  deriving (Show, Ord, Eq, Generic, ToJSON, FromJSON)

optionParser :: Parser Options
optionParser =
  Options
    <$> optional topologyParser
    <*> option auto
          (  long "output-mode"
          <> short 'm'
          <> metavar "MODE"
          <> value PDF
          <> showDefault
          <> help "Available modes are: csv, pdf"
          )
    <*> option auto
          (  long "steps"
          <> short 's'
          <> metavar "NUM"
          <> value 150000
          <> showDefault
          <> help "Number of clock cycles to simulate"
          )
    <*> option auto
          (  long "samples"
          <> short 'a'
          <> metavar "NUM"
          <> value 100
          <> showDefault
          <> help "Number of samples to keep & pass to matplotlib"
          )
    <*> option auto
          (  long "margin"
          <> short 'g'
          <> metavar "NUM"
          <> value 8
          <> showDefault
          <> help
               (  "Maximum number of elements a buffer occupancy is "
               <> "allowed to deviate to be considered stable"
               )
          )
    <*> option auto
          (  long "frame-size"
          <> short 'f'
          <> metavar "NUM"
          <> value 1500000
          <> showDefault
          <> help
               (  "Minimum number of clock cycles a buffer occupancy "
               <> "must remain within to be considered stable"
               )
          )
    <*> flag False True
          (  long "stop-when-stable"
          <> short 'x'
          <> help "Stop simulation as soon as all buffers get stable"
          )
    <*> option auto
          (  long "offsets"
          <> short 't'
          <> metavar "NUM LIST"
          <> value []
          <> showDefault
          <> help "Initital clock offsets (randomly generated if missing"
          )
    <*> strOption
          (  long "output-directory"
          <> short 'o'
          <> metavar "DIR"
          <> action "directory"
          <> value "_build"
          <> showDefault
          <> help "Directory, to which the generated files are written"
          )
    <*> optional
          ( strOption
              (  long "json-args"
              <> short 'j'
              <> metavar "FILE"
              <> action "file"
              <> help
                   (  "Read arguments from a 'simulate.json' file "
                   <> "(overwrites all arguments other than '-jz')"
                   )
              )
          )
    <*> pure False

cliParser :: ParserInfo Options
cliParser = info (optionParser <**> helper)
  (  fullDesc
  <> header "Bittide Hardware Topology Simulator"
  )

main :: IO ()
main = do
  options@Options{..} <- do
    opts@Options{..} <- execParser cliParser
    case jsonArgs of
      Nothing   -> return opts
      Just file -> do
        cnt <- BS.readFile file
        case decode cnt of
          Nothing -> die $ "ERROR: Invalid JSON file - " <> file
          Just o  -> return o { jsonArgs }

  let
    settings =
      SimulationSettings
        { margin     = stabilityMargin
        , framesize  = stabilityFrameSize
        , samples    = simulationSamples
        , periodsize = simulationSteps `quot` simulationSamples
        , mode       = outMode
        , dir        = outDir
        , stopStable = stopWhenStable
        , fixOffsets = offsets
        }

  createDirectoryIfMissing True outDir

  (((isStable, offs), g), name) <- case topology of
    Just t -> let dName = ttype t in case t of
      Diamond       -> (,dName) <$> plotDiamond settings
      Line n        -> (,dName) <$> plotLine settings n
      HyperCube n   -> (,dName) <$> plotHyperCube settings n
      Grid r c      -> (,dName) <$> plotGrid settings r c
      Torus2D r c   -> (,dName) <$> plotTorus2D settings r c
      Torus3D r c p -> (,dName) <$> plotTorus3D settings r c p
      Tree d c      -> (,dName) <$> plotTree settings d c
      Star n        -> (,dName) <$> plotStar settings n
      Cycle n       -> (,dName) <$> plotCyclic settings n
      Complete n    -> (,dName) <$> plotComplete settings n
      Random n      -> (,dName) <$> (randomGraph n >>= plotGraph settings)
      DotFile f     -> (fromDot <$> readFile f) >>= \case
        Right (g, name) -> (,name) <$> plotGraph settings g
        Left err        -> die $ "ERROR: Invalid DOT file - " <> f <> "\n" <> err
    Nothing ->
      handleParseResult $ Failure
        $ parserFailure defaultPrefs cliParser (ShowHelpText Nothing) []

  let topologyFile = outDir </> "topology.gv"
  writeFile topologyFile $ (<> "\n") $ render $ toDot g name
  BS.writeFile (outDir </> "simulate.json") $ encode options
     { stable   = isStable
     , offsets  = offs
     , jsonArgs = Nothing
     , topology = case topology of
         Just (Random _) -> Just $ DotFile topologyFile
         _               -> topology
     }

  if isStable
  then exitSuccess
  else exitFailure

randomGraph :: Int -> IO Graph
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
  Graph ->
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
  -> Either String (Graph, String)
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
-- [(1,2), (2, 3), (3, 4), (4, 5)]
-- >>> pairwise []
-- []
pairwise :: [a] -> [(a,a)]
pairwise as = zip as (tail as)
