-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImplicitParams #-}

module Bittide.Plot
  ( OutputMode(..)
  , SimulationSettings(..)
  , plotDiamond
  , plotCyclic
  , plotComplete
  , plotHourglass
  , plotGrid
  , plotStar
  , plotTorus2D
  , plotTorus3D
  , plotTree
  , plotLine
  , plotHyperCube
  , plotGraph
  , simulateTopology
  ) where

import Prelude

import Clash.Prelude
  ( KnownDomain
  , KnownNat
  , SomeNat(..)
  , SNat(..)
  , type (<=)
  , type (+)
  , someNatVal
  , natToNum
  )

import Clash.Sized.Vector qualified as V
import Clash.Signal.Internal (Femtoseconds(..))

import GHC.Int (Int64)
import System.Exit (die)
import Text.Read (Read(..), lexP, pfail, readMaybe)
import Text.Read.Lex (Lexeme(Ident))
import Data.List (foldl', transpose)
import Data.Proxy (Proxy(..))
import Data.Aeson (ToJSON, FromJSON, Value(..))
import Data.Aeson.Types (typeMismatch)
import Data.Aeson qualified as A
import Data.Text qualified as T
import Data.Graph qualified as G (Graph)
import Data.Array ((!), bounds)
import Data.Bifunctor (bimap)
import Data.Csv (toField, encode)
import Control.Monad (void, zipWithM_, forM_)
import System.FilePath ((</>))
import System.Random (randomRIO)
import Data.Maybe (isNothing)
import Data.ByteString.Lazy qualified as BSL (appendFile)

import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits.Compare ((:<=?)(..))
import GHC.TypeLits.Witnesses ((%<=?))
import GHC.TypeLits.Witnesses qualified as TLW (SNat(..))

import Graphics.Matplotlib
  ( Matplotlib, (%), (@@)
  , file, plot, xlabel, ylabel, o1, o2, mp, legend, axes, figure
  )

import Bittide.Simulate (Offset)
import Bittide.Domain (Bittide, defBittideClockConfig)
import Bittide.ClockControl (ClockControlConfig(..), clockPeriodFs)
import Bittide.ClockControl.StabilityChecker qualified as SC (StabilityIndication(..))
import Bittide.ClockControl.Callisto (ReframingState(..))
import Bittide.Topology (simulate, simulationEntity, allSettled)
import Bittide.Arithmetic.Ppm (Ppm(..), diffPeriod)
import Bittide.Topology.Graph

data OutputMode =
    CSV
  | PDF
  deriving (Ord, Eq)

instance Show OutputMode where
  show = \case
    CSV -> "csv"
    PDF -> "pdf"

instance Read OutputMode where
  readPrec = lexP >>= \case
    Ident "csv" -> return CSV
    Ident "pdf" -> return PDF
    _           -> pfail

instance ToJSON OutputMode where
  toJSON = String . T.pack . show

instance FromJSON OutputMode where
  parseJSON v = case v of
    String str -> maybe tmm return $ readMaybe $ T.unpack str
    _          -> tmm
   where
    tmm = typeMismatch "OutputMode" v

data SimulationSettings =
  SimulationSettings
    { margin       :: Int
    , framesize    :: Int
    , samples      :: Int
    , periodsize   :: Int
    , reframe      :: Bool
    , rustySim     :: Bool
    , waittime     :: Int
    , mode         :: OutputMode
    , dir          :: FilePath
    , stopStable   :: Maybe Int
    , fixClockOffs :: [Int64]
    , fixStartOffs :: [Int]
    , maxStartOff  :: Int
    , save         :: G.Graph -> [Int64] -> [Int] -> Maybe Bool -> IO ()
    }

plotDiamond :: (?settings :: SimulationSettings) => IO Bool
plotDiamond =
  case ( someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology (clockControlConfig @margin @framesize) diamond
    (x, y) -> invalidArgs False (isNothing x) $ isNothing y
 where
  SimulationSettings{..} = ?settings

plotCyclic :: (?settings :: SimulationSettings) => Int -> IO Bool
plotCyclic nodes =
  case ( simulatableG (SomeGraph . cyclic) nodes
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology (clockControlConfig @margin @framesize) graph
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)
 where
  SimulationSettings{..} = ?settings

plotComplete :: (?settings :: SimulationSettings) => Int -> IO Bool
plotComplete nodes =
  case ( simulatableG (SomeGraph . complete) nodes
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology (clockControlConfig @margin @framesize) graph
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)
 where
  SimulationSettings{..} = ?settings

plotHourglass :: (?settings :: SimulationSettings) => Int -> IO Bool
plotHourglass nodes =
  case ( simulatableG (SomeGraph . hourglass) nodes
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology (clockControlConfig @margin @framesize) graph
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)
 where
  SimulationSettings{..} = ?settings

plotGrid :: (?settings :: SimulationSettings) => Int -> Int -> IO Bool
plotGrid rows cols =
  case ( simulatableG2 ((SomeGraph .) . grid) rows cols
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology (clockControlConfig @margin @framesize) graph
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)
 where
  SimulationSettings{..} = ?settings

plotStar :: (?settings :: SimulationSettings) => Int -> IO Bool
plotStar nodes =
  case ( simulatableG (SomeGraph . star) nodes
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology (clockControlConfig @margin @framesize) graph
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)
 where
  SimulationSettings{..} = ?settings

plotTorus2D :: (?settings :: SimulationSettings) => Int -> Int -> IO Bool
plotTorus2D rows cols =
  case ( simulatableG2 ((SomeGraph .) . torus2d) rows cols
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology (clockControlConfig @margin @framesize) graph
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)
 where
  SimulationSettings{..} = ?settings

plotTorus3D :: (?settings :: SimulationSettings) => Int -> Int -> Int -> IO Bool
plotTorus3D rows cols planes =
  case ( simulatableG3 (((SomeGraph .) .) . torus3d) rows cols planes
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology (clockControlConfig @margin @framesize) graph
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)
 where
  SimulationSettings{..} = ?settings

plotTree :: (?settings :: SimulationSettings) => Int -> Int -> IO Bool
plotTree depth childs =
  case ( simulatableG2 ((SomeGraph .) . tree) depth childs
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology (clockControlConfig  @margin @framesize) graph
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)
 where
  SimulationSettings{..} = ?settings

plotLine :: (?settings :: SimulationSettings) => Int -> IO Bool
plotLine nodes =
  case ( simulatableG (SomeGraph . line) nodes
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology (clockControlConfig @margin @framesize) graph
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)
 where
  SimulationSettings{..} = ?settings

plotHyperCube :: (?settings :: SimulationSettings) => Int -> IO Bool
plotHyperCube nodes =
  case ( simulatableG (SomeGraph . hypercube) nodes
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology (clockControlConfig @margin @framesize) graph
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)
 where
  SimulationSettings{..} = ?settings

plotGraph :: (?settings :: SimulationSettings) => G.Graph -> IO Bool
plotGraph g =
  case ( simulatableG (SomeGraph . givenGraph) n
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology (clockControlConfig @margin @framesize) graph
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)
 where
  SimulationSettings{..} = ?settings
  n = let (l, u) = bounds g in u - l + 1
  givenGraph :: KnownNat n => SNat n -> Graph n
  givenGraph = const $ boundGraph g

clockControlConfig ::
  forall margin framesize.
  (?settings :: SimulationSettings, KnownNat margin, KnownNat framesize) =>
  ClockControlConfig Bittide 12 margin framesize
clockControlConfig =
  ClockControlConfig
    { cccStabilityCheckerMargin    = SNat @margin
    , cccStabilityCheckerFramesize = SNat @framesize
    , cccEnableReframing           = reframe
    , cccReframingWaitTime         = fromInteger $ toInteger waittime
    , cccEnableRustySimulation     = rustySim
    , ..
    }
  where
    ClockControlConfig{..} = defBittideClockConfig
    SimulationSettings{..} = ?settings

invalidArgs :: Bool -> Bool -> Bool -> IO Bool
invalidArgs invalidGraph invalidMargin invalidFramesize
  | invalidGraph     = die "ERROR: the given topology must have between 1 and 20 nodes"
  | invalidMargin    = die "ERROR: the given margin must be non-negative"
  | invalidFramesize = die "ERROR: the given frame size must be positive"
  | otherwise        = return False

-- | Creates and write plots for a given topology according to the
-- given output mode.
simulateTopology ::
  forall dom nodes dcount margin framesize.
  ( ?settings :: SimulationSettings
  -- ^ simulation settings
  , KnownDomain dom
  -- ^ domain
  , KnownNat nodes
  -- ^ the size of the topology is know
  , KnownNat dcount
  -- ^ the size of the data counts is known
  , KnownNat margin
  -- ^ the margins of the stability checker are known
  , KnownNat framesize
  -- ^ the frame size of cycles within the margins required is known
  , 1 <= nodes
  -- ^ the topology consists of at least one node
  , 1 <= dcount
  -- ^ data counts must contain data
  , nodes + dcount <= 32
  -- ^ computational limit of the clock control
  , 1 + nodes <= 32
  -- ^ computational limit of the clock control
  , 1 <= framesize
  -- ^ frames must at least cover one element
  ) =>
  ClockControlConfig dom dcount margin framesize ->
  -- ^ clock control configuration
  Graph nodes ->
  -- ^ the topology
  IO Bool
  -- ^ stability result
simulateTopology ccc graph = do
  clockOffsets <-
    V.zipWith (maybe id const) givenClockOffsets
      <$> genClockOffsets ccc
  startupOffsets <-
    V.zipWith (maybe id const) givenStartupOffsets
      <$> genStartupOffsets maxStartOff
  let
    simResult = sim clockOffsets startupOffsets
    saveSettings =
      save (unboundGraph graph)
        ((\(Femtoseconds x) -> x) <$> V.toList clockOffsets)
        (V.toList startupOffsets)

  saveSettings Nothing

  case mode of
    PDF -> plotTopology simResult
    CSV -> dumpCsv simResult

  let result = allSettled $ V.map last simResult
  saveSettings $ Just result
  return result
 where
  SimulationSettings
    { samples
    , periodsize
    , mode
    , dir
    , stopStable
    , fixClockOffs
    , fixStartOffs
    , maxStartOff
    , save
    } = ?settings

  sim o =
   simulate graph stopStable samples periodsize
    . simulationEntity graph ccc o

  plotTopology =
    uncurry (matplotWrite dir) . V.unzip . V.imap plotDats

  plotDats i =
      bimap
        ( withLegend
        . (@@ [o2 "label" $ fromEnum i])
        . uncurry plot
        . unzip
        )
        ( foldPlots
        . fmap (\(j, p) -> withLegend
                   (p @@ [o2 "label" $ show i <> " ← " <> show j])
               )
        . zip (filter (hasEdge graph i) [0,1..])
        . fmap plotEbData
        . transpose
        )
    . unzip
    . fmap (\(a,b,c,d) -> ((a,b), ((a,c),) <$> d))

  withLegend =
    ( @@ [ o2 "bbox_to_anchor" (1.01 :: Double, 1 :: Double)
         , o2 "loc" "upper left"
         ]
    ) . (% legend)

  givenClockOffsets =
      V.unsafeFromList
    $ take (natToNum @nodes)
    $ (Just . Femtoseconds <$> fixClockOffs) <> repeat Nothing

  givenStartupOffsets =
      V.unsafeFromList
    $ take (natToNum @nodes)
    $ (Just <$> fixStartOffs) <> repeat Nothing

  dumpCsv simulationResult = do
    forM_ [0..n] $ \i -> do
      let eb = unboundGraph graph ! i
      writeFile (filename i)
        ( "t,clk" <> show i
            <> concatMap (\j -> ",eb" <> show i <> show j) eb <>  "\n")
    let dats = V.map (encode . fmap flatten) simulationResult
    zipWithM_
      (\dat i -> BSL.appendFile (filename i) dat)
      (V.toList dats)
      [(0 :: Int)..]

  filename i = dir </> "clocks" <> "_" <> show i <> ".csv"
  flatten (a, b, _, v) = toField a : toField b : (toField . fst <$> v)
  (0, n) = bounds $ unboundGraph graph

data Marking = Waiting | Stable | Settled | None deriving (Eq)

-- | Plots the datacount of an elastic buffer and marks those parts of
-- the plots that are reported to be stable/settled by the stability
-- checker as well as the time frames at which the reframing detector
-- is in the waiting state.
plotEbData ::
  (ToJSON t, ToJSON d) =>
  [((t, ReframingState), (d, SC.StabilityIndication))] ->
  Matplotlib
plotEbData xs = foldPlots markedIntervals % ebPlot
 where
  mGr = (@@ [ o1 "g-", o2 "linewidth" (8 :: Int)]) -- green marking
  mBl = (@@ [ o1 "b-", o2 "linewidth" (8 :: Int)]) -- blue marking
  mRe = (@@ [ o1 "r-", o2 "linewidth" (8 :: Int)]) -- red marking
  ebPlot = uncurry plot $ unzip ((\((t, _), (d, _)) -> (t, d)) <$> xs)

  mindMarking ys ms = \case
    Waiting -> (mRe, reverse ys) : ms
    Stable  -> (mBl, reverse ys) : ms
    Settled -> (mGr, reverse ys) : ms
    None    -> ms

  markedIntervals =
    (\(mark, ys) -> mark $ uncurry plot $ unzip ys)
      <$> collectIntervals ((None, []), []) xs

  collectIntervals ((previous, ys), markings) [] =
    mindMarking ys markings previous

  collectIntervals ((previous, ys), markings) (((t, rfState), (d, sci)) : xr) =
    collectIntervals a' xr
   where
    current = case rfState of
      Wait {}            -> Waiting
      _
        | SC.settled sci -> Settled
        | SC.stable sci  -> Stable
        | otherwise      -> None

    markings' = mindMarking ys markings previous

    a' | current == previous = ((current, (t, d) : ys), markings )
       | current == None     = ((None,    []         ), markings')
       | otherwise           = ((current, [(t, d)]   ), markings')

-- | Folds the vectors of generated plots and writes the results to
-- the disk.
matplotWrite ::
  KnownNat n =>
  FilePath ->
  -- ^ output directory
  V.Vec n Matplotlib ->
  -- ^ clock plots
  V.Vec n Matplotlib ->
  -- ^ elastic buffer plots
  IO ()
matplotWrite dir clockDats ebDats = do
  void $ file (dir </> "clocks" <> ".pdf") $ constrained
    ( xlabel "Time (fs)"
    % ylabel "Relative period (fs) [0 = ideal frequency]"
    % foldPlots (V.toList clockDats)
    )
  void $ file (dir </> "elasticbuffers" <> ".pdf") $ constrained
    ( xlabel "Time (fs)"
    % foldPlots (V.toList ebDats)
    )
 where
  constrained =
    ((figure @@ [o2 "layout" "constrained"] % axes) %)

-- | Folds multiple plots together
foldPlots :: [Matplotlib] -> Matplotlib
foldPlots = foldl' (%) mp

-- | Generates a vector of random clock offsets.
genClockOffsets ::
  forall dom k n m c.
  (KnownDomain dom, KnownNat k, KnownNat n) =>
  ClockControlConfig dom n m c ->
  IO (V.Vec k Offset)
genClockOffsets ClockControlConfig{cccDeviation} =
  V.traverse# (const genOffset) $ V.repeat ()
 where
  genOffset = do
    Ppm offsetPpm <- randomRIO (-cccDeviation, cccDeviation)
    let nonZeroOffsetPpm = if offsetPpm == 0 then cccDeviation else Ppm offsetPpm
    pure (diffPeriod nonZeroOffsetPpm (clockPeriodFs @dom Proxy))

-- | Generates a vector of random startup offsets.
genStartupOffsets :: KnownNat k => Int -> IO (V.Vec k Int)
genStartupOffsets limit =
  V.traverse# (const ((+1) <$> randomRIO (0, limit))) $ V.repeat ()

someNat :: Int -> Maybe SomeNat
someNat = someNatVal . toInteger

data SomePositiveNat =
  forall n. (KnownNat n, 1 <= n) =>
    SomePositiveNat (Proxy n)

somePositiveNat :: Int -> Maybe SomePositiveNat
somePositiveNat n = someNatVal (toInteger n) >>= \(SomeNat (_ :: p n)) ->
  case TLW.SNat @1 %<=? TLW.SNat @n of
    LE Refl -> Just $ SomePositiveNat (Proxy @n)
    _       -> Nothing

data SomeGraph =
    forall n. KnownNat n =>
      SomeGraph (Graph n)

data SomeSimulatableGraph =
  forall n. (KnownNat n, 1 <= n, n <= 20)
    => SomeSimulatableGraph (Graph n)

simulatableG ::
  ( forall n.
    KnownNat n =>
    SNat n -> SomeGraph
  ) ->
  Int -> Maybe SomeSimulatableGraph
simulatableG build n =
  someNatVal (toInteger n) >>= \case
    (SomeNat (_ :: Proxy s)) -> case build (SNat @s) of
      SomeGraph g -> case TLW.SNat @1 %<=? (graphSize g) of
        LE Refl -> case (graphSize g) %<=? TLW.SNat @20 of
          LE Refl -> Just $ SomeSimulatableGraph g
          _       -> Nothing
        _       -> Nothing

simulatableG2 ::
  ( forall n m.
    (KnownNat n, KnownNat m) =>
    SNat n -> SNat m -> SomeGraph
  ) ->
  Int -> Int -> Maybe SomeSimulatableGraph
simulatableG2 build x y =
  someNatVal (toInteger x) >>= \case
    (SomeNat (_ :: Proxy s)) ->
      simulatableG (build (SNat @s)) y

simulatableG3 ::
  ( forall n m k.
    (KnownNat n, KnownNat m, KnownNat k) =>
    SNat n -> SNat m -> SNat k -> SomeGraph) ->
  Int -> Int -> Int -> Maybe SomeSimulatableGraph
simulatableG3 build x y z =
  someNatVal (toInteger x) >>= \case
    (SomeNat (_ :: Proxy s)) ->
      simulatableG2 (build (SNat @s)) y z

graphSize :: KnownNat n => Graph n -> TLW.SNat n
graphSize = const TLW.SNat
