-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

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
  (Matplotlib, (%), (@@), file, plot, xlabel, ylabel, o1, o2, mp)

import Bittide.Simulate (Offset)
import Bittide.Domain (Bittide, defBittideClockConfig)
import Bittide.ClockControl (ClockControlConfig(..), clockPeriodFs)
import Bittide.Topology (simulate, simulationEntity, allStableAndCentered)
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
    { margin     :: Int
    , framesize  :: Int
    , samples    :: Int
    , periodsize :: Int
    , mode       :: OutputMode
    , dir        :: FilePath
    , stopStable :: Maybe Int
    , fixOffsets :: [Int64]
    , save       :: G.Graph -> [Int64] -> Maybe Bool -> IO ()
    }

plotDiamond :: SimulationSettings -> IO Bool
plotDiamond settings@SimulationSettings{..} =
  case ( someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology clockControlConfig
            (SNat @margin) (SNat @framesize) diamond settings
    (x, y) -> invalidArgs False (isNothing x) $ isNothing y

plotCyclic :: SimulationSettings -> Int -> IO Bool
plotCyclic settings@SimulationSettings{..} nodes =
  case ( simulatableG (SomeGraph . cyclic) nodes
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology clockControlConfig
            (SNat @margin) (SNat @framesize) graph settings
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)

plotComplete :: SimulationSettings -> Int -> IO Bool
plotComplete settings@SimulationSettings{..} nodes =
  case ( simulatableG (SomeGraph . complete) nodes
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology clockControlConfig
            (SNat @margin) (SNat @framesize) graph settings
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)

plotHourglass :: SimulationSettings -> Int -> IO Bool
plotHourglass settings@SimulationSettings{..} nodes =
  case ( simulatableG (SomeGraph . hourglass) nodes
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology clockControlConfig
            (SNat @margin) (SNat @framesize) graph settings
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)

plotGrid :: SimulationSettings -> Int -> Int -> IO Bool
plotGrid settings@SimulationSettings{..} rows cols =
  case ( simulatableG2 ((SomeGraph .) . grid) rows cols
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology clockControlConfig
            (SNat @margin) (SNat @framesize) graph settings
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)

plotStar :: SimulationSettings -> Int -> IO Bool
plotStar settings@SimulationSettings{..} nodes =
  case ( simulatableG (SomeGraph . star) nodes
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology clockControlConfig
            (SNat @margin) (SNat @framesize) graph settings
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)

plotTorus2D :: SimulationSettings -> Int -> Int -> IO Bool
plotTorus2D settings@SimulationSettings{..} rows cols =
  case ( simulatableG2 ((SomeGraph .) . torus2d) rows cols
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology clockControlConfig
           (SNat @margin) (SNat @framesize) graph settings
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)

plotTorus3D :: SimulationSettings -> Int -> Int -> Int -> IO Bool
plotTorus3D settings@SimulationSettings{..} rows cols planes =
  case ( simulatableG3 (((SomeGraph .) .) . torus3d) rows cols planes
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology clockControlConfig
            (SNat @margin) (SNat @framesize) graph settings
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)

plotTree :: SimulationSettings -> Int -> Int -> IO Bool
plotTree settings@SimulationSettings{..} depth childs =
  case ( simulatableG2 ((SomeGraph .) . tree) depth childs
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology clockControlConfig
            (SNat @margin) (SNat @framesize) graph settings
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)

plotLine :: SimulationSettings -> Int -> IO Bool
plotLine settings@SimulationSettings{..} nodes =
  case ( simulatableG (SomeGraph . line) nodes
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology clockControlConfig
            (SNat @margin) (SNat @framesize) graph settings
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)

plotHyperCube :: SimulationSettings -> Int -> IO Bool
plotHyperCube settings@SimulationSettings{..} nodes =
  case ( simulatableG (SomeGraph . hypercube) nodes
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology clockControlConfig
            (SNat @margin) (SNat @framesize) graph settings
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)


plotGraph :: SimulationSettings -> G.Graph -> IO Bool
plotGraph settings@SimulationSettings{..} g =
  case ( simulatableG (SomeGraph . givenGraph) n
       , someNat margin
       , somePositiveNat framesize
       ) of
    (  Just (SomeSimulatableGraph graph)
     , Just (SomeNat (_ :: Proxy margin))
     , Just (SomePositiveNat (_ :: Proxy framesize))
     ) -> simulateTopology clockControlConfig
            (SNat @margin) (SNat @framesize) graph settings
    (x, y, z) -> invalidArgs (isNothing x) (isNothing y) (isNothing z)
 where

  n = let (l, u) = bounds g in u - l + 1
  givenGraph :: KnownNat n => SNat n -> Graph n
  givenGraph = const $ boundGraph g

clockControlConfig ::
  forall margin framesize.
  (KnownNat margin, KnownNat framesize) =>
  ClockControlConfig Bittide 12 margin framesize
clockControlConfig =
  ClockControlConfig
    { cccStabilityCheckerMargin    = SNat @margin
    , cccStabilityCheckerFramesize = SNat @framesize
    , ..
    }
  where
    ClockControlConfig{..} = defBittideClockConfig

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
  ( KnownDomain dom
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
  SNat margin ->
  -- ^ margin of the stability checker
  SNat framesize ->
  -- ^ frame size of cycles within the margins required
  Graph nodes ->
  -- ^ the topology
  SimulationSettings ->
  -- ^ simulation settings
  IO Bool
  -- ^ stability result
simulateTopology ccc margin framesize graph settings = do
  offsets <- V.zipWith (maybe id const) givenOffsets <$> genOffs ccc
  let
    simResult = sim offsets
    saveSettings =
      save (unboundGraph graph)
        $ (\(Femtoseconds x) -> x) <$> V.toList offsets

  saveSettings Nothing

  case mode of
    PDF -> plotTopology simResult
    CSV -> dumpCsv simResult

  let stable = allStableAndCentered $ V.map last simResult
  saveSettings $ Just stable
  return stable
 where
  SimulationSettings
    { samples
    , periodsize
    , mode
    , dir
    , stopStable
    , fixOffsets
    , save
    } = settings

  sim =
   simulate graph stopStable samples periodsize
    . simulationEntity graph ccc margin framesize

  plotTopology =
    uncurry (matplotWrite dir) . V.unzip . V.map plotDats

  plotDats =
      bimap
        (uncurry plot . unzip)
        (foldPlots . fmap plotEbData . transpose)
    . unzip
    . fmap (\(x,y,z) -> ((x,y), (x,) <$> z))

  givenOffsets =
      V.unsafeFromList
    $ take (natToNum @nodes)
    $ (Just . Femtoseconds <$> fixOffsets) <> repeat Nothing

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
  flatten (a, b, v) = toField a : toField b : (toField . fst <$> v)
  (0, n) = bounds $ unboundGraph $ graph

-- | Plots the datacount of an elastic buffer and marks those parts of
-- the plots that are reported to be stable/centered by the stability
-- checker for the repsective buffer.
plotEbData ::
  (ToJSON t, ToJSON d) => [(t, (d, (Bool, Bool)))] -> Matplotlib
plotEbData xs = foldPlots markedIntervals % ebPlot
 where
  markGreen = (@@ [ o1 "g-", o2 "linewidth" (10 :: Int)])
  markBlue = (@@ [ o1 "b-", o2 "linewidth" (10 :: Int)])
  ebPlot = uncurry plot $ unzip (pData <$> xs)

  markedIntervals =
    (\(mark, ys) -> mark $ uncurry plot $ unzip ys)
      <$> stableIvs [] False False xs

  stableIvs a _  _  []     = a
  stableIvs a as ac (x:xr) = stableIvs a' (stable x) (centered x) xr
   where
    a' |      stable x  && not (centered x) && not as           =
         (markBlue,  [pData x]  ) : a
       |      stable x  &&      centered x  &&           not ac =
         (markGreen, [pData x]  ) : a
       |      stable x  && not (centered x) &&     as &&     ac =
         (markBlue,  [pData x]  ) : (m, reverse y) : yr
       |      stable x                      &&     as           =
         (m,         pData x : y) : yr
       | not (stable x)                     &&     as           =
         (m,         reverse y  ) : yr
       | otherwise                                              =
                                    a
    (m, y) : yr = a

  stable (_, (_, (s, _))) = s
  centered (_, (_, (_, c))) = c
  pData (t, (d, _)) = (t, d)

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
  void $
    file
      ( dir </> "clocks" <> ".pdf")
      ( xlabel "Time (fs)"
      % ylabel "Relative period (fs) [0 = ideal frequency]"
      % foldPlots (V.toList clockDats)
      )
  void $
    file
      ( dir </> "elasticbuffers" <> ".pdf")
      (xlabel "Time (fs)" % foldPlots (V.toList ebDats))

-- | Folds multiple plots together
foldPlots :: [Matplotlib] -> Matplotlib
foldPlots = foldl' (%) mp

-- | Generates a vector of random offsets.
genOffs ::
  (KnownDomain dom, KnownNat k, KnownNat n) =>
  ClockControlConfig dom n m c ->
  IO (V.Vec k Offset)
genOffs = V.traverse# genOffsets . V.replicate SNat

-- | Randomly generate an 'Offset', how much a real clock's period may
-- differ from its spec.
genOffsets ::
  forall dom n m c.
  KnownDomain dom =>
  ClockControlConfig dom n m c ->
  IO Offset
genOffsets ClockControlConfig{cccDeviation} = do
  Ppm offsetPpm <- randomRIO (-cccDeviation, cccDeviation)
  let nonZeroOffsetPpm = if offsetPpm == 0 then cccDeviation else Ppm offsetPpm
  pure (diffPeriod nonZeroOffsetPpm (clockPeriodFs @dom Proxy))

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
