-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ImplicitPrelude #-}

module Bittide.Plot (
  ReframingStage (..),
  fromRfState,
  plot,
  plotClocksFileName,
  plotElasticBuffersFileName,
) where

import Clash.Prelude (KnownNat, Vec)
import Clash.Signal.Internal (Femtoseconds (..))
import Clash.Sized.Vector qualified as Vec

import Bittide.Arithmetic.PartsPer (PartsPer, fromSteps, ppb, toPpm)
import Control.Monad (void)
import Data.Graph (edges)
import Data.Int (Int64)
import Data.List (foldl', transpose, unzip4, unzip5, zip4)
import GHC.Float.RealFracMethods (roundFloatInteger)
import System.FilePath ((</>))
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

import Debug.Trace

import Graphics.Matplotlib (
  Matplotlib,
  axes,
  figure,
  file,
  legend,
  mp,
  o1,
  o2,
  xlabel,
  xlim,
  ylabel,
  (%),
  (@@),
 )
import Graphics.Matplotlib qualified as MP (plot)

import Bittide.ClockControl (RelDataCount)
import Bittide.ClockControl.Callisto (ReframingState (..))
import Bittide.ClockControl.StabilityChecker qualified as SC (StabilityIndication (..))
import Bittide.Topology

{- $setup
>>> import Clash.Prelude
>>> import Data.Proxy
-}

{- | 'Bittide.ClockControl.Callisto.ReframingState' reduced to its
stages.
-}
data ReframingStage = RSDetect | RSWait | RSDone deriving (Show)

-- | Default name of the clocks plot file.
plotClocksFileName :: String
plotClocksFileName = "clocks.pdf"

-- | Default name of the elastic buffers plot file.
plotElasticBuffersFileName :: String
plotElasticBuffersFileName = "elasticbuffers.pdf"

fromRfState :: ReframingState -> ReframingStage
fromRfState = \case
  Detect{} -> RSDetect
  Wait{} -> RSWait
  Done{} -> RSDone

type FincFdecs = Int

normalizeFincFdecs :: PartsPer -> [PartsPer] -> [FincFdecs] -> [PartsPer]
normalizeFincFdecs stepSize measuredOffsets (fmap (fromSteps stepSize) -> offsets) =
  (+correction) <$> offsets
 where
  correction = target - last offsets

  -- target: what the last value should normalize to
  target = arithmeticMean (drop (length measuredOffsets - 10) measuredOffsets)
  arithmeticMean xs = sum xs `div` fromIntegral (length xs)


plot ::
  forall nNodes m.
  (KnownNat nNodes, KnownNat m) =>
  -- | A common correction to apply to all clock plots
  Maybe PartsPer ->
  -- | output directory for storing the results
  FilePath ->
  -- | topology corresponding to the plot
  Topology nNodes ->
  -- | plot data
  Vec
    nNodes
    [ ( Femtoseconds -- time
      , PartsPer -- relative clock offset
      , FincFdecs
      , ReframingStage
      , [(RelDataCount m, SC.StabilityIndication)]
      )
    ] ->
  IO ()
plot maybeCorrection outputDir graph plotData =
  matplotWrite
    outputDir
    maybeCorrection
    clockPlotsGuessed
    clockPlotsMeasured
    elasticBufferPlots
 where
  (clockPlotsGuessed, clockPlotsMeasured) = Vec.unzip $ Vec.imap toClockPlot plotData
  elasticBufferPlots = Vec.imap toElasticBufferPlot plotData

  edgeCount = length $ edges $ topologyGraph graph

  toElasticBufferPlot nodeIndex (unzip5 -> (time, _, _, reframingStage, buffersPerNode)) =
    foldPlots
      $ fmap
        ( -- Too many legend entries don't fit. We picked 20 by
          -- simply observing when legend entries wouldn't fit
          -- anymore.
          if edgeCount <= 20
            then \(j, p) ->
              withLegend $
                p @@ [o2 "label" $ traceId (show nodeIndex <> " ← " <> show j)]
            else snd
        )
      $ zip (filter (hasEdge graph nodeIndex) [0, 1 ..])
      $ fmap plotEbData
      -- Organize data by node instead of by timestamp. I.e., the first item in
      -- 'timedBuffers' is for this node's first neighbor.
      $ transpose
      $ [ [(t, r, dataCount, stability) | (dataCount, stability) <- bs]
        | (t, r, bs) <- zip3 time reframingStage buffersPerNode
        ]

  toClockPlot nodeIndex (unzip5 -> (time, relativeOffsetPartsPer, fincFdecs, _, _)) =
    ( offsetGuessed @@ [o2 "color" "black", o2 "linestyle" "--", o2 "linewidth" (0.5 :: Double)]
    , offsetMeasured
    )
   where
    correctedRelativeOffsetPartsPer = correctOffset <$> relativeOffsetPartsPer

    offsetGuessed =
      MP.plot
        (map fsToMs time)
        (map toPpm (normalizeFincFdecs (ppb 10) correctedRelativeOffsetPartsPer fincFdecs))

    offsetMeasured =
      withLegend $
        (@@ [o2 "label" $ fromEnum nodeIndex]) $
          MP.plot
            (map fsToMs time)
            (map toPpm correctedRelativeOffsetPartsPer)


  withLegend =
    ( @@
        [ o2 "bbox_to_anchor" (1.01 :: Double, 1 :: Double)
        , o2 "loc" "upper left"
        ]
    )
      . (% legend)

  correctOffset = case maybeCorrection of
    Just c -> (+ c)
    Nothing -> id

data Marking = Waiting | Stable | Settled | None deriving (Eq)

-- | Convert femtoseconds to milliseconds
fsToMs :: Femtoseconds -> Int64
fsToMs (Femtoseconds fs) =
  -- fs -> ps -> ns -> µs -> ms
  fs `div` 1_000_000_000_000

{- | Plots the datacount of an elastic buffer and marks those parts of
the plots that are reported to be stable/settled by the stability
checker as well as the time frames at which the reframing detector
is in the waiting state.
-}
plotEbData ::
  (KnownNat m) =>
  [(Femtoseconds, ReframingStage, RelDataCount m, SC.StabilityIndication)] ->
  Matplotlib
plotEbData (unzip4 -> (timestampsFs, reframingStages, dataCounts, stabilities)) =
  foldPlots markedIntervals % ebPlot
 where
  timestamps = map fsToMs timestampsFs
  xs = zip4 timestamps reframingStages dataCounts stabilities

  mGr = (@@ [o1 "g-", o2 "linewidth" (8 :: Int)]) -- green marking
  mBl = (@@ [o1 "b-", o2 "linewidth" (8 :: Int)]) -- blue marking
  mRe = (@@ [o1 "r-", o2 "linewidth" (8 :: Int)]) -- red marking
  ebPlot = MP.plot (0:timestamps) (0:dataCounts)

  mindMarking ys ms = \case
    Waiting -> (mRe, reverse ys) : ms
    Stable -> (mBl, reverse ys) : ms
    Settled -> (mGr, reverse ys) : ms
    None -> ms

  markedIntervals =
    (\(mark, ys) -> mark $ uncurry MP.plot $ unzip ys)
      <$> collectIntervals ((None, []), []) xs

  collectIntervals ((previous, ys), markings) [] =
    mindMarking ys markings previous
  collectIntervals ((previous, ys), markings) ((t, mode, d, sci) : xr) =
    collectIntervals a' xr
   where
    current = case mode of
      RSWait{} -> Waiting
      _
        | SC.settled sci -> Settled
        | SC.stable sci -> Stable
        | otherwise -> None

    markings' = mindMarking ys markings previous

    a'
      | current == previous = ((current, (t, d) : ys), markings)
      | current == None = ((None, []), markings')
      | otherwise = ((current, [(t, d)]), markings')

{- | Folds the vectors of generated plots and writes the results to
the disk.
-}
matplotWrite ::
  (KnownNat n) =>
  -- | output directory
  FilePath ->
  -- | Correction to add to Y-axis label
  Maybe PartsPer ->
  -- | clock plots (guessed)
  Vec n Matplotlib ->
  -- | clock plots (measured)
  Vec n Matplotlib ->
  -- | elastic buffer plots
  Vec n Matplotlib ->
  IO ()
matplotWrite dir maybeCorrection clockDatsG clockDatsM ebDats = do
  plotFincFdecs <- (==Just "yes") <$> lookupEnv "PLOT_FINCFDECS"

  wait_ms <- read @Double . fromMaybe (error "CUTOFF_MS not set") <$> lookupEnv "CUTOFF_MS"

  void $
    file (dir </> plotClocksFileName) $
      constrained
        ( xlabel "Time (ms)"
            % ylabel ("ω (ppm)" <> correctionLabel)
            % xlim (0 :: Double) (wait_ms :: Double)
            % foldPlots (reverse $ (if plotFincFdecs then Vec.toList clockDatsG else []) <> Vec.toList clockDatsM)
        )
  void $
    file (dir </> plotElasticBuffersFileName) $
      constrained
        ( xlabel "Time (ms)"
            % ylabel "β (elements)"
            % xlim (0 :: Double) (wait_ms :: Double)
            % foldPlots (Vec.toList ebDats)
        )
 where
  correctionLabel = case maybeCorrection of
    Just (roundFloatInteger . toPpm -> c)
      | c < 0 -> " [" <> show c <> "]"
      | otherwise -> " [+" <> show c <> "]"
    _ -> ""

  constrained =
    ((figure @@ [o2 "layout" "constrained"] % axes) %)

-- | Folds multiple plots together
foldPlots :: [Matplotlib] -> Matplotlib
foldPlots = foldl' (%) mp
