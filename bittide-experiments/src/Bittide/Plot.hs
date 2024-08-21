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
  fsToPpm,
) where

import Clash.Prelude (KnownDomain, KnownNat, Vec)
import Clash.Signal.Internal (Femtoseconds (..), unFemtoseconds)
import Clash.Sized.Vector qualified as Vec

import Control.Monad (void)
import Data.Graph (edges)
import Data.Int (Int64)
import Data.List (foldl', transpose, unzip4, zip4)
import Data.Proxy (Proxy)
import System.FilePath ((</>))

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
  ylabel,
  (%),
  (@@),
 )
import Graphics.Matplotlib qualified as MP (plot)

import Bittide.ClockControl (RelDataCount, clockPeriodFs)
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

plot ::
  forall nNodes m refDom.
  (KnownNat nNodes, KnownNat m, KnownDomain refDom) =>
  -- | Reference domain
  Proxy refDom ->
  -- | output directory for storing the results
  FilePath ->
  -- | topology corresponding to the plot
  Topology nNodes ->
  -- | plot data
  Vec
    nNodes
    [ ( Femtoseconds -- time
      , Femtoseconds -- relative clock offset
      , ReframingStage
      , [(RelDataCount m, SC.StabilityIndication)]
      )
    ] ->
  IO ()
plot refDom outputDir graph plotData =
  matplotWrite outputDir clockPlots elasticBufferPlots
 where
  clockPlots = Vec.imap toClockPlot plotData
  elasticBufferPlots = Vec.imap toElasticBufferPlot plotData

  edgeCount = length $ edges $ topologyGraph graph

  toElasticBufferPlot nodeIndex (unzip4 -> (time, _, reframingStage, buffersPerNode)) =
    foldPlots
      $ fmap
        ( -- Too many legend entries don't fit. We picked 20 by
          -- simply observing when legend entries wouldn't fit
          -- anymore.
          if edgeCount <= 20
            then \(j, p) ->
              withLegend $
                p @@ [o2 "label" $ show nodeIndex <> " ← " <> show j]
            else snd
        )
      $ zip (filter (hasEdge graph nodeIndex) [0, 1 ..])
      $ fmap (plotEbData refDom)
      -- Organize data by node instead of by timestamp. I.e., the first item in
      -- 'timedBuffers' is for this node's first neighbor.
      $ transpose
      $ [ [(t, r, dataCount, stability) | (dataCount, stability) <- bs]
        | (t, r, bs) <- zip3 time reframingStage buffersPerNode
        ]

  toClockPlot nodeIndex (unzip4 -> (time, relativeOffsetFs, _, _)) =
    withLegend $
      (@@ [o2 "label" $ fromEnum nodeIndex]) $
        MP.plot
          (map fsToMs time)
          (map (fsToPpm refDom . fromIntegral . unFemtoseconds) relativeOffsetFs)

  withLegend =
    ( @@
        [ o2 "bbox_to_anchor" (1.01 :: Double, 1 :: Double)
        , o2 "loc" "upper left"
        ]
    )
      . (% legend)

data Marking = Waiting | Stable | Settled | None deriving (Eq)

-- | Convert femtoseconds to milliseconds
fsToMs :: Femtoseconds -> Int64
fsToMs (Femtoseconds fs) =
  -- fs -> ps -> ns -> µs -> ms
  fs `div` 1_000_000_000_000

{- | Convert femtoseconds to parts per million, where femtoseconds represents
the relative shortening or lengthening of a clock period. Note that a positive
value for the input results in a negative output. This is because a /shorter/
period results in /more/ clock ticks per time span.

>>> fsToPpm (Proxy @System) 400
-40.0
>>> fsToPpm (Proxy @System) (-400)
40.0
-}
fsToPpm :: (KnownDomain dom) => Proxy dom -> Double -> Double
fsToPpm refDom fs = -1 * (fs / onePpm)
 where
  onePpm = case clockPeriodFs refDom of
    Femtoseconds f -> fromIntegral f / 1_000_000

{- | Plots the datacount of an elastic buffer and marks those parts of
the plots that are reported to be stable/settled by the stability
checker as well as the time frames at which the reframing detector
is in the waiting state.
-}
plotEbData ::
  (KnownNat m, KnownDomain refDom) =>
  -- | Reference domain
  Proxy refDom ->
  [(Femtoseconds, ReframingStage, RelDataCount m, SC.StabilityIndication)] ->
  Matplotlib
plotEbData _refDom (unzip4 -> (timestampsFs, reframingStages, dataCounts, stabilities)) =
  foldPlots markedIntervals % ebPlot
 where
  timestamps = map fsToMs timestampsFs
  xs = zip4 timestamps reframingStages dataCounts stabilities

  mGr = (@@ [o1 "g-", o2 "linewidth" (8 :: Int)]) -- green marking
  mBl = (@@ [o1 "b-", o2 "linewidth" (8 :: Int)]) -- blue marking
  mRe = (@@ [o1 "r-", o2 "linewidth" (8 :: Int)]) -- red marking
  ebPlot = MP.plot timestamps dataCounts

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
  -- | clock plots
  Vec n Matplotlib ->
  -- | elastic buffer plots
  Vec n Matplotlib ->
  IO ()
matplotWrite dir clockDats ebDats = do
  void $
    file (dir </> plotClocksFileName) $
      constrained
        ( xlabel "Time (ms)"
            % ylabel "Relative frequency (ppm)"
            % foldPlots (reverse $ Vec.toList clockDats)
        )
  void $
    file (dir </> plotElasticBuffersFileName) $
      constrained
        ( xlabel "Time (ms)"
            % foldPlots (Vec.toList ebDats)
        )
 where
  constrained =
    ((figure @@ [o2 "layout" "constrained"] % axes) %)

-- | Folds multiple plots together
foldPlots :: [Matplotlib] -> Matplotlib
foldPlots = foldl' (%) mp
