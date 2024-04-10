-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ImplicitPrelude #-}
module Bittide.Plot
  ( ReframingStage(..)
  , fromRfState
  , plot
  , plotClocksFileName
  , plotElasticBuffersFileName
  ) where

import Clash.Prelude (KnownNat, Vec)
import Clash.Sized.Vector qualified as Vec

import Data.List (foldl', transpose)
import Data.Aeson (ToJSON)
import Data.Bifunctor (bimap)
import Control.Monad (void)
import System.FilePath ((</>))

import qualified Graphics.Matplotlib as MP (plot)
import Graphics.Matplotlib
  ( Matplotlib, (%), (@@)
  , file, xlabel, ylabel, o1, o2, mp, legend, axes, figure
  )

import Bittide.ClockControl.StabilityChecker qualified as SC (StabilityIndication(..))
import Bittide.ClockControl.Callisto (ReframingState(..))
import Bittide.Topology

-- | 'Bittide.ClockControl.Callisto.ReframingState' reduced to its
-- stages.
data ReframingStage = RSDetect | RSWait | RSDone deriving (Show)

-- | Default name of the clocks plot file.
plotClocksFileName :: String
plotClocksFileName = "clocks.pdf"

-- | Default name of the elastic buffers plot file.
plotElasticBuffersFileName :: String
plotElasticBuffersFileName = "elasticbuffers.pdf"

fromRfState :: ReframingState -> ReframingStage
fromRfState = \case
  Detect {} -> RSDetect
  Wait {}   -> RSWait
  Done {}   -> RSDone

plot ::
  (KnownNat n, ToJSON b, ToJSON t, ToJSON d) =>
  -- ^ constraints
  FilePath ->
  -- ^ output directory for storing the resuts
  Topology n ->
  -- ^ topology corresponding to the plot
  Vec n [(t, b, ReframingStage, [(d, SC.StabilityIndication)])] ->
  -- ^ plot data
  IO ()
plot dir graph =
  uncurry (matplotWrite dir) . Vec.unzip . Vec.imap plotDats
 where
  plotDats i =
      bimap
        ( withLegend
        . (@@ [o2 "label" $ fromEnum i])
        . uncurry MP.plot
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

data Marking = Waiting | Stable | Settled | None deriving (Eq)

-- | Plots the datacount of an elastic buffer and marks those parts of
-- the plots that are reported to be stable/settled by the stability
-- checker as well as the time frames at which the reframing detector
-- is in the waiting state.
plotEbData ::
  (ToJSON t, ToJSON d) =>
  [((t, ReframingStage), (d, SC.StabilityIndication))] ->
  Matplotlib
plotEbData xs = foldPlots markedIntervals % ebPlot
 where
  mGr = (@@ [ o1 "g-", o2 "linewidth" (8 :: Int)]) -- green marking
  mBl = (@@ [ o1 "b-", o2 "linewidth" (8 :: Int)]) -- blue marking
  mRe = (@@ [ o1 "r-", o2 "linewidth" (8 :: Int)]) -- red marking
  ebPlot = uncurry MP.plot $ unzip ((\((t, _), (d, _)) -> (t, d)) <$> xs)

  mindMarking ys ms = \case
    Waiting -> (mRe, reverse ys) : ms
    Stable  -> (mBl, reverse ys) : ms
    Settled -> (mGr, reverse ys) : ms
    None    -> ms

  markedIntervals =
    (\(mark, ys) -> mark $ uncurry MP.plot $ unzip ys)
      <$> collectIntervals ((None, []), []) xs

  collectIntervals ((previous, ys), markings) [] =
    mindMarking ys markings previous

  collectIntervals ((previous, ys), markings) (((t, mode), (d, sci)) : xr) =
    collectIntervals a' xr
   where
    current = case mode of
      RSWait {}          -> Waiting
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
  Vec n Matplotlib ->
  -- ^ clock plots
  Vec n Matplotlib ->
  -- ^ elastic buffer plots
  IO ()
matplotWrite dir clockDats ebDats = do
  void $ file (dir </> plotClocksFileName) $ constrained
    ( xlabel "Time (fs)"
    % ylabel "Relative period (fs) [0 = ideal frequency]"
    % foldPlots (reverse $ Vec.toList clockDats)
    )
  void $ file (dir </> plotElasticBuffersFileName) $ constrained
    ( xlabel "Time (fs)"
    % foldPlots (Vec.toList ebDats)
    )
 where
  constrained =
    ((figure @@ [o2 "layout" "constrained"] % axes) %)

-- | Folds multiple plots together
foldPlots :: [Matplotlib] -> Matplotlib
foldPlots = foldl' (%) mp
