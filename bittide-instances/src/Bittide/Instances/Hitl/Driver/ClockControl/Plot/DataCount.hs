-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Driver.ClockControl.Plot.DataCount where

import Prelude

import Bittide.ClockControl.Topology (Topology)
import Bittide.Instances.Hitl.Driver.ClockControl.Plot.Common (constrained, withLegend)
import Bittide.Instances.Hitl.Driver.ClockControl.Samples (
  Buffer (dataCount),
  Picoseconds,
  Sample (..),
  psToS,
  sampleToPicoseconds,
 )
import Bittide.Instances.Hitl.Setup (FpgaCount)
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Graphics.Matplotlib (o2, (%), (@@))

import qualified Bittide.ClockControl.Topology as Topology
import qualified Clash.Prelude as C
import qualified Data.String.Interpolate as I
import qualified Graphics.Matplotlib as Mpl

type Link = (Int, Int)

{- | Organize parsed samples into a list of links (FPGA -> FPGA) with their
buffers statistics. Useful for plotting buffer occupancy over time.
-}
fromSamples ::
  -- | Hardware topology, any buffers not in the topology will be ignored
  Topology ->
  -- | Parsed samples
  C.Vec FpgaCount [Sample] ->
  [(Link, [(Picoseconds, Buffer)])]
fromSamples topology samples =
  concat
    [ goFpga topology toFpga fpgaSamples
    | (toFpga, fpgaSamples) <- zip [0 ..] (C.toList samples)
    , toFpga < Topology.size topology
    ]

goFpga ::
  Topology ->
  Int ->
  [Sample] ->
  [(Link, [(Picoseconds, Buffer)])]
goFpga topology toFpga samples =
  catMaybes
    [ goLink topology timestamps toFpga linkIdx linkSamples
    | (linkIdx, linkSamples) <- zip [0 ..] linkBuffers
    ]
 where
  timestamps = sampleToPicoseconds <$> samples
  linkBuffers = transpose (map (.buffers) samples)

goLink ::
  Topology ->
  [Picoseconds] ->
  Int ->
  Int ->
  [Buffer] ->
  Maybe (Link, [(Picoseconds, Buffer)])
goLink topology timestamps toFpga linkIdx bufferSamples
  | linkIdx < length neighbors
  , let fromFpga = neighbors !! linkIdx =
      Just ((fromFpga, toFpga), zip timestamps bufferSamples)
  | otherwise = Nothing
 where
  neighbors = [n | n <- [0 .. Topology.size topology - 1], Topology.hasEdge topology n toFpga]

-- | Plot buffer occupancy over time
plot :: [(Link, [(Picoseconds, Buffer)])] -> Mpl.Matplotlib
plot (unzip -> (links, linkData)) =
  constrained
    % Mpl.xlabel "Time (s)"
    % Mpl.ylabel "Buffer occupancy"
    % mconcat plotsWithLabels
 where
  plotsWithLabels = zipWith addOptions links plots
  plots = fmap plotLink linkData

  addOptions :: Link -> Mpl.Matplotlib -> Mpl.Matplotlib
  addOptions (from, to) plot0
    -- Skip legend if there are too many links
    | length links <= 15 = withLegend plot1
    | otherwise = plot1
   where
    plot1 = plot0 @@ (dotSize : label)

    -- Adds a label to each plot
    label
      | length links <= 15 = [o2 "label" ([I.i|#{from} → #{to}|] :: String)]
      | otherwise = []

    -- Makes the dots (from the scatter plot) a bit smaller
    dotSize = o2 "s" (0.1 :: Double)

  plotLink :: [(Picoseconds, Buffer)] -> Mpl.Matplotlib
  plotLink (unzip -> (timestamps, buffers)) =
    Mpl.scatter
      (map psToS timestamps)
      (map (toInteger . (.dataCount)) buffers)
