-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Driver.ClockControl.Plot.DataCount where

import Prelude

import Bittide.ClockControl.Topology (Topology, hasEdge)
import Bittide.Instances.Hitl.Driver.ClockControl.Plot.Common (constrained, withLegend)
import Bittide.Instances.Hitl.Driver.ClockControl.Samples (
  Buffer (dataCount),
  Link,
  Picoseconds,
  Sample (..),
  psToS,
  sampleToPicoseconds,
 )
import Bittide.Instances.Hitl.Setup (FpgaCount, LinkCount, fpgaSetup)
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Graphics.Matplotlib (o2, (%), (@@))

import qualified Clash.Prelude as C
import qualified Clash.Sized.Vector as V
import qualified Data.String.Interpolate as I
import qualified Graphics.Matplotlib as Mpl

{- | Organize parsed samples into a list of links (FPGA -> FPGA) with their
buffers statistics. Useful for plotting buffer occupancy over time.
-}
fromSamples ::
  -- | Hardware topology, any buffers not in the topology will be ignored
  Topology ->
  -- | Parsed samples
  C.Vec FpgaCount [Sample] ->
  [(Link, [(Picoseconds, Buffer)])]
fromSamples topology = concat . C.toList . C.imap goFpga
 where
  goFpga ::
    C.Index FpgaCount ->
    [Sample] ->
    [(Link, [(Picoseconds, Buffer)])]
  goFpga toFpga samples =
    catMaybes (C.toList (C.imap (goLink timestamps toFpga) linkBuffers))
   where
    timestamps = sampleToPicoseconds <$> samples
    linkBuffers = transposeListVec (map (.buffers) samples)

  goLink ::
    [Picoseconds] ->
    C.Index FpgaCount ->
    C.Index LinkCount ->
    [Buffer] ->
    Maybe (Link, [(Picoseconds, Buffer)])
  goLink timestamps toFpga fromLink samples
    | hasEdge topology (C.numConvert fromFpga) (C.numConvert toFpga) =
        Just ((fromFpga, toFpga), zip timestamps samples)
    | otherwise = Nothing
   where
    fromFpga = snd (fpgaSetup C.!! toFpga) C.!! fromLink

transposeListVec :: (C.KnownNat n) => [C.Vec n a] -> C.Vec n [a]
transposeListVec = V.unsafeFromList . transpose . map C.toList

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

  addOptions :: (C.Index FpgaCount, C.Index FpgaCount) -> Mpl.Matplotlib -> Mpl.Matplotlib
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
