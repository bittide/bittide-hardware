-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Driver.ClockControl.Plot.Ppm where

import Prelude

import Bittide.Arithmetic.PartsPer (PartsPer, cyclesToPartsPer)
import Bittide.Arithmetic.Time (PeriodToCycles)
import Bittide.ClockControl.Topology (Topology)
import Bittide.Instances.Domains (Bittide)
import Bittide.Instances.Hitl.Driver.ClockControl.Plot.Common (constrained, withLegend)
import Bittide.Instances.Hitl.Driver.ClockControl.Samples (
  Picoseconds,
  Sample (..),
  psToS,
  sampleToPicoseconds,
 )
import Bittide.Instances.Hitl.Setup (FpgaCount)
import Bittide.Sync (SyncOutGeneratorHalfPeriod)
import Data.Word (Word32, Word64)
import Graphics.Matplotlib (o2, (%), (@@))

import qualified Bittide.Arithmetic.PartsPer as PartsPer
import qualified Bittide.ClockControl.Topology as Topology
import qualified Clash.Prelude as C
import qualified Clash.Sized.Vector as V
import qualified Graphics.Matplotlib as Mpl

-- | Organize parsed samples in timestamped deviations from a reference clock
fromSamples :: Topology -> C.Vec FpgaCount [Sample] -> [[(Picoseconds, PartsPer)]]
fromSamples topology = map fromSamples1 . take (Topology.size topology) . V.toList

-- | Convert a list of samples to a list of timestamped clock frequency deviations
fromSamples1 :: [Sample] -> [(Picoseconds, PartsPer)]
fromSamples1 samples =
  zip
    (map sampleToPicoseconds samples)
    (zipWith go samples (drop 1 samples))
 where
  go :: Sample -> Sample -> PartsPer
  go s0 s1 = cyclesToPartsPer ideal observed
   where
    observed = s1.localClockCounter - s0.localClockCounter
    ideal = s1ideal - s0ideal

    s0ideal = toIdeal s0.numberOfSyncPulsesSeen s0.cyclesSinceSyncPulse
    s1ideal = toIdeal s1.numberOfSyncPulsesSeen s1.cyclesSinceSyncPulse

    toIdeal :: Word32 -> Word32 -> Word64
    toIdeal seen0 since0 = seen2 * cyclesPerSync + since2
     where
      -- XXX: Overlapping instances!
      since1 = C.numConvert since0 :: C.Unsigned 32
      since2 = C.numConvert since1 :: Word64

      seen1 = C.numConvert seen0 :: C.Unsigned 32
      seen2 = C.numConvert seen1 :: Word64

      cyclesPerSync = C.natToNum @(PeriodToCycles Bittide SyncOutGeneratorHalfPeriod)

-- | Plot clock deviations over time
plot :: [[(Picoseconds, PartsPer)]] -> Mpl.Matplotlib
plot ppmsData =
  constrained
    % Mpl.xlabel "Time (s)"
    % Mpl.ylabel "Relative frequency (ppm)"
    % mconcat plotsWithLabels
 where
  plotsWithLabels = zipWith addOptions [0 ..] plots
  plots = fmap plotPpm ppmsData

  addOptions :: Int -> Mpl.Matplotlib -> Mpl.Matplotlib
  addOptions idx plot_ = withLegend (plot_ @@ [label, dotSize])
   where
    -- Adds a label to each plot
    label = o2 "label" (show idx)

    -- Makes the dots (from the scatter plot) a bit smaller
    dotSize = o2 "s" (1 :: Int)

  plotPpm :: [(Picoseconds, PartsPer)] -> Mpl.Matplotlib
  plotPpm (unzip -> (timestamps, deviations)) =
    Mpl.scatter
      (map psToS timestamps)
      (map PartsPer.toPpm deviations)
