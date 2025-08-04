-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Driver.ClockControl.Plot.Common where

import Prelude

import Graphics.Matplotlib (o2, (%), (@@))

import qualified Graphics.Matplotlib as Mpl

-- | Add a legend to a plot, positioned outside the plot area
withLegend :: Mpl.Matplotlib -> Mpl.Matplotlib
withLegend p = p % Mpl.legend @@ opts
 where
  opts =
    [ o2 "bbox_to_anchor" (1.01 :: Double, 1 :: Double)
    , o2 "loc" "upper left"
    ]

-- | Minimize overlaps between plot elements (axes, labels, titles, legends, etc.)
constrained :: Mpl.Matplotlib
constrained = Mpl.figure @@ [o2 "layout" "constrained"] % Mpl.axes
