-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Graphics.Matplotlib.Ext ( foldPlots ) where

import Data.List (foldl')

import Graphics.Matplotlib (Matplotlib, (%), mp)

foldPlots :: [Matplotlib] -> Matplotlib
foldPlots = foldl' (%) mp
