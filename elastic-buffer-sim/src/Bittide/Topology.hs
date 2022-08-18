-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

-- | This module generates a static topology using template haskell and then
-- dumps clock periods and elastic buffer occupancy to csv.
module Bittide.Topology ( dumpCsv, genOffsets ) where

import Clash.Explicit.Prelude
import Control.Monad          (forM_, replicateM, zipWithM_)
import Prelude qualified as P

import Data.Array qualified as A
import Data.ByteString.Lazy qualified as BSL
import Data.Csv
import System.Random (randomRIO)

import Bittide.Simulate
import Bittide.Simulate.Ppm
import Bittide.Topology.Graph
import Bittide.Topology.TH

-- | This samples @n@ steps; the result can be fed to @script.py@
dumpCsv :: Int -> IO ()
dumpCsv m = do
  offs <- replicateM (n+1) genOffsets
  forM_ [0..n] $ \i ->
    let eb = g A.! i in
    writeFile
      ("clocks" <> show i <> ".csv")
      ("t,clk" <> show i <> P.concatMap (\j -> ",eb" <> show i <> show j) eb <>  "\n")
  let dats =
          onN (encode . P.take m)
        $ $(simNodesFromGraph defClockConfig (kn 6)) offs
  zipWithM_ (\dat i ->
    BSL.appendFile ("clocks" <> show i <> ".csv") dat) dats [(0::Int)..]
 where
  onN = $(onTup 6)
  (0, n) = A.bounds g
  g = kn 6

-- | Randomly generate a 'Offset', how much a real clock's period may differ
-- from its spec.
genOffsets :: IO Offset
genOffsets =
  (`subtract` toInteger specPeriod)
    <$> randomRIO (toInteger minT, toInteger maxT)
 where
  minT = speedUpPeriod specPpm specPeriod
  maxT = slowDownPeriod specPpm specPeriod

-- | Clocks uncertainty is ±100 ppm
specPpm :: Ppm
specPpm = Ppm 100
