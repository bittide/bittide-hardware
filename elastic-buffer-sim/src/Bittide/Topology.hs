-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE PartialTypeSignatures #-}

-- we need this for TH-generated code
--
-- TODO: don't rely on this
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

-- | This module generates a static topology using template haskell and then
-- dumps clock periods and elastic buffer occupancy to csv.
module Bittide.Topology ( dumpCsv, genOffsets, plotEbs ) where

import Clash.Explicit.Prelude
import Control.Monad (void, forM_, replicateM, zipWithM_)

import Prelude qualified as P

import Graphics.Matplotlib ((%), file, xlabel, ylabel)
import System.Directory (createDirectoryIfMissing)
import System.Random (randomRIO)

import Data.Array qualified as A
import Data.ByteString.Lazy qualified as BSL

import Bittide.Simulate
import Bittide.Simulate.Ppm
import Bittide.Topology.Graph
import Bittide.Topology.TH
import Graphics.Matplotlib.Ext

discardN :: Int -> [a] -> [a]
discardN _ [] = []
discardN n (x:xs) = x : discardN n (P.drop n xs)

-- | This samples @n@ steps, taking every @k@th datum, and plots clock speeds
-- and elastic buffer occupancy
plotEbs :: Int -> Int -> IO ()
plotEbs m k = do
  offs <- replicateM (n+1) genOffsets
  createDirectoryIfMissing True "_build"
  let (clockDats, ebDats) =
          P.unzip
        $ $(onN 3) (plotEachNode m)
        $ discardN k
        $ $(simNodesFromGraph defClockConfig (complete 3)) offs
  void $ file "_build/clocks.pdf" (xlabel "Time (ps)" % ylabel "Period (ps)" % foldPlots clockDats)
  void $ file "_build/elasticbuffers.pdf" (xlabel "Time (ps)" % foldPlots ebDats)
 where
  (0, n) = A.bounds g
  g = complete 3
  plotEachNode = $(plotDats (complete 3))

-- | This samples @n@ steps, taking every @k@th datum; the result can be fed to
-- @script.py@
dumpCsv :: Int -> Int -> IO ()
dumpCsv m k = do
  offs <- replicateM (n+1) genOffsets
  createDirectoryIfMissing True "_build"
  forM_ [0..n] $ \i ->
    let eb = g A.! i in
    writeFile
      ("_build/clocks" <> show i <> ".csv")
      ("t,clk" <> show i <> P.concatMap (\j -> ",eb" <> show i <> show j) eb <>  "\n")
  let dats =
          $(onN 6) ($(encodeDats 6) m)
        $ discardN k
        $ $(simNodesFromGraph defClockConfig (complete 6)) offs
  zipWithM_ (\dat i ->
    BSL.appendFile ("_build/clocks" <> show i <> ".csv") dat) dats [(0::Int)..]
 where
  (0, n) = A.bounds g
  g = complete 6

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
