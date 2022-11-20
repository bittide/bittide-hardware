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
module Bittide.Topology
  ( dumpCsv
  , plotEbs
  , plotHypercube
  , plotHypercube4
  , plotTorus34
  , plotK2
  , plotK3
  , plotK6
  , plotC12
  , plotDiamond
  , plotTree32
  , plotTree23
  , plotStar7
  )
where

import Clash.Explicit.Prelude
import Control.Monad (forM_, zipWithM_)
import System.Directory (createDirectoryIfMissing)

import Prelude qualified as P

import Data.Array qualified as A
import Data.ByteString.Lazy qualified as BSL

import Bittide.Topology.Graph
import Bittide.Topology.TH
import Bittide.Topology.TH.Domain (defBittideClockConfig)

-- | This samples @n@ steps, taking every @k@th datum, and plots clock speeds
-- and elastic buffer occupancy
plotEbs :: Int -> Int -> IO ()
plotEbs = plotC12

plotDiamond :: Int -> Int -> IO ()
plotDiamond = $(plotEbsAPI ("diamond", diamond))

plotHypercube :: Int -> Int -> IO ()
plotHypercube = $(plotEbsAPI ("hypercube3", hypercube 3))

plotHypercube4 :: Int -> Int -> IO ()
plotHypercube4 = $(plotEbsAPI ("hypercube4", hypercube 4))

plotTorus34 :: Int -> Int -> IO ()
plotTorus34 = $(plotEbsAPI ("torus34", torus2d 3 4))

plotK2 :: Int -> Int -> IO ()
plotK2 = $(plotEbsAPI ("complete2", complete 2))

plotK3 :: Int -> Int -> IO ()
plotK3 = $(plotEbsAPI ("complete3", complete 3))

plotK6 :: Int -> Int -> IO ()
plotK6 = $(plotEbsAPI ("complete6", complete 6))

plotC12 :: Int -> Int -> IO ()
plotC12 = $(plotEbsAPI ("cyclic12", cyclic 12))

plotStar7 :: Int -> Int -> IO ()
plotStar7 = $(plotEbsAPI ("star7", star 7))

plotTree32 :: Int -> Int -> IO ()
plotTree32 = $(plotEbsAPI ("tree32", tree 3 2))

plotTree23 :: Int -> Int -> IO ()
plotTree23 = $(plotEbsAPI ("tree23", tree 2 3))

-- | This samples @n@ steps, taking every @k@th datum; the result can be fed to
-- @script.py@
dumpCsv :: Int -> Int -> IO ()
dumpCsv m k = do
  offs <- genOffsN defBittideClockConfig n
  createDirectoryIfMissing True "_build"
  forM_ [0..n] $ \i ->
    let eb = g A.! i in
    writeFile
      ("_build/clocks" <> show i <> ".csv")
      ("t,clk" <> show i <> P.concatMap (\j -> ",eb" <> show i <> show j) eb <>  "\n")
  let dats =
          $(onN 6) ($(encodeDats 6) m)
        $ takeEveryN k
        $ $(simNodesFromGraph defBittideClockConfig (complete 6)) offs
  zipWithM_ (\dat i ->
    BSL.appendFile ("_build/clocks" <> show i <> ".csv") dat) dats [(0::Int)..]
 where
  (0, n) = A.bounds g
  g = complete 6
