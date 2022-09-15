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
  )
where

import Clash.Explicit.Prelude
import Control.Monad (forM_, zipWithM_)
import System.Directory (createDirectoryIfMissing)

import Prelude qualified as P

import Data.Array qualified as A
import Data.ByteString.Lazy qualified as BSL

import Bittide.ClockControl
import Bittide.Topology.Graph
import Bittide.Topology.TH

-- | This samples @n@ steps, taking every @k@th datum, and plots clock speeds
-- and elastic buffer occupancy
plotEbs :: Int -> Int -> IO ()
plotEbs = $(plotEbsAPI (complete 6))

-- | This samples @n@ steps, taking every @k@th datum; the result can be fed to
-- @script.py@
dumpCsv :: Int -> Int -> IO ()
dumpCsv m k = do
  offs <- genOffsN n
  createDirectoryIfMissing True "_build"
  forM_ [0..n] $ \i ->
    let eb = g A.! i in
    writeFile
      ("_build/clocks" <> show i <> ".csv")
      ("t,clk" <> show i <> P.concatMap (\j -> ",eb" <> show i <> show j) eb <>  "\n")
  let dats =
          $(onN 6) ($(encodeDats 6) m)
        $ takeEveryN k
        $ $(simNodesFromGraph defClockConfig (complete 6)) offs
  zipWithM_ (\dat i ->
    BSL.appendFile ("_build/clocks" <> show i <> ".csv") dat) dats [(0::Int)..]
 where
  (0, n) = A.bounds g
  g = complete 6
