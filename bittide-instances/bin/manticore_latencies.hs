-- SPDX-FileCopyrightText: 2026 QBayLogic
--
-- SPDX-License-Identifier: Apache-2.0

{- | Emit the Manticore @--hop-latencies@ CSV for the 8-FPGA 2D-torus demo,
derived from the WireDemo golden UGNs (the rig is the same). Run before the
compiler so @MANTICORE_HOP_LATENCIES@ points at the result:

> manticore-latencies latencies.csv
-}
module Main where

import Prelude

import System.Environment (getArgs, getProgName)
import System.Exit (die)

import Bittide.Instances.Hitl.ManticoreDemo.Latencies (writeLatenciesCsv)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> writeLatenciesCsv path
    _ -> do
      prog <- getProgName
      die ("usage: " <> prog <> " <output-latencies.csv>")
