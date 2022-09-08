-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main (main) where

import Control.Monad (when)
import System.Environment (getArgs)
import System.Console.Docopt

import Bittide.Topology

patterns :: Docopt
patterns = [docopt|
sim version 0.1.0

Usage:
  sim csv <steps> <sample>
  sim plot <steps> <sample>
|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs

  when (args `isPresent` (command "csv")) $ do
    n <- args `getArgOrExit` (argument "steps")
    k <- args `getArgOrExit` (argument "sample")
    dumpCsv (read n) (read k)

  when (args `isPresent` (command "plot")) $ do
    n <- args `getArgOrExit` (argument "steps")
    k <- args `getArgOrExit` (argument "sample")
    plotEbs (read n) (read k)
