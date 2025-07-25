-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Converts a binary sample dump (produced by `bittide_sys::sample_store`) to
CSV format. This isn't used by our system, but is useful for debugging.
-}
module Main where

import Prelude

import Bittide.ClockControl.SampleStore (Sample, parseAll)
import Control.Concurrent.Async (mapConcurrently_)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((<.>))

import qualified Data.ByteString.Lazy as L
import qualified Data.Csv as Csv

writeCsv :: FilePath -> [Sample 7] -> IO ()
writeCsv path samples = L.writeFile path (Csv.encodeDefaultOrderedByName samples)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> die "Usage: sample_dump_to_csv <input_file1> [<input_file2> ...]"
    inputPaths -> mapConcurrently_ processFile inputPaths
  where
    processFile inputPath = do
      let outputPath = inputPath <.> "csv"
      binaryData <- L.readFile inputPath
      writeCsv outputPath (parseAll binaryData)
