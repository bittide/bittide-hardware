-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE QuasiQuotes #-}

{- | Tooling to convert samples saved by @sample_store.rs@ and extracted by GDB's
@dump@ command into CSV files, plots, and reports. These can be used to debug
clock control issues.

Usage: @samples-to <mode> <args>@

Modes:
  - @csv <binary-file>...@: Convert binary files to CSV.
  - @ppm-csv <binary-file>...@: Convert binary files to CSV with PPM values.
  - @ppm-plot <directory>...@: Generate PPM plots from directories containing
    samples.
  - @data-count-plot <directory>...@: Generate data count plots from directories
    containing samples.
  - @report <directory>...@: Generate a report with PPM and data count plots
    from directories containing samples.
  - @multi-report <directory>...@: Generate a report for multiple directories,
    combining their plots into a single PDF.
-}
module Main where

import Prelude

import Bittide.Arithmetic.PartsPer (PartsPer, toPpm)
import Bittide.ClockControl.Config (CcConf (topology))
import Bittide.Instances.Hitl.Driver.ClockControl.Samples (Picoseconds)
import Control.Concurrent.Async (mapConcurrently, wait, withAsync)
import Control.Monad (filterM, void)
import Data.List.NonEmpty (NonEmpty (..))
import Data.String.Interpolate (i)
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (replaceExtension, (</>))
import System.Process (callProcess)

import qualified Bittide.Instances.Hitl.Driver.ClockControl.Plot.DataCount as DataCount
import qualified Bittide.Instances.Hitl.Driver.ClockControl.Plot.Ppm as Ppm
import qualified Bittide.Instances.Hitl.Driver.ClockControl.Plot.Report as Report
import qualified Bittide.Instances.Hitl.Driver.ClockControl.Samples as Samples
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Csv as Csv
import qualified Data.List.NonEmpty as NonEmpty
import qualified Graphics.Matplotlib as Mpl

data PpmSample = PpmSample
  { timestamp :: Double
  , ppm :: Float
  }
  deriving (Show, Eq, Generic, Csv.ToNamedRecord, Csv.DefaultOrdered)

mkPpmSample :: Picoseconds -> PartsPer -> PpmSample
mkPpmSample timestamp partsPer =
  PpmSample
    { timestamp = fromIntegral timestamp / 1e12
    , ppm = toPpm partsPer
    }

csvMain :: NonEmpty String -> IO (NonEmpty FilePath)
csvMain = mapConcurrently processFile
 where
  processFile inputPath = do
    samples <- Samples.parseFile inputPath
    let outputPath = replaceExtension inputPath "csv"
    LazyByteString.writeFile outputPath (Csv.encodeDefaultOrderedByName samples)
    pure outputPath

ppmCsvMain :: NonEmpty String -> IO (NonEmpty FilePath)
ppmCsvMain = mapConcurrently processFile
 where
  processFile inputPath = do
    samples <- Samples.parseFile inputPath
    let
      outputPath = replaceExtension inputPath "ppm.csv"
      ppmSamples = map (uncurry mkPpmSample) (Ppm.fromSamples1 samples)
    LazyByteString.writeFile outputPath (Csv.encodeDefaultOrderedByName ppmSamples)
    pure outputPath

ppmPlotMain :: NonEmpty String -> IO (NonEmpty FilePath)
ppmPlotMain = mapConcurrently processDir
 where
  processDir dir = do
    (ccConf, samples) <- Samples.parseDirectory dir
    let
      outputPath = dir </> "cc-samples-clocks.pdf"
      plot = Ppm.plot (Ppm.fromSamples ccConf.topology samples)
    result <- Mpl.file outputPath plot
    case result of
      Left err -> die $ [i|Error generating plot for directory '#{dir}': #{show err}|]
      Right _ -> pure outputPath

countPlotMain :: NonEmpty String -> IO (NonEmpty FilePath)
countPlotMain = mapConcurrently processDir
 where
  processDir dir = do
    (ccConf, samples) <- Samples.parseDirectory dir
    let
      outputPath = dir </> "cc-samples-data-counts.pdf"
      plot = DataCount.plot (DataCount.fromSamples ccConf.topology samples)
    result <- Mpl.file outputPath plot
    case result of
      Left err -> die $ [i|Error generating plot for directory '#{dir}': #{show err}|]
      Right _ -> pure outputPath

reportMain :: NonEmpty String -> IO (NonEmpty FilePath)
reportMain = mapConcurrently processDir
 where
  processDir dir = do
    (ccConf, _samples) <- Samples.parseDirectory dir
    (clocksPdf, dataCountsPdf) <-
      withAsync (NonEmpty.head <$> ppmPlotMain (pure dir)) $ \clocksAsync ->
        withAsync (NonEmpty.head <$> countPlotMain (pure dir)) $ \countsAsync -> do
          clocksPdf <- wait clocksAsync
          dataCountsPdf <- wait countsAsync
          pure (clocksPdf, dataCountsPdf)

    latex <-
      Report.asLatex $
        Report.ToLatexArgs
          { url = Nothing
          , header = Nothing
          , clocksPdf
          , dataCountsPdf
          , ccConf
          }

    let outputPath = dir </> "cc-samples-report.pdf"
    Report.renderLatex outputPath latex
    pure outputPath

multiReportMain :: NonEmpty FilePath -> IO (NonEmpty FilePath)
multiReportMain = mapConcurrently processDir
 where
  processDir parentDir = do
    paths <- map (parentDir </>) <$> listDirectory parentDir
    dirs <- filterM doesDirectoryExist paths
    case NonEmpty.nonEmpty dirs of
      Nothing -> die $ "No subdirectories found in " ++ parentDir
      Just nonEmptyDirs -> do
        pdfs <- reportMain nonEmptyDirs
        let outputPdf = parentDir </> "cc-report.pdf"
        callProcess "pdfunite" (NonEmpty.toList pdfs ++ [outputPdf])
        pure outputPdf

main :: IO ()
main = do
  args <- getArgs
  void $
    case args of
      ("csv" : file : files) -> csvMain (file :| files)
      ("ppm-csv" : file : files) -> ppmCsvMain (file :| files)
      ("ppm-plot" : dir : dirs) -> ppmPlotMain (dir :| dirs)
      ("data-count-plot" : dir : dirs) -> countPlotMain (dir :| dirs)
      ("report" : dir : dirs) -> reportMain (dir :| dirs)
      ("multi-report" : dir : dirs) -> multiReportMain (dir :| dirs)
      _ ->
        die $
          unlines
            [ "Usage: samples-to <mode> <args>"
            , "Modes:"
            , "  csv <binary-file>..."
            , "  ppm-csv <binary-file>..."
            , "  ppm-plot <directory>..."
            , "  data-count-plot <directory>..."
            , "  report <directory>..."
            , "  multi-report <directory>..."
            ]
