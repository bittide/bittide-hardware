-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.Clash.Shake.Vivado.ParseTimingSummary where

import Prelude

import Paths_bittide_instances
import System.FilePath
import System.IO.Unsafe
import Test.Tasty
import Test.Tasty.Golden
import Text.Pretty.Simple
import Data.Text.Lazy.Encoding

import Clash.Shake.Vivado.ParseTimingSummary

{-# NOINLINE dataDir #-}
dataDir :: FilePath
dataDir = unsafePerformIO getDataDir

reportDir, goldenDir :: FilePath
reportDir = dataDir </> "tests/reports"
goldenDir = reportDir </> "golden"

tests :: TestTree
tests = testGroup "ParseTimingSummary"
  [ goldenTest "post_place_timing_summary.rpt"
  , goldenTest "post_route_timing_summary.rpt"
  , goldenTest "post_synth_timing_summary.rpt"
  ]

goldenTest :: FilePath -> TestTree
goldenTest report = goldenVsString report (goldenDir </> report) (go report)
 where
  go = fmap (encodeUtf8 . pShowNoColor . parse . lines) . readFile . (reportDir </>)
