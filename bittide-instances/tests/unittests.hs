-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}

module Main where

import Prelude

import Data.String.Interpolate (i)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (cwd, proc, readCreateProcessWithExitCode, readProcess)
import Test.Tasty
import Test.Tasty.HUnit (assertFailure, testCase)
import "extra" Data.List.Extra (trim)

import qualified Tests.ClockControlWb as ClockControlWb
import qualified Wishbone.Axi as Axi
import qualified Wishbone.CaptureUgn as CaptureUgn
import qualified Wishbone.DnaPortE2 as DnaPortE2
import qualified Wishbone.RegisterWbC as RegisterWbC
import qualified Wishbone.ScatterGather as ScatterGather
import qualified Wishbone.SwitchCalendar as Wishbone.SwitchCalendar
import qualified Wishbone.SwitchDemoProcessingElement as SwitchDemoProcessingElement
import qualified Wishbone.Time as Time
import qualified Wishbone.Watchdog as Watchdog

getGitRoot :: IO FilePath
getGitRoot = trim <$> readProcess "git" ["rev-parse", "--show-toplevel"] ""

run :: FilePath -> [String] -> Maybe FilePath -> IO ()
run cmd0 args cwd = do
  let cmd1 = (proc cmd0 args){cwd = cwd}
  (exitCode, stdout, stdErr) <- readCreateProcessWithExitCode cmd1 ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ ->
      assertFailure
        [i|
      Command failed with exit code #{exitCode}:

        #{cmd0} #{unwords args}

      stdout:

        #{stdout}

      stderr:

        #{stdErr}
    |]

tests :: TestTree
tests =
  sequentialTestGroup
    "bittide-instances"
    AllSucceed
    [ testGroup
        "Build Rust crates"
        [ testCase "release" (run "./cargo.sh" ["build", "--release"] (Just gitRoot))
        , testCase "debug" (run "./cargo.sh" ["build"] (Just gitRoot))
        ]
    , testGroup
        "Unittests"
        [ Axi.tests
        , CaptureUgn.tests
        , ClockControlWb.tests
        , DnaPortE2.tests
        , ScatterGather.tests
        , SwitchDemoProcessingElement.tests
        , Time.tests
        , RegisterWbC.tests
        , Watchdog.tests
        , Wishbone.SwitchCalendar.tests
        ]
    ]
 where
  gitRoot = unsafePerformIO getGitRoot

main :: IO ()
main = defaultMain tests
