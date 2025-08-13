-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.Common where

import Prelude

import Data.List.Extra (trim)
import Data.String.Interpolate (i)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (
  CreateProcess (cwd),
  proc,
  readCreateProcessWithExitCode,
  readProcess,
 )
import Test.Tasty.HUnit (assertFailure)

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
