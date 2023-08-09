-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Clash.Shake.Cargo where

import Prelude

import System.Exit
import System.IO
import System.Process

import qualified Clash.Util.Interpolate as I

data CargoBuildMode = Release | Debug
  deriving Eq

cargoBuildFirmwareProgram ::
  FilePath ->
  -- ^ Directory to perform the call in
  CargoBuildMode ->
  -- ^ release mode?
  IO ()
cargoBuildFirmwareProgram workingDir release = do
  let
    cmd = "cargo"
    args = "build" : ["--release" | release == Release]
    createProc =
      (proc cmd args)
        { cwd = Just workingDir}
  (exitCode, _stdoutOut, stderrOut) <- readCreateProcessWithExitCode createProc ""

  case exitCode of
    ExitSuccess -> do
      pure ()
    ExitFailure exitInt -> do
      hPutStrLn stderr [I.i|
        "#{cmd} #{unwords args}" terminated with exit code #{exitInt}.

        Stderr:

          #{stderrOut}

        Stdout:

          #{_stdoutOut}
        |]
      exitWith exitCode
