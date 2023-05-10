-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ProcessingElement.DeviceTreeCompiler
  ( compileDeviceTreeSource
  ) where


import Prelude

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.Exit
import System.IO (hPutStrLn)
import System.Process

import System.IO.Temp.Extra

import qualified Data.ByteString as BS
import qualified System.IO as IO


findDtc :: IO (Maybe FilePath)
findDtc = do
  let process = shell "which dtc"
  (exitcode, stdout, _) <- readCreateProcessWithExitCode process ""

  case exitcode of
    ExitSuccess   -> pure . Just $ dropWhileEnd isSpace stdout
    ExitFailure _ -> pure Nothing

compileDeviceTreeSource :: FilePath -> IO (Maybe BS.ByteString)
compileDeviceTreeSource src = withTempBinaryFile "tmp" "fdt.dtb" $ \path _ -> do
  dtcPathRes <- findDtc
  case dtcPathRes of
    Nothing -> do
      hPutStrLn IO.stderr
        "Unable to find device tree compiler on the system. Are you in a Nix shell?"
      pure Nothing
    Just dtc -> do

      (exitCode, stdout, stderr) <- readProcessWithExitCode
        dtc
        ["-O", "dtb", "-b", "0", src, "-o", path] -- args
        "" -- stdin

      case exitCode of
        ExitSuccess -> do
          content <- BS.readFile path
          pure $ Just content
        ExitFailure n -> do
          hPutStrLn IO.stderr $ "devicetree compilation failed with exit code: " <> show n
          hPutStrLn IO.stderr $ "devicetree-source file: " <> src
          hPutStrLn IO.stderr ""
          hPutStrLn IO.stderr "stdout:"
          hPutStrLn IO.stderr stdout
          hPutStrLn IO.stderr ""
          hPutStrLn IO.stderr "stderr:"
          hPutStrLn IO.stderr stderr
          pure Nothing
