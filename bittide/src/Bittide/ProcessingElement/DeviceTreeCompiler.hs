-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ProcessingElement.DeviceTreeCompiler
  ( compileDeviceTreeSource
  ) where


import qualified Data.ByteString  as BS
import           Data.Char        (isSpace)
import           Data.List        (dropWhileEnd)
import           Prelude
import           System.Directory
import           System.Exit
import System.IO ( hClose, hPutStrLn, openBinaryTempFile )
import           System.Process

import qualified System.IO as IO


findDtc :: IO (Maybe FilePath)
findDtc = do
  let process = shell "which dtc"
  (exitcode, stdout, _) <- readCreateProcessWithExitCode process ""

  case exitcode of
    ExitSuccess   -> pure . Just $ dropWhileEnd isSpace stdout
    ExitFailure _ -> pure Nothing

compileDeviceTreeSource :: FilePath -> IO (Maybe BS.ByteString)
compileDeviceTreeSource src = do

  dtcPathRes <- findDtc
  case dtcPathRes of
    Nothing -> do
      hPutStrLn IO.stderr
        "Unable to find device tree compiler on the system. Are you in a Nix shell?"
      pure Nothing
    Just dtc -> do

      (path, handle) <- openBinaryTempFile "/tmp" "fdt.dtb"

      (exitCode, stdout, stderr) <- readProcessWithExitCode
        dtc
        ["-O", "dtb", "-b", "0", src, "-o", path] -- args
        "" -- stdin

      case exitCode of
        ExitSuccess   -> do
          hClose handle

          content <- BS.readFile path
          removeFile path

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

          hClose handle
          removeFile path

          pure Nothing
