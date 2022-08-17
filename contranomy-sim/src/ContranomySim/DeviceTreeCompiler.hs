-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module ContranomySim.DeviceTreeCompiler
  ( compileDeviceTreeSource
  ) where


import qualified Data.ByteString  as BS
import           Data.Char        (isSpace)
import           Data.List        (dropWhileEnd)
import           Prelude
import           System.Directory
import           System.Exit
import           System.IO        (hClose, openBinaryTempFile)
import           System.Process


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
      putStrLn "Unable to find device tree compiler on the system."
      putStrLn "Please install the device tree compiler on your system."
      putStrLn "(Ubuntu) `apt-get install device-tree-compiler`"
      putStrLn "(Fedora) `dnf install dtc`"

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
          putStrLn $ "devicetree compilation failed with exit code: " <> show n
          putStrLn $ "devicetree-source file: " <> src
          putStrLn ""
          putStrLn "stdout:"
          putStrLn stdout
          putStrLn ""
          putStrLn "stderr:"
          putStrLn stderr

          hClose handle
          removeFile path

          pure Nothing
