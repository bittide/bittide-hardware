-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import Prelude

import Bittide.ProcessingElement.ProgramStream

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

import qualified Clash.Prelude as C
import qualified Data.ByteString as BS

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      elfContent <- BS.readFile path
      let streamBytes = elfStream elfContent
      let stream = BS.pack (C.bitCoerce <$> streamBytes)
      BS.putStr stream
      exitSuccess
    _ -> do
      hPutStrLn stderr "This program requires exactly one argument, a path to an ELF"
      exitFailure
