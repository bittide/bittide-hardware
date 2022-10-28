-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Prelude

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text.Lazy.Encoding (encodeUtf8)
import System.Console.Docopt
import System.Environment (getArgs)
import Text.Pretty.Simple (pShow)

import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Clash.Shake.Vivado.ParseTimingSummary as ParseTimingSummary

patterns :: Docopt
patterns = [docopt|
Parse timing report and print its tables to something more manageable.

Usage:
  parse-timing-summary [--json] [--show] <rpt> <out>

Options:
  --json           Print JSON instead of pretty printed Haskell data structure
  --show           Print raw show output instead of pretty printed one
|]


main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs

  let
    printJson = args `isPresent` longOption "json"
    printShow = args `isPresent` longOption "show"
    Just rptPath = args `getArg` argument "rpt"
    Just outPath = args `getArg` argument "out"

  rpt <- readFile rptPath
  let
    result = ParseTimingSummary.parse (lines rpt)
    resultBs
      | printJson = encodePretty result
      | printShow = ByteString.pack (show result)
      | otherwise = encodeUtf8 (pShow result)

  ByteString.writeFile outPath resultBs
