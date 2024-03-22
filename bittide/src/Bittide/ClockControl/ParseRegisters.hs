-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}

module Bittide.ClockControl.ParseRegisters where

import Prelude

import Bittide.ClockControl.Si539xSpi (RegisterEntry)
import Clash.Sized.Vector (listToVecTH)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Data.List.Extra (trim)
import Language.Haskell.TH
import System.FilePath ((</>))

import "bittide-extra" Numeric.Extra (parseHex)

import Paths_bittide (getDataFileName)

type Lines = [String]

-- | Parse a triple of hex strings into a 'RegisterEntry'.
--
-- >>> parseRegisterEntry "0x0850,0x00"
-- Right (0b0000_1000,0b0101_0000,0b0000_0000)
-- >>> parseRegisterEntry "0x0850,,,"
-- Left "Could not parse as triple: 0x0850,,,"
parseRegisterEntry :: String -> Either String RegisterEntry
parseRegisterEntry [_, _, h0, h1, h2, h3, _, _, _, h4, h5] = do
  n0 <- parseHex [h0, h1]
  n1 <- parseHex [h2, h3]
  n2 <- parseHex [h4, h5]
  pure (n0, n1, n2)
parseRegisterEntry s = Left ("Could not parse as triple: " <> s)

-- | Keep parsing register entries until a line starting with '#' is encountered.
parseRegisterEntries :: Lines -> Either String ([RegisterEntry], Lines)
parseRegisterEntries [] = Left "parseRegisterEntries: unexpected EOF"
parseRegisterEntries (l:ls0)
  | "#" `isPrefixOf` l = pure ([], ls0)
  | otherwise = do
    entry <- parseRegisterEntry l
    (entries, ls1) <- parseRegisterEntries ls0
    pure (entry:entries, ls1)

-- | Consume lines until a line matching the given string is encountered, then
-- parse register entries until a line starting with '#' is encountered.
parseSection :: String -> Lines -> Either String ([RegisterEntry], Lines)
parseSection s [] = Left ("parseSection: unexpected EOF for '" <> s <> "'")
parseSection s (l:ls)
  | l == s    = parseRegisterEntries ls
  | otherwise = parseSection s ls

-- | Parse a preamble, configuration, and postamble from a \"CSV\" file produced
-- by ClockBuilder Pro. Note that this is not actually a CSV file, but multiple
-- CSV files concatenated together, with comments (lines starting with @#@) in
-- between.
parse :: Lines -> Either String ([RegisterEntry], [RegisterEntry], [RegisterEntry])
parse ls0 = do
  (preamble, ls1) <- parseSection "# Start configuration preamble" ls0
  (config, ls2)   <- parseSection "# Start configuration registers" ls1
  (postamble, _)  <- parseSection "# Start configuration postamble" ls2
  pure (preamble, config, postamble)

-- | Like 'parse', but reads from a file.
parseFromFile ::
  FilePath ->
  IO (Either String ([RegisterEntry], [RegisterEntry], [RegisterEntry]))
parseFromFile f = parse . map trim . lines <$> readFile f

-- | Parse a CSV produced by ClockBuilder Pro into a 'Si539xRegisterMap' using
-- Template Haskell.
parseFromFileToRegisterMap :: FilePath -> Q Exp
parseFromFileToRegisterMap fileName = do
  path <- liftIO $ getDataFileName ("data" </> "clock_configs" </> fileName <> ".csv")
  entries <- liftIO $ parseFromFile path
  case entries of
    Left err -> fail err
    Right (preamble, config, postamble) ->
      [e| Si539xRegisterMap
            $(listToVecTH preamble)
            $(listToVecTH config)
            $(listToVecTH postamble) |]
