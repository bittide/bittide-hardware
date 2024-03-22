-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.ClockControl.ParseRegisters where

import Prelude

import Clash.Sized.Vector (listToVecTH)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Data.List.Extra (trim)
import Numeric (readHex)
import System.FilePath ((</>))

import Language.Haskell.TH

import Paths_bittide (getDataFileName)

type Triple = (Integer, Integer, Integer)
type Lines = [String]

-- | Parse a hex string into an integer.
--
-- >>> parseHex "1234"
-- Right 4660
-- >>> parseHex "foobar"
-- Left "parseHex: could not parse: foobar. Got: [(15,\"oobar\")]"
parseHex :: String -> Either String Integer
parseHex s =
  case readHex s of
    [(n, "")] -> Right n
    p -> Left ("parseHex: could not parse: " <> s <> ". Got: " <> show p)

-- | Parse a triple of hex strings into a triple of integers.
--
-- >>> parseTriple "0x0850,0x00"
-- Right (8,80,0)
-- >>> parseTriple "0x0850,,,"
-- Left "Could not parse as triple: 0x0850,,,"
parseTriple :: String -> Either String Triple
parseTriple [_, _, h0, h1, h2, h3, _, _, _, h4, h5] = do
  n0 <- parseHex [h0, h1]
  n1 <- parseHex [h2, h3]
  n2 <- parseHex [h4, h5]
  pure (n0, n1, n2)
parseTriple s = Left ("Could not parse as triple: " <> s)

parseTriples :: Lines -> Either String ([Triple], Lines)
parseTriples [] = Left "parseTriples: unexpected EOF"
parseTriples (l:ls0)
  | "#" `isPrefixOf` l = pure ([], ls0)
  | otherwise = do
    triple <- parseTriple l
    (triples, ls1) <- parseTriples ls0
    pure (triple:triples, ls1)

parseSection :: String -> Lines -> Either String ([Triple], Lines)
parseSection s [] = Left ("parseSection: unexpected EOF for '" <> s <> "'")
parseSection s (l:ls)
  | l == s    = parseTriples ls
  | otherwise = parseSection s ls

parse :: Lines -> Either String ([Triple], [Triple], [Triple])
parse ls0 = do
  (preamble, ls1) <- parseSection "# Start configuration preamble" ls0
  (config, ls2)   <- parseSection "# Start configuration registers" ls1
  (postamble, _)  <- parseSection "# Start configuration postamble" ls2
  pure (preamble, config, postamble)

parseFromFile :: FilePath -> IO (Either String ([Triple], [Triple], [Triple]))
parseFromFile f = parse . map trim . lines <$> readFile f

parseFromFileToRegisterMap :: FilePath -> Q Exp
parseFromFileToRegisterMap fileName = do
  path <- liftIO $ getDataFileName ("data" </> "clock_configs" </> fileName <> ".csv")
  tripleOrError <- liftIO $ parseFromFile path
  case tripleOrError of
    Left err -> fail err
    Right (preamble, config, postamble) ->
      [e| Si539xRegisterMap
            $(listToVecTH preamble)
            $(listToVecTH config)
            $(listToVecTH postamble) |]
