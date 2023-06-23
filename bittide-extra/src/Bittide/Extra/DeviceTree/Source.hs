-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{- | Automatic device tree specification retrieval from the project's cabal file.
-}
module Bittide.Extra.DeviceTree.Source
  ( dtsSources
  ) where

import Prelude

import Data.ByteString.Char8 (unpack, pack)
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (find)
import Distribution.Fields (Field(..), Name(..), FieldLine(..), readFields)
import System.Directory (listDirectory)

-- | The path to the DTS sources file listed in the
-- 'extra-source-files'-field of the '.cabal'-file of the project. An
-- error is thrown in case the cabal source is not valid.
dtsSources :: IO [FilePath]
dtsSources = do
  file <- fromMaybe err . find cabalFile <$> listDirectory "."
  extractSources . pack <$> readFile file
 where
  err = error "There is no <pkgname>.cabal package file."
  cabalFile = \case
      (reverse -> 'l':'a':'b':'a':'c':'.':_) -> True
      _ -> False

  extractSources =
    catMaybes . concatMap filterDts . either (error . show) id . readFields

  filterDts = \case
    Field (Name _ name) xs
      | unpack name == "extra-source-files" ->
          map (\(FieldLine _ bs) -> isDts $ unpack bs) xs
      | otherwise -> [Nothing]
    _ -> [Nothing]

  isDts = \case
    name@(reverse -> 's':'t':'d':'.':_) -> Just name
    _ -> Nothing
