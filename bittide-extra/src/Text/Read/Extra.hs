module Text.Read.Extra where

import Prelude

import Data.Bifunctor (bimap)
import Text.Read (readEither)

-- | Same as 'readEither', but replace the obnoxious error message from @base@
-- with a sane one.
saneReadEither :: Read a => String -> Either String a
saneReadEither s = bimap saneError id (readEither s)
 where
  saneError err =
    "Could not parse: " <> s <> ". Parser reported: " <> err <> "."

-- | Same as 'read', but replaces the obnoxious error message from @base@ with
-- a sane one.
saneRead :: Read a => String -> a
saneRead = either error id . saneReadEither
