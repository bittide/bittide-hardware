-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Clash.Shake.Vivado.Util where

import Prelude

import Data.Bifunctor (first)
import Data.Char (isSpace)

-- | Is the given string empty, or does it only contain whitespace?
isEmptyLine :: String -> Bool
isEmptyLine = all isSpace

-- | The logical proposition that is false when @p@ is true and @q@ is false and
-- true otherwise. I.e.,

-- @
-- | p | q | p ⇒ q |
-- |---|---|-------|
-- | 0 | 0 | 1     |
-- | 0 | 1 | 1     |
-- | 1 | 0 | 0     |
-- | 1 | 1 | 1     |
-- @
--
implies :: Bool -> Bool -> Bool
implies True q = q
implies _p _q  = True

-- | Repeatedly apply a parse function. Useful for parsing /all/ items of a
-- certain form.
parseAll ::
  ([String] -> Maybe (a, [String])) ->
  [String] ->
  [a]
parseAll _     []  = []
parseAll parse ls =
  case parse ls of
    Just (a, rest) -> a : parseAll parse rest
    Nothing -> parseAll parse (tail ls)

-- | Like 'break', but predicate takes all items left in the list, instead of
-- taking a single one.
breakOverMultipleLines :: ([a] -> Bool) -> [a] -> ([a], [a])
breakOverMultipleLines f = go
 where
  go [] = ([], [])
  go as0@(a:as1)
    | f as0      = ([], as0)
    | otherwise  = first (a:) (go as1)
