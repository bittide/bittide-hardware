-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Data.List.Extra where

import Data.Maybe

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast (x : xs) = go x xs
 where
  go :: a -> [a] -> Maybe a
  go last [] = Just last
  go _ (y : ys) = go y ys
