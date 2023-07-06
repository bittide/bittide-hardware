-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Clash.Sized.Vector.Extra where

import Clash.Explicit.Prelude
import Data.Maybe (fromMaybe)

-- | Finds first element in given vector matching the predicate. Returns
-- 'Nothing' if no element satisfied the predicate.
find :: KnownNat n => (a -> Bool) -> Vec n a -> Maybe a
find f = foldl (<|>) Nothing . map go
 where
  go a
    | f a = Just a
    | otherwise = Nothing

-- | Finds first element in given vector matching the predicate. Returns a
-- default element (the first argument) if no element satisfied the predicate.
findWithDefault :: KnownNat n => a -> (a -> Bool) -> Vec n a -> a
findWithDefault a f = fromMaybe a . find f
