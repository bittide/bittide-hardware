-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Control.Concurrent.Async.Extra where

import Prelude

import Control.Concurrent.Async (mapConcurrently)

{- | Like 'zipWith', but executes the function concurrently. Is implemented in
terms of 'mapConcurrently'.
-}
zipWithConcurrently :: (a -> b -> IO c) -> [a] -> [b] -> IO [c]
zipWithConcurrently f xs ys = mapConcurrently (uncurry f) (zip xs ys)
