-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Control.Concurrent.Async.Extra where

import Prelude

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (void)
import Data.Tuple.Extra (uncurry3)

{- | Like 'zipWithM', but executes the function concurrently. Is implemented in
terms of 'mapConcurrently'.
-}
zipWithConcurrently :: (a -> b -> IO c) -> [a] -> [b] -> IO [c]
zipWithConcurrently f xs ys = mapConcurrently (uncurry f) (zip xs ys)

{- | Like 'zipWithM_', but executes the function concurrently. Is implemented in
terms of 'mapConcurrently'.
-}
zipWithConcurrently_ :: (a -> b -> IO c) -> [a] -> [b] -> IO ()
zipWithConcurrently_ f xs ys = void $ mapConcurrently (uncurry f) (zip xs ys)

{- | Like 'zipWithM', but for three arguments and it executes the function
concurrently. Is implemented in terms of 'mapConcurrently'.
-}
zipWithConcurrently3 :: (a -> b -> c -> IO d) -> [a] -> [b] -> [c] -> IO [d]
zipWithConcurrently3 f xs ys zs = mapConcurrently (uncurry3 f) (zip3 xs ys zs)

{- | Like 'zipWithM_', but for three arguments and it executes the function
concurrently. Is implemented in terms of 'mapConcurrently'.
-}
zipWithConcurrently3_ :: (a -> b -> c -> IO d) -> [a] -> [b] -> [c] -> IO ()
zipWithConcurrently3_ f xs ys zs = void $ mapConcurrently (uncurry3 f) (zip3 xs ys zs)
