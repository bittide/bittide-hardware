-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Control.Concurrent.Async.Extra where

import Prelude

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Tuple.Extra (uncurry3)

{- | Like 'zipWithM', but executes the function concurrently. Is implemented in
terms of 'mapConcurrently'.
-}
zipWithConcurrently :: (MonadIO m) => (a -> b -> IO c) -> [a] -> [b] -> m [c]
zipWithConcurrently f xs ys = liftIO $ mapConcurrently (uncurry f) (zip xs ys)

{- | Like 'zipWithM_', but executes the function concurrently. Is implemented in
terms of 'mapConcurrently'.
-}
zipWithConcurrently_ :: (MonadIO m) => (a -> b -> IO c) -> [a] -> [b] -> m ()
zipWithConcurrently_ f xs ys = liftIO $ void $ mapConcurrently (uncurry f) (zip xs ys)

{- | Like 'zipWithM', but for three arguments and it executes the function
concurrently. Is implemented in terms of 'mapConcurrently'.
-}
zipWithConcurrently3 :: (MonadIO m) => (a -> b -> c -> IO d) -> [a] -> [b] -> [c] -> m [d]
zipWithConcurrently3 f xs ys zs = liftIO $ mapConcurrently (uncurry3 f) (zip3 xs ys zs)

{- | Like 'zipWithM_', but for three arguments and it executes the function
concurrently. Is implemented in terms of 'mapConcurrently'.
-}
zipWithConcurrently3_ :: (MonadIO m) => (a -> b -> c -> IO d) -> [a] -> [b] -> [c] -> m ()
zipWithConcurrently3_ f xs ys zs = liftIO $ void $ mapConcurrently (uncurry3 f) (zip3 xs ys zs)
