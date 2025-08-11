-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Control.Exception.Extra where

import Prelude

import Control.Monad.Catch (MonadMask (mask), SomeException, onException, try)
import GHC.Stack (HasCallStack)

{- | Like 'Control.Monad.Catch.bracket', but allows the cleanup action to fail
while still propagating any exception from the main action. Sadly, we can't throw
multiple exceptions (Python-like), so we have to pick one.
-}
preferMainBracket ::
  (HasCallStack, MonadMask m) =>
  -- | computation to run first (\"acquire resource\")
  m a ->
  -- | computation to run last (\"release resource\")
  (a -> m b) ->
  -- | computation to run in-between
  (a -> m c) ->
  -- | returns the value from the in-between computation
  m c
preferMainBracket before after thing =
  mask $ \restore -> do
    a <- before
    r <- restore (thing a) `onException` (try @_ @SomeException (after a))
    _ <- after a
    return r

-- | Like 'preferMainBracket', but with a list of actions.
brackets ::
  (HasCallStack, MonadMask m) =>
  -- | Acquisition actions
  [m a] ->
  -- | Release actions
  (a -> m b) ->
  -- | Action to run with the acquired resources
  ([a] -> m c) ->
  -- | Results from actions
  m c
brackets acqs rel act = go [] acqs
 where
  go resL [] = act (reverse resL)
  go resL (acq : acqs1) = preferMainBracket acq rel $ \res -> go (res : resL) acqs1
