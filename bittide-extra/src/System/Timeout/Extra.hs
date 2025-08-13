-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module System.Timeout.Extra where

import Prelude

import Control.Monad (void)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)

import qualified Control.Monad.Trans.Control as CMTC
import qualified System.Timeout.Lifted as STL

{- | A generic wrapper around 'System.Timeout.Lifted.timeout' that 'fail's if the
timeout is hit.
-}
tryWithTimeout ::
  forall m a.
  (HasCallStack, CMTC.MonadBaseControl IO m, MonadFail m) =>
  String ->
  Int ->
  m a ->
  m a
tryWithTimeout actionName duration = tryWithTimeoutOn actionName duration (pure ())

{- | A generic wrapper around 'System.Timeout.Lifted.timeout' that 'fail's if the
timeout is hit. This function exists because the 'onException' function does not
work with 'System.Timeout.Lifted.timeout'.
-}
tryWithTimeoutOn ::
  forall m a b.
  (HasCallStack, CMTC.MonadBaseControl IO m, MonadFail m) =>
  String ->
  Int ->
  m b ->
  m a ->
  m a
tryWithTimeoutOn actionName duration onTimeout action = do
  result <- STL.timeout duration action
  case result of
    Nothing -> do
      void onTimeout
      fail $
        "Timeout while performing action: "
          <> actionName
          <> ". Stack:\n\n"
          <> prettyCallStack callStack
    Just r -> do
      pure r

{- | Like 'tryWithTimeout', but takes an additional action to perform regardless
of whether the timeout was hit or not.
-}
tryWithTimeoutFinally ::
  forall m a b.
  (HasCallStack, CMTC.MonadBaseControl IO m, MonadFail m) =>
  String ->
  Int ->
  m b ->
  m a ->
  m a
tryWithTimeoutFinally actionName duration finally action = do
  a <- tryWithTimeoutOn actionName duration finally action
  void finally
  pure a
