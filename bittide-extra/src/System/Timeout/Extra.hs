-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module System.Timeout.Extra where

import Prelude

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)

import qualified Control.Monad.Trans.Control as CMTC
import Data.Time (diffUTCTime, getCurrentTime)
import qualified System.Timeout.Lifted as STL

-- | Controls whether to print the time taken by the action
data PrintActionTime = PrintActionTime | NoPrintActionTime
  deriving (Eq, Show)

{- | A generic wrapper around 'System.Timeout.Lifted.timeout' that 'fail's if the
timeout is hit.
-}
tryWithTimeout ::
  forall m a.
  (HasCallStack, CMTC.MonadBaseControl IO m, MonadFail m, MonadIO m) =>
  -- | Whether to print the time it took to complete the action
  PrintActionTime ->
  -- | Name of the action, used for logging
  String ->
  -- | Timeout duration in microseconds
  Int ->
  -- | Action to perform with a timeout
  m a ->
  -- | Result of the action if it completes in time
  m a
tryWithTimeout logDiff actionName duration = tryWithTimeoutOn logDiff actionName duration (pure ())

{- | A generic wrapper around 'System.Timeout.Lifted.timeout' that 'fail's if the
timeout is hit. This function exists because the 'onException' function does not
work with 'System.Timeout.Lifted.timeout'.
-}
tryWithTimeoutOn ::
  forall m a b.
  (HasCallStack, CMTC.MonadBaseControl IO m, MonadFail m, MonadIO m) =>
  -- | Whether to print the time it took to complete the action
  PrintActionTime ->
  -- | Name of the action, used for logging
  String ->
  -- | Timeout duration in microseconds
  Int ->
  -- | Action to perform if the timeout is hit
  m b ->
  -- | Action to perform with a timeout
  m a ->
  -- | Result of the action if it completes in time
  m a
tryWithTimeoutOn logDiff actionName duration onTimeout action = do
  maybeStart <-
    if logDiff == PrintActionTime then Just <$> liftIO getCurrentTime else pure Nothing
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
      case maybeStart of
        Nothing -> pure ()
        Just start -> liftIO $ do
          end <- getCurrentTime
          let diff = end `diffUTCTime` start
          putStrLn $ "Completed action: " <> actionName <> " in " <> show diff
      pure r

{- | Like 'tryWithTimeout', but takes an additional action to perform regardless
of whether the timeout was hit or not.
-}
tryWithTimeoutFinally ::
  forall m a b.
  (HasCallStack, CMTC.MonadBaseControl IO m, MonadFail m, MonadIO m) =>
  -- | Whether to print the time it took to complete the action
  PrintActionTime ->
  -- | Name of the action, used for logging
  String ->
  -- | Timeout duration in microseconds
  Int ->
  -- | Action to perform if the timeout is hit
  m b ->
  -- | Action to perform with a timeout
  m a ->
  m a
tryWithTimeoutFinally logDiff actionName duration finally action = do
  a <- tryWithTimeoutOn logDiff actionName duration finally action
  void finally
  pure a
