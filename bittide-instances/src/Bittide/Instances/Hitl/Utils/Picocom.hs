-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Hitl.Utils.Picocom where

import Bittide.Instances.Hitl.Utils.Program
import Prelude

import Paths_bittide_instances

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Maybe (fromJust)
import System.Posix.Env (getEnvironment)
import System.Process

getStartPath :: IO FilePath
getStartPath = getDataFileName "data/picocom/start.sh"

-- | Start picocom with the given device path.
start :: FilePath -> IO (ProcessStdIoHandles, IO ())
start devPath = do
  startPath <- getStartPath

  let
    picocomProc =
      (proc startPath [devPath])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , new_session = True
        }

  picoHandles@(picoStdin, picoStdout, picoStderr, _picoPh) <-
    createProcess picocomProc

  let
    picoHandles' =
      ProcessStdIoHandles
        { stdinHandle = fromJust picoStdin
        , stdoutHandle = fromJust picoStdout
        , stderrHandle = fromJust picoStderr
        }

  pure (picoHandles', cleanupProcess picoHandles)

{- | Starts Picocom with the given device path and paths for logging stdout and stderr.
Then perform the action and clean up the picocom process.
-}
withPicocomWithLogging ::
  (MonadIO m, MonadMask m) =>
  FilePath ->
  FilePath ->
  FilePath ->
  (ProcessStdIoHandles -> m a) ->
  m a
withPicocomWithLogging devPath stdoutPath stderrPath action = do
  (pico, clean) <- liftIO $ startWithLogging devPath stdoutPath stderrPath
  finally (action pico) (liftIO clean)

{- | Starts Picocom with the given device path, paths for logging stdout and stderr and
extra environment variables. Then perform the action and clean up the picocom process.
-}
withPicocomWithLoggingAndEnv ::
  FilePath ->
  FilePath ->
  FilePath ->
  [(String, String)] ->
  (ProcessStdIoHandles -> IO a) ->
  IO a
withPicocomWithLoggingAndEnv devPath stdoutPath stderrPath extraEnv action = do
  (pico, clean) <- startWithLoggingAndEnv devPath stdoutPath stderrPath extraEnv
  finally (action pico) clean

startWithLogging ::
  FilePath -> FilePath -> FilePath -> IO (ProcessStdIoHandles, IO ())
startWithLogging devPath stdoutPath stderrPath =
  startWithLoggingAndEnv devPath stdoutPath stderrPath []

{- | Starts Picocom with the given device path, paths for logging stdout and stderr and
extra environment variables.
-}
startWithLoggingAndEnv ::
  FilePath ->
  FilePath ->
  FilePath ->
  [(String, String)] ->
  IO (ProcessStdIoHandles, IO ())
startWithLoggingAndEnv devPath stdoutPath stderrPath extraEnv = do
  startPath <- getStartPath
  currentEnv <- getEnvironment

  let
    picocomProc =
      (proc startPath [devPath])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , new_session = True
        , env =
            Just
              ( currentEnv
                  <> extraEnv
                  <> [("PICOCOM_STDOUT_LOG", stdoutPath), ("PICOCOM_STDERR_LOG", stderrPath)]
              )
        }

  picoHandles@(picoStdin, picoStdout, picoStderr, _picoPh) <-
    createProcess picocomProc

  let
    picoHandles' =
      ProcessStdIoHandles
        { stdinHandle = fromJust picoStdin
        , stdoutHandle = fromJust picoStdout
        , stderrHandle = fromJust picoStderr
        }

  pure (picoHandles', cleanupProcess picoHandles)
