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
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO
import System.Posix.Env (getEnvironment)
import System.Process

{-
getStartPath :: IO FilePath
getStartPath = getDataFileName "data/picocom/start.sh"
-}

data StdStreams = StdStreams
  { stdin :: StdStream
  , stdout :: StdStream
  , stderr :: StdStream
  }

defaultStdStreams :: StdStreams
defaultStdStreams = StdStreams CreatePipe CreatePipe CreatePipe

{-
-- | Start picocom with the given device path.
start :: StdStreams -> FilePath -> IO (ProcessHandles, IO ())
start stdStreams devPath = do
  startPath <- getStartPath

  let
    picocomProc =
      (proc startPath [devPath])
        { std_in = stdStreams.stdin
        , std_out = stdStreams.stdout
        , std_err = stdStreams.stderr
        , new_session = True
        }

  picoHandles@(picoStdin, picoStdout, picoStderr, picoPh) <-
    createProcess picocomProc

  let
    picoHandles' =
      ProcessHandles
        { stdinHandle = fromJust picoStdin
        , stdoutHandle = fromJust picoStdout
        , stderrHandle = fromJust picoStderr
        , process = picoPh
        }

  pure (picoHandles', cleanupProcess picoHandles)

{- | Starts Picocom with the given device path and paths for logging stdout and stderr.
Then perform the action and clean up the picocom process.
-}
withPicocomWithLogging ::
  (MonadIO m, MonadMask m) =>
  StdStreams ->
  FilePath ->
  FilePath ->
  FilePath ->
  (ProcessHandles -> m a) ->
  m a
withPicocomWithLogging stdStreams devPath stdoutPath stderrPath action = do
  (pico, clean) <- liftIO $ startWithLogging stdStreams devPath stdoutPath stderrPath
  finally (action pico) (liftIO clean)

{- | Starts Picocom with the given device path, paths for logging stdout and stderr and
extra environment variables. Then perform the action and clean up the picocom process.
-}
withPicocomWithLoggingAndEnv ::
  StdStreams ->
  FilePath ->
  FilePath ->
  FilePath ->
  [(String, String)] ->
  (ProcessHandles -> IO a) ->
  IO a
withPicocomWithLoggingAndEnv stdStreams devPath stdoutPath stderrPath extraEnv action = do
  (pico, clean) <- startWithLoggingAndEnv stdStreams devPath stdoutPath stderrPath extraEnv
  finally (action pico) clean

startWithLogging ::
  StdStreams ->
  FilePath ->
  FilePath ->
  FilePath ->
  IO (ProcessHandles, IO ())
startWithLogging stdStreams devPath stdoutPath stderrPath =
  startWithLoggingAndEnv stdStreams devPath stdoutPath stderrPath []

{- | Starts Picocom with the given device path, paths for logging stdout and stderr and
extra environment variables.
-}
{-
startWithLoggingAndEnv ::
  StdStreams ->
  FilePath ->
  FilePath ->
  FilePath ->
  [(String, String)] ->
  IO (ProcessHandles, IO ())
startWithLoggingAndEnv stdStreams devPath stdoutPath stderrPath extraEnv = do
  startPath <- getStartPath
  currentEnv <- getEnvironment

  let
    picocomProc =
      (proc startPath [devPath])
        { std_in = stdStreams.stdin
        , std_out = stdStreams.stdout
        , std_err = stdStreams.stderr
        , new_session = True
        , env =
            Just
              ( currentEnv
                  <> extraEnv
                  <> [("PICOCOM_STDOUT_LOG", stdoutPath), ("PICOCOM_STDERR_LOG", stderrPath)]
              )
        }

  picoHandles@(picoStdin, picoStdout, picoStderr, picoPh) <-
    createProcess picocomProc

  let
    picoHandles' =
      ProcessHandles
        { stdinHandle = fromJust picoStdin
        , stdoutHandle = fromJust picoStdout
        , stderrHandle = fromJust picoStderr
        , process = picoPh
        }

  pure (picoHandles', cleanupProcess picoHandles)
-}

startWithLoggingAndEnv ::
  StdStreams ->
  FilePath ->
  FilePath ->
  FilePath ->
  [(String, String)] ->
  IO (ProcessHandles, IO ())
startWithLoggingAndEnv _stdStreams devPath stdoutPath stderrPath _extraEnv = startPicocom devPath params
 where
  params = picocomParameters {stdOut = stdoutPath, stdErr = stderrPath}
-}

data PicocomParameters = PicocomParameters
  { stdOut :: FilePath
  , stdErr :: FilePath
  , baudRate :: Integer
  }

parameters =
  PicocomParameters
    { stdOut = "/dev/null"
    , stdErr = "/dev/null"
    , baudRate = 12600
    }

withPicocomWithLogging ::
  (MonadIO m, MonadMask m) =>
  StdStreams ->
  FilePath ->
  PicocomParameters ->
  (ProcessHandles -> m a) ->
  m a
withPicocomWithLogging stdStreams devPath params action = do
  (pico, clean) <- liftIO $ startWithLogging stdStreams devPath params
  finally (action pico) (liftIO clean)

startWithLogging :: StdStreams -> FilePath -> PicocomParameters -> IO (ProcessHandles, IO ())
startWithLogging stdStreams devicePath params = do
  createDirectoryIfMissing True (takeDirectory params.stdOut)
  createDirectoryIfMissing True (takeDirectory params.stdErr)
  logFileH <- openFile params.stdOut WriteMode
  errFileH <- openFile params.stdErr WriteMode

  picocom@(picocomIn, _, _, _) <-
    createProcess
      ( proc
          "picocom"
          [ "--baud"
          , show params.baudRate
          , "--imap"
          , "lfcrlf"
          , "--omap"
          , "lfcrlf"
          , devicePath
          ]
      )
        { std_in = stdStreams.stdin
        , std_out = UseHandle logFileH
        , std_err = UseHandle errFileH
        }

  tail@(tailIn, tailOut, tailErr, tailH) <-
    createProcess
      ( proc
          "tail"
          ["-n", "+1", "-f", params.stdOut]
      )
        { std_out = stdStreams.stdout
        , std_err = stdStreams.stderr
        }

  let
    handles =
      ProcessHandles
        { stdinHandle = fromJust picocomIn
        , stdoutHandle = fromJust tailOut
        , stderrHandle = fromJust tailErr
        , process = tailH
        }

  let cleanup = hClose errFileH >> hCLose logFileH >> cleanupProcess tail >> cleanupProcess picocom
  pure (handles, cleanup)
