-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Utils.Picocom where

import Bittide.Instances.Hitl.Utils.Program
import Prelude

import Paths_bittide_instances

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString (ByteString, hGetSome)
import Data.Maybe (fromJust)
import GHC.IO.Exception

-- import GHC.IO.Handle (Handle, BufferMode(..), hSetBuffering)
import System.IO
import System.Posix.Env (getEnvironment)
import System.Process

getStartPath :: IO FilePath
getStartPath = getDataFileName "data/picocom/start.sh"

data StdStreams = StdStreams
  { stdin :: StdStream
  , stdout :: StdStream
  , stderr :: StdStream
  }

defaultStdStreams :: StdStreams
defaultStdStreams = StdStreams CreatePipe CreatePipe CreatePipe

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

startWithChan :: StdStreams -> FilePath -> IO (Chan ByteString, IO ())
startWithChan stdStreams devPath = do
  (pHandles, cleanupHandles) <- start stdStreams devPath
  (chan, cleanupChan) <- handleToChan pHandles.stdoutHandle
  pure (chan, cleanupChan >> cleanupHandles)

handleToChan :: Handle -> IO (Chan ByteString, IO ())
handleToChan h = do
  c <- newChan
  threadId <-
    forkIO $
      (readHandle c) `catch` \(e :: IOException) -> do
        putStrLn $ "[handleToChan: " <> show h <> "] IOException: " <> show e
  let cleanup = killThread threadId
  pure (c, cleanup)
 where
  readHandle chan = do
    hSetBuffering h LineBuffering
    bytes <- hGetSome h 4096
    writeChan chan bytes
    readHandle chan

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

withPicocomWithLoggingAndEnvChan ::
  StdStreams ->
  FilePath ->
  FilePath ->
  FilePath ->
  [(String, String)] ->
  (Chan ByteString -> IO a) ->
  IO a
withPicocomWithLoggingAndEnvChan stdStreams devPath stdoutPath stderrPath extraEnv action = do
  (picoChan, clean) <- startWithLoggingAndEnvChan stdStreams devPath stdoutPath stderrPath extraEnv
  finally (action picoChan) clean

startWithLogging ::
  StdStreams ->
  FilePath ->
  FilePath ->
  FilePath ->
  IO (ProcessHandles, IO ())
startWithLogging stdStreams devPath stdoutPath stderrPath =
  startWithLoggingAndEnv stdStreams devPath stdoutPath stderrPath []

{-
handleToChan :: Handle -> IO (Chan ByteString)
handleToChan h = do
  c <- newChan
  _ <-
    forkIO $
      (readHandle c) `catch` \(e :: IOException) -> do
        putStrLn $ "[handleToChan: " <> show h <> "] IOException: " <> show e
  pure c
 where
  readHandle chan = do
    hSetBuffering h LineBuffering
    bytes <- hGetSome h 4096
    writeChan chan bytes
    readHandle chan
-}

{- | Starts Picocom with the given device path, paths for logging stdout and stderr and
extra environment variables.
-}
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

  putStrLn
    ( "Created picocom with log "
        <> show stdoutPath
        <> " and stdoutHandler "
        <> show picoHandles'.stdoutHandle
    )
    >> hFlush stdout
    >> pure
      (picoHandles', putStrLn "cleaning up picocom called" >> hFlush stdout >> cleanupProcess picoHandles)

startWithLoggingAndEnvChan ::
  StdStreams ->
  FilePath ->
  FilePath ->
  FilePath ->
  [(String, String)] ->
  IO (Chan ByteString, IO ())
startWithLoggingAndEnvChan stdStreams devPath stdoutPath stderrPath extraEnv = do
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

  (chan, chanCleanup) <- handleToChan picoHandles'.stdoutHandle

  putStrLn
    ( "Created picocom with log "
        <> show stdoutPath
        <> " and stdoutHandler "
        <> show picoHandles'.stdoutHandle
    )
    >> hFlush stdout
    >> pure
      ( chan
      , putStrLn "cleaning up picocom called" >> hFlush stdout >> chanCleanup >> cleanupProcess picoHandles
      )
