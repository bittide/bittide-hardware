-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedRecordDot #-}

module Bittide.Instances.Hitl.Utils.Picocom where

import Prelude

import Bittide.Instances.Hitl.Utils.Program
import Paths_bittide_instances

import Control.Monad.Catch
import Control.Monad.IO.Class

getStartPath :: IO FilePath
getStartPath = getDataFileName "data/picocom/start.sh"

data PicocomConfig = PicocomConfig
  { devPath :: FilePath
  , baudRate :: Maybe Integer
  , stdoutPath :: Maybe FilePath
  , stderrPath :: Maybe FilePath
  }

startPicocom :: PicocomConfig -> IO (ProcessStdIoHandles, IO ())
startPicocom picocomConfig = do
  let
    picocomArgs =
      [ "--baud"
      , maybe "921600" show picocomConfig.baudRate
      , "--imap"
      , "lfcrlf"
      , "--omap"
      , "lfcrlf"
      , picocomConfig.devPath
      ]
    singleEnvVar varName varVal = [(varName, varVal)]
    out = maybe [] (singleEnvVar "STDOUT_LOG") picocomConfig.stdoutPath
    err = maybe [] (singleEnvVar "STDERR_LOG") picocomConfig.stderrPath
  (handles, _, cleanup) <- startProgram "picocom" picocomArgs (out <> err)
  pure (handles, cleanup)

withPicocom ::
  (MonadIO m, MonadMask m) =>
  PicocomConfig ->
  (ProcessStdIoHandles -> m a) ->
  m a
withPicocom picocomConfig action = do
  (pico, clean) <- liftIO $ startPicocom picocomConfig
  finally (action pico) (liftIO clean)
