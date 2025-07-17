-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Utils.OpenOcd where

import Bittide.Instances.Hitl.Utils.Program
import Project.Handle
import Prelude

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Paths_bittide_instances
import System.Posix.Env (getEnvironment)
import System.Process

getStartPath :: IO FilePath
getStartPath = getDataFileName "data/openocd/start.sh"

-- | Wait until we see "Halting processor", fail if we see an error.
waitForHalt :: String -> Filter
waitForHalt s
  | "Error:" `isPrefixOf` s = Stop (Error ("Found error in OpenOCD output: " <> s))
  | "Halting processor" `isPrefixOf` s = Stop Ok
  | otherwise = Continue

withOpenOcd ::
  (MonadMask m, MonadIO m) =>
  -- | USB device location
  String ->
  -- | GDB port
  Int ->
  -- | TCL port
  Int ->
  -- | Telnet port
  Int ->
  -- | Action to run with OpenOCD
  (ProcessStdIoHandles -> m a) ->
  m a
withOpenOcd = withOpenOcdWithEnv []

{- | Run an action with an openocd process initialzed according to the scripts
located in the data/openocd directory.
-}
withOpenOcdWithEnv ::
  (MonadMask m, MonadIO m) =>
  -- | Extra environment variables to pass to OpenOCD in form (name, value)
  [(String, String)] ->
  -- | USB device location
  String ->
  -- | GDB port
  Int ->
  -- | TCL port
  Int ->
  -- | Telnet port
  Int ->
  -- | Action to run with OpenOCD
  (ProcessStdIoHandles -> m a) ->
  m a
withOpenOcdWithEnv extraEnv usbLoc gdbPort tclPort telnetPort action = do
  (ocd, _handle, clean) <-
    liftIO $ startOpenOcdWithEnv extraEnv usbLoc gdbPort tclPort telnetPort
  finally (action ocd) (liftIO clean)

{- | Starts openocd with the given USB device location and ports.
Sets all handles to line buffering.
-}
startOpenOcdWithEnv ::
  -- | Extra environment variables to pass to OpenOCD in form (name, value)
  [(String, String)] ->
  -- | USB device location
  String ->
  -- | GDB port
  Int ->
  -- | TCL port
  Int ->
  -- | Telnet port
  Int ->
  IO (ProcessStdIoHandles, ProcessHandle, IO ())
startOpenOcdWithEnv extraEnv usbLoc gdbPort tclPort telnetPort =
  startOpenOcdWithEnvAndArgs
    ["-f", "ports.tcl", "-f", "sipeed.tcl", "-f", "vexriscv_init.tcl"]
    ( [ ("USB_DEVICE", usbLoc)
      , ("GDB_PORT", show gdbPort)
      , ("TCL_PORT", show tclPort)
      , ("TELNET_PORT", show telnetPort)
      ]
        <> extraEnv
    )

startOpenOcdWithEnvAndArgs ::
  [String] ->
  [(String, String)] ->
  IO (ProcessStdIoHandles, ProcessHandle, IO ())
startOpenOcdWithEnvAndArgs args extraEnv = do
  startPath <- getStartPath
  currentEnv <- getEnvironment
  let
    openOcdProc =
      (proc startPath args)
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , env = Just (currentEnv <> extraEnv)
        }

  ocdHandles@(openOcdStdin, openOcdStdout, openOcdStderr, openOcdPh) <-
    createProcess openOcdProc

  let
    ocdHandles' =
      ProcessStdIoHandles
        { stdinHandle = fromJust openOcdStdin
        , stdoutHandle = fromJust openOcdStdout
        , stderrHandle = fromJust openOcdStderr
        }

  pure (ocdHandles', openOcdPh, cleanupProcess ocdHandles)
