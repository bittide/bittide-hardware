-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Bittide.Instances.Hitl.Driver.Transceivers (driver) where

import Clash.Prelude

import Control.Concurrent.Async (forConcurrently_)
import Control.Monad.IO.Class
import Project.Chan
import System.Exit
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM

import Bittide.Hitl
import Bittide.Instances.Hitl.Utils.BootPe (withBootPes)

import qualified System.Timeout.Extra as T

driver ::
  String ->
  [(HwTarget, DeviceInfo)] ->
  VivadoM ExitCode
driver _name targets =
  withBootPes "transceivers-up-test" targets $ \_gdbs serials -> do
    liftIO
      $ T.tryWithTimeout T.PrintActionTime "Waiting for test success" 60_000_000
      $ forConcurrently_ serials
      $ \serial ->
        waitForLine serial "All tests passed"
    pure ExitSuccess
