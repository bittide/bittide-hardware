-- SPDX-FileCopyrightText: 2026 QBayLogic
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}

{- | Driver for the Manticore demo.

Milestone-1 placeholder: it asserts the test-start probe and returns success.
The real flow — bring up the management-unit GDB, load the Manticore program
image into gmem over the DMI window, write the schedule command + start,
poll device registers, run the FLUSH/resume loop, and read the trace back —
mirrors the KCU105 @run_manifest.py@ host but over the MU CPU. That belongs to
the milestone-1 verification step and depends on the MU firmware, which is not
in place yet.
-}
module Bittide.Instances.Hitl.ManticoreDemo.Driver where

import Prelude

import Bittide.Hitl (DeviceInfo)
import Bittide.Instances.Hitl.Utils.Driver (assertProbe)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Vector.Internal.Check (HasCallStack)
import System.Exit (ExitCode (ExitSuccess))
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM (VivadoM)

driver ::
  (HasCallStack) =>
  String ->
  [(HwTarget, DeviceInfo)] ->
  VivadoM ExitCode
driver _testName targets = do
  liftIO
    . putStrLn
    $ "Running Manticore demo driver (milestone-1 placeholder) for "
      <> show (length targets)
      <> " target(s)"

  forM_ targets (assertProbe "probe_test_start")

  liftIO $
    putStrLn
      "NOTE: Manticore host flow (image load / run / trace readback) not yet implemented."
  pure ExitSuccess
