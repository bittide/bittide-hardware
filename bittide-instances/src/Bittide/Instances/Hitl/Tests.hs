-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Full definitions of HITL tests. For every test, this includes:

  1. The fully qualified name of the function that is the top-level Clash
     circuit. The test controller will compile, synthesize and implement this
     and program the relevant hardware targets (FPGAs).

  2. The HITL test configuration. See `Bittide.Hitl.HitlTestGroup`.
-}
module Bittide.Instances.Hitl.Tests (
  ClashTargetName,
  HitlTestGroup (..),
  HitlTestCase (..),
  hitlTests,
) where

import Bittide.Hitl (ClashTargetName, HitlTestCase (..), HitlTestGroup (..))
import Prelude

import qualified Bittide.Instances.Hitl.BoardTest as BoardTest
import qualified Bittide.Instances.Hitl.Ddr4 as Ddr4
import qualified Bittide.Instances.Hitl.DnaOverSerial as DnaOverSerial
import qualified Bittide.Instances.Hitl.Ethernet as Ethernet
import qualified Bittide.Instances.Hitl.FincFdec as FincFdec
import qualified Bittide.Instances.Hitl.LinkConfiguration as LinkConfiguration
import qualified Bittide.Instances.Hitl.Si539xConfiguration as Si539xConfiguration
import qualified Bittide.Instances.Hitl.SoftUgnDemo as SoftUgnDemo
import qualified Bittide.Instances.Hitl.SwitchDemo.TopEntity as SwitchDemo
import qualified Bittide.Instances.Hitl.SwitchDemoGppe as SwitchDemoGppe
import qualified Bittide.Instances.Hitl.SyncInSyncOut as SyncInSyncOut
import qualified Bittide.Instances.Hitl.TemperatureMonitor as TemperatureMonitor
import qualified Bittide.Instances.Hitl.Transceivers as Transceivers
import qualified Bittide.Instances.Hitl.VexRiscv as VexRiscv

hitlTests :: [HitlTestGroup]
hitlTests =
  []
    <> [BoardTest.testExtended]
    <> [BoardTest.testSimple]
    <> [Ddr4.tests]
    <> [DnaOverSerial.tests]
    <> [Ethernet.tests]
    <> [FincFdec.tests]
    <> [LinkConfiguration.tests]
    <> [Si539xConfiguration.tests]
    <> [SoftUgnDemo.tests]
    <> [SwitchDemo.tests]
    <> [SwitchDemoGppe.tests]
    <> [SyncInSyncOut.tests]
    <> [TemperatureMonitor.tests]
    <> [Transceivers.tests]
    <> [VexRiscv.tests]
