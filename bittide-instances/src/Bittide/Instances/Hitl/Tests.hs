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
import qualified Bittide.Instances.Hitl.SwCcTopologies as SwCcTopologies
import qualified Bittide.Instances.Hitl.SwitchDemo as SwitchDemo
import qualified Bittide.Instances.Hitl.SyncInSyncOut as SyncInSyncOut
import qualified Bittide.Instances.Hitl.TemperatureMonitor as TemperatureMonitor
import qualified Bittide.Instances.Hitl.Transceivers as Transceivers
import qualified Bittide.Instances.Hitl.VexRiscv as VexRiscv

hitlTests :: [HitlTestGroup]
hitlTests =
  []
    <> [BoardTest.testSimple]
    <> [BoardTest.testExtended]
    <> [Ddr4.tests]
    <> [DnaOverSerial.tests]
    <> [Ethernet.tests]
    <> [FincFdec.tests]
    <> [LinkConfiguration.tests]
    <> SwCcTopologies.tests
    <> [SwitchDemo.tests]
    <> [SyncInSyncOut.tests]
    <> [TemperatureMonitor.tests]
    <> [Transceivers.tests]
    <> [VexRiscv.tests]
