-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Bittide.Instances.Hitl.Tests
  ( HitlTest(..)
  , hitlTests
  ) where

import Bittide.Simulate.Config (SimConf)
import Bittide.Hitl (HitlTestsWithPostProcData, MayHavePostProcData)
import Clash.Prelude (BitPack, String, FilePath, show)
import Data.Aeson (ToJSON)

import qualified Bittide.Instances.Hitl.BoardTest         as BoardTest
import qualified Bittide.Instances.Hitl.Ethernet          as Ethernet
import qualified Bittide.Instances.Hitl.FincFdec          as FincFdec
import qualified Bittide.Instances.Hitl.FullMeshHwCc      as FullMeshHwCc
import qualified Bittide.Instances.Hitl.FullMeshSwCc      as FullMeshSwCc
import qualified Bittide.Instances.Hitl.HwCcTopologies    as HwCcTopologies
import qualified Bittide.Instances.Hitl.LinkConfiguration as LinkConfiguration
import qualified Bittide.Instances.Hitl.SyncInSyncOut     as SyncInSyncOut
import qualified Bittide.Instances.Hitl.Tcl.ExtraProbes   as ExtraProbes
import qualified Bittide.Instances.Hitl.Transceivers      as Transceivers
import qualified Bittide.Instances.Hitl.VexRiscv          as VexRiscv

-- | Existential wrapper for tests with known Haskell types.
data HitlTest where
  -- | Tests with known Haskell types.
  KnownType ::
    forall a b.
    (BitPack a, ToJSON b, MayHavePostProcData b SimConf) =>
    String ->
    (HitlTestsWithPostProcData a b) ->
    HitlTest
  -- | Load config from 'bittide-instances/data/test_configs'
  LoadConfig ::
    String ->
    FilePath ->
    HitlTest

-- | Available HITL tests.
hitlTests :: [HitlTest]
hitlTests =
  [ -- tests with known Haskell types
    knownType  'BoardTest.boardTestExtended             BoardTest.testsExtended
  , knownType  'BoardTest.boardTestSimple               BoardTest.testsSimple
  , knownType  'FincFdec.fincFdecTests                  FincFdec.tests
  , knownType  'FullMeshHwCc.fullMeshHwCcTest           FullMeshHwCc.tests
  , knownType  'FullMeshHwCc.fullMeshHwCcWithRiscvTest  FullMeshHwCc.tests
  , knownType  'FullMeshSwCc.fullMeshSwCcTest           FullMeshSwCc.tests
  , knownType  'HwCcTopologies.hwCcTopologyTest         HwCcTopologies.tests
  , knownType  'LinkConfiguration.linkConfigurationTest LinkConfiguration.tests
  , knownType  'SyncInSyncOut.syncInSyncOut             SyncInSyncOut.tests
  , knownType  'Transceivers.transceiversUpTest         Transceivers.tests
  , knownType  'VexRiscv.vexRiscvTest                   VexRiscv.tests
  , knownType  'Ethernet.vexRiscvTcpTest                Ethernet.tests
    -- tests that are loaded from config files
  , loadConfig 'ExtraProbes.extraProbesTest             "extraProbesTest.yml"
  ]
 where
  knownType nm = KnownType (show nm)
  loadConfig nm = LoadConfig (show nm)
