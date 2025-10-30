-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- We're using the SwitchDemo test as a top-level test for the clock control
-- tests and we don't want to repeat the type signatures.
{-# OPTIONS_GHC -Wno-missing-signatures #-}

{- | Test whether clock boards are configurable and transceiver links come
online. If they do, run clock control and wait for the clocks to stabilize.
Also see  'Bittide.Instances.Hitl.Setup'.
-}
module Bittide.Instances.Hitl.SwCcTopologies where

import Clash.Explicit.Prelude

import Bittide.Hitl (
  HitlTestCase (HitlTestCase, name, parameters, postProcData),
  HitlTestGroup (..),
  paramForHwTargets,
 )
import Bittide.Instances.Hitl.Setup (allHwTargets)
import Clash.Annotations.TH (makeTopEntity)
import Protocols.MemoryMap (MemoryMap)
import System.FilePath ((</>))

import qualified Bittide.Instances.Hitl.Driver.SwCcTopologies as D
import qualified Bittide.Instances.Hitl.Dut.SwitchDemo as SwitchDemoDut
import qualified Bittide.Instances.Hitl.SwitchDemo as SwitchDemo

swCcTopologyTest = SwitchDemo.switchDemoTest
{-# OPAQUE swCcTopologyTest #-}
makeTopEntity 'swCcTopologyTest

memoryMap :: MemoryMap
memoryMap = SwitchDemoDut.memoryMapCc

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'swCcTopologyTest
    , targetXdcs =
        [ "switchDemoTest.xdc"
        , "jtag" </> "config.xdc"
        , "jtag" </> "pmod1.xdc"
        , "uart" </> "pmod1.xdc"
        ]
    , externalHdl = []
    , testCases =
        [ HitlTestCase
            { name = "SwCcTopologies"
            , parameters = paramForHwTargets allHwTargets ()
            , postProcData = ()
            }
        ]
    , mDriverProc = Just D.driverFunc
    , mPostProc = Nothing
    }
